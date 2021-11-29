library(here)
library(tidyverse)
library(lubridate)
library(streamMetabolizer)
library(npreg)
library(ungeviz)
library(data.table)
source(here("functions", "readMiniDOTData.R"))
source(here("functions", "nighttimeRegression.R"))

startDate <- as_date("2021-08-25")
endDate <- as_date("2021-09-09")

# TODO: get all data from DB
DO <- readAllLoggerFiles("data/miniDOT", startDate, endDate)

baro <- read.csv("data/baro_mbar_2021-08-26-2021-09-10.csv") %>%
  mutate(dateTime = as_datetime(Time, tz = "US/Mountain")) %>%
  rename(P_mb = `Absolute.Pressure..mb.`)

treatments <- tibble(
  treatment = as_factor(rep(c(0,100,300,850,2500), each = 3)), 
  SAM = c("E","M","N","A","H","I","F","G","L","B","C","K","D","J","O"),
  loggerID = as.character(c(493593, 688268, 432269, 648356, 739125, 567371, 497232, 415148,702726,488982,701237,467606,736634,728987,511686))
)

DO <- joinBaroData(DO, baro) %>%
  mutate(DO.sat = calc_DO_sat(T_degC, P_mb)) %>%
  mutate(PAR = if_else(hour(dateTime) >= 7 & hour(dateTime) < 19, 1, 0)) %>%
  select(loggerID, dateTime, DO_mgL, T_degC, P_mb, DO.sat, PAR)

regressionOutput <- DO %>%
  group_by(loggerID) %>%
  nest() %>%
  mutate(regressionEsts = map(data, nighttimeRegression, nightStart = 19, nightEnd = 7)) %>%
  select(-data) %>%
  unnest(cols = regressionEsts) %>%
  left_join(treatments, by = "loggerID") %>%
  filter(!loggerID == "921379")

# Plot one night, all loggers
ggplot(regressionOutput %>% filter(nightOf == as.Date("2021-08-30", tz = "US/Mountain")), aes(x = interceptEstimate, y = loggerID, moe = interceptStdError)) +
  stat_confidence_density(fill = "lightblue", height = 0.8, confidence = 0.68) + 
  geom_point() + 
  xlab("Ecosystem Respiration (mg/(L*hr))") +
  ylab("Logger")+
  geom_errorbar(aes(xmin = interceptEstimate - interceptStdError, xmax =interceptEstimate + interceptStdError))+
  theme_minimal()

# Plot all nights, all loggers
ggplot(regressionOutput, 
       aes(x = interceptEstimate, y = loggerID, moe = interceptStdError)) +
  stat_confidence_density(fill = "lightblue", height = 0.8, confidence = 0.68) + 
  geom_point() + 
  facet_wrap(vars(nightOf))+
  xlab("Ecosystem Respiration (mg/(L*hr))") +
  ylab("Logger")+
  geom_errorbar(aes(xmin = interceptEstimate - interceptStdError, xmax =interceptEstimate + interceptStdError))+
  theme_minimal()

ggplot(regressionOutput, 
       aes(x = slopeEstimate, y = loggerID, moe = slopeStdError)) +
  stat_confidence_density(fill = "lightblue", height = 0.8, confidence = 0.68) + 
  geom_point() + 
  facet_wrap(vars(nightOf))+
  xlab("k (1/hr))") +
  ylab("Logger")+
  geom_errorbar(aes(xmin = slopeEstimate - slopeStdError, xmax = slopeEstimate + slopeStdError))+
  theme_minimal()

# Plot a single logger
logger = 511686
ggplot(DO %>% filter(loggerID == logger), aes(x = dateTime, y = DO_mgL, color = loggerID)) + 
  geom_point()

regressionOutput %>% 
  filter(loggerID == logger) %>%
  ggplot(aes(x = nightOf, y = interceptEstimate))+
  geom_point() + 
  geom_errorbar(aes(ymin = interceptEstimate - interceptStdError, ymax =interceptEstimate + interceptStdError))

regressionOutput %>% 
  filter(loggerID == logger) %>%
  ggplot(aes(x = nightOf, y = slopeEstimate))+
  geom_point() + 
  geom_errorbar(aes(ymin = slopeEstimate - slopeStdError, ymax = slopeEstimate + slopeStdError))

# Plot panel grouped by treatment
interceptsByTreatment <- regressionOutput %>%
  group_by(treatment, nightOf) %>%
  summarise(mean = mean(interceptEstimate), 
            sd = sqrt(sum(interceptStdError^2)))
ggplot(interceptsByTreatment, 
       aes(x = nightOf, y = mean, color = treatment)) +
  geom_point() + 
  ylab("Ecosystem Respiration (mg/(L*hr))") +
  xlab("Date")+
  theme_minimal()+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd))

# Try fitting a linear mixed effects model
library(nlme)
lme <- lme(interceptEstimate ~ nightOf+treatment, random= ~1|loggerID, data=regressionOutput)
summary(lme)

