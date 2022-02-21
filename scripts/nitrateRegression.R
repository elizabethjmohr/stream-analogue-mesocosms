library(tidyverse)
library(lubridate)
library(broom)
library(cowplot)

### get data ####
# TODO: get these from the SQL DB
flumeMetadata <- tibble(
  flume = LETTERS[1:15],
  releaseTime = c("8:08:16", "8:32:58", "9:05:53", "9:33:10", "10:13:04",
                  "8:32:30", "8:48:30", "9:23:28", "9:47:30", "10:23:30", 
                  # Note: assumed 30 seconds for times without seconds recorded
                  "8:06:17", "8:31:43", "9:05:14", "9:32:32", "10:10:15"),
  treatment = as_factor(c(100, 850, 850, 2500, 0, 300, 300, 100, 100, 2500, 850, 300, 0, 0, 2500))
) %>%
  mutate(releaseTime = as_datetime(paste("2021-09-17", releaseTime), tz = "US/Mountain"),
         binaryTreatment = as_factor(if_else(treatment == 0, "No Caddisflies", "Caddisflies"))) 

# Read in nitrate data and calculate time since release
nitrate <- read.csv("data/Nitrate_2021-10-02.csv",
                    skip = 3, 
                    header = FALSE, 
                    col.names = c("SampleID", "Date", "Time","Blank" ,"NOx_ugN_L")) %>%
  select(-Blank) %>%
  slice(1:174) %>% 
  as_tibble() %>%
  mutate(Date = paste0(str_sub(Date, start = 1, end = 5), "21")) %>%
  mutate(flume = str_sub(SampleID, start = 12, end = 12), 
         NOx_ugN_L = if_else(NOx_ugN_L== "< 1.5", as.numeric(0), as.numeric(NOx_ugN_L)),
         sampleTime = mdy_hm(paste(Date, Time), tz = "US/Mountain")) %>%
  left_join(flumeMetadata, by = "flume") %>%
  mutate(timeSinceRelease = as.numeric(difftime(sampleTime, releaseTime, units = "hours"))+24) %>%
  filter(str_length(SampleID) < 16) %>%
  group_by(flume) %>%
  arrange(flume, timeSinceRelease) %>%
  mutate(deltaC = (NOx_ugN_L - lag(NOx_ugN_L)/(timeSinceRelease - lag(timeSinceRelease))),
         k = -(1/timeSinceRelease)*log(NOx_ugN_L/lag(NOx_ugN_L))) %>%
  filter(timeSinceRelease > 0.35, 
         timeSinceRelease <20)

# regress first order rate constant and treatment 
lmResult_cat <- lm(k ~ treatment, data = nitrate) # treatment as categorical variable
summary(lmResult_cat)

lmResult_quant <- lm(k ~ as.numeric(treatment), data = nitrate) # treatment as quantitative variable
summary(lmResult_quant)
