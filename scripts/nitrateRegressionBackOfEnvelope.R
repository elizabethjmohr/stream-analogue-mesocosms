library(tidyverse)
library(lubridate)
library(broom)

# Manually enter release times
# TODO: get these from the SQL DB
flumeMetadata <- tibble(
  flume = LETTERS[1:15],
  releaseTime = c("8:08:16", "8:32:58", "9:05:53", "9:33:10", "10:13:04",
                  "8:32:30", "8:48:30", "9:23:28", "9:47:30", "10:23:30", 
                  #Note: assumed 30 seconds for times without seconds recorded
                  "8:06:17", "8:31:43", "9:05:14", "9:32:32", "10:10:15"),
  treatment = as_factor(c(100, 850, 850, 2500, 0, 300, 300, 100, 100, 2500, 850, 300, 0, 0, 2500))
) %>%
  mutate(releaseTime = as_datetime(paste("2021-09-16", releaseTime), tz = "US/Mountain"))

# Read in nitrate data and calculate time since release
# TODO: get these from the SQL DB
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
  mutate(timeSinceRelease = difftime(sampleTime, releaseTime, units = "hours"))

# Regression (back of R envelope)
slopes <- nitrate %>% 
  mutate(timeSinceRelease = as.numeric(timeSinceRelease)) %>%
  filter(timeSinceRelease > 0, timeSinceRelease <5) %>%
  group_by(flume) %>%
  group_modify(~ broom::tidy(lm(NOx_ugN_L~timeSinceRelease, data = .x))) %>%
  filter(term == "timeSinceRelease") %>%
  left_join(flumeMetadata, by = "flume") 

ggplot(slopes, aes(x = treatment, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) + 
  xlab("Treatment (caddisflies per square meter)") + 
  ylab("Slope Estimate")

lmOut <- lm(estimate ~ treatment, data = slopes)
summary(lmOut)

lmOut2 <- lm(estimate~as.numeric(treatment), data = slopes)
summary(lmOut2)
