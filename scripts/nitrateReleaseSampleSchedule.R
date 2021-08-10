library(tidyverse)
library(lubridate)

####  N14-NO3 release  ####

# Specify when the experiment will begin
experimentStartTime <- ymd_hms(ymd_hms("2021-04-07 07:00:00"),
                               tz = "America/Denver")

# Specify times at which each flume will be sampled following the nitrate release
timepoints <- c(3,10,20,40,80,160,240,360,480) %>%
  paste0(":0") %>%
  ms(roll = TRUE)
timepoints <- c(ms(paste0(-4, ":0")), timepoints)

# Specify times at which nitrate will be released
releaseTimes <- tibble(flume = c("1","2","3"), 
                       releaseTime = hm("0:6", "0:35", "1:4"))

# Determine times at which each flume should be sampled 
times <- tibble(flume = rep(c("1","2","3"), each = length(timepoints)),
                timepoint = rep(timepoints, times = 3)) %>%
  left_join(releaseTimes, by = c("flume")) %>%
  mutate(samplingTimes = releaseTime + timepoint) %>%
  # Add three blank samples
  add_row(flume = "Blank1", timepoint = NA, releaseTime = NA, samplingTimes = hm("5:09", roll = TRUE))%>%
  add_row(flume = "Blank2", timepoint = NA, releaseTime = NA, samplingTimes = hm("5:19", roll = TRUE))%>%
  add_row(flume = "Blank3", timepoint = NA, releaseTime = NA, samplingTimes = hm("5:29", roll = TRUE))%>%
  mutate(samplingDateTime = samplingTimes + experimentStartTime) %>%
  mutate(duplicate = FALSE)

# Add duplicate samples
duplicates <- tibble(
  flume = c(1,1,1,2,2,2,3,3,3,"Blank1", "Blank2"),
  timepoint = c((c(10,80,160,80,160,240,40,360,480) %>% paste0(":0") %>% ms(roll = TRUE)), NA, NA)
) %>%
  left_join(times, by = c("flume", "timepoint")) %>%
  mutate(duplicate = TRUE, 
         samplingTimes = samplingTimes + hm("0:05"),
         samplingDateTime = samplingDateTime + hm("0:05"))

times <- times %>%
  bind_rows(duplicates) %>%
  mutate(timepoint = timepoint %>% as.numeric() %>% `/`(60)) %>%
  arrange(samplingDateTime)

# Format table for Latex document
times %>%
  select(-releaseTime) %>%
  write.table("tables/sampleTimesN14.txt", quote=FALSE, eol="\\\\\n", sep=" & ", row.names = FALSE)

####  N15-NO3 release ####
# Table of how estimated time for each sampling "task"
# Specify when the experiment will begin
experimentStartTime <- ymd_hms(ymd_hms("2021-08-17 07:00:00"),
                               tz = "America/Denver")

# Specify times at which nitrate will be released
releaseTimes <- tibble(flume = c("1","2","3","4","5"), 
                       releaseTime = hm("0:0", "0:25", "0:50", "1:15", "1:40"))

# Specify times at which each flume will be sampled following the nitrate release
timepoints <- timepoints <- c(5, 15, 30, 60, 120, 180, 240, 360, 480, 600) %>%
  paste0(":0") %>%
  ms(roll = TRUE)
timepoints <- c(ms(paste0(-4, ":0")), timepoints)

# Determine times at which each flume should be sampled 
times <- tibble(flume = rep( c("1","2","3", "4", "5"), each = length(timepoints)),
                timepoint = rep(timepoints, times = 3)) %>%
  left_join(releaseTimes, by = c("flume")) %>%
  mutate(samplingTimes = releaseTime + timepoint) %>%
  mutate(samplingDateTime = samplingTimes + experimentStartTime) %>%
  mutate(duplicate = FALSE)
