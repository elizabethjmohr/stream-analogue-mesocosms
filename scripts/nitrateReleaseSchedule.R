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
  write.table("schedules/N14NitrateSamples.txt", quote=FALSE, eol="\\\\\n", sep=" & ", row.names = FALSE)

####  N15-NO3 release ####

# Specify flumes assigned to each of three teams
flumesList = list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15])

#Specify tasks for each of two team members
tasks = c("Gas Samples", "Nitrate Samples")

# Generate schedule tables for each team and each task
for (i in 1:3) {
  for (j in tasks){
    rmarkdown::render("./scripts/nitrateReleaseSchedule.Rmd", 
                      params = list(flumes = flumesList[[i]], sampleType = j),
                      output_file=paste0(paste0(flumesList[[i]], collapse = ""),
                                         str_replace_all(j, " ", "")),
                      output_dir = "schedules")
  }
  
}

  
  