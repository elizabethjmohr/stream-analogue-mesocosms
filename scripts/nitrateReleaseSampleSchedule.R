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
# Specify flume names
flumes <- LETTERS[1:5]

# Specify when the experiment will begin
experimentStartTime <- ymd_hms(ymd_hms("2021-09-17 07:20:00"),
                               tz = "America/Denver")

# Specify times at which nitrate will be released
releaseTimes <- tibble(flume = flumes, 
                       releaseTime = experimentStartTime + minutes(c(10,35,70,95,135)))
tibble(Flume = LETTERS[1:15],
       releaseTime = rep(releaseTimes$releaseTime, times = 3)) %>%
  write.table("releaseTimes.txt", quote=FALSE, eol="\\\\\n", sep=" & ", row.names = FALSE)


# Specify times at which each flume will be sampled following the nitrate release
timepoints <- c(-10,10,40,85,140,200,280,360,475,600)

# Determine times at which each flume should be sampled 
times <- tibble(flume = rep( flumes, each = length(timepoints)),
                timepoint = rep(timepoints, times = nrow(releaseTimes))) %>%
  left_join(releaseTimes, by = c("flume")) %>%
  mutate("Gas Sample" = releaseTime + minutes(timepoint),
         "Nitrate Samples" = releaseTime + minutes(timepoint) + minutes(5)) %>% 
  pivot_longer(cols = c("Gas Sample", "Nitrate Samples"), names_to = "Task", values_to = "Start") %>% 
  mutate(End = Start + minutes(5)) %>%
  mutate(low = match(flume, flumes) - 0.5, 
         high = match(flume, flumes) + 0.5) 

times[which(times$low == 1.5 & times$timepoint == 140), ]$Start <- times[which(times$low == 1.5 & times$timepoint == 140),]$Start - minutes(c(5,5))
times[which(times$low == 0.5 & times$timepoint == 200), ]$Start <- times[which(times$low == 0.5 & times$timepoint == 200),]$Start - minutes(c(5,5))
times[which(times$low == 1.5 & times$timepoint == 200), ]$Start <- times[which(times$low == 1.5 & times$timepoint == 200),]$Start - minutes(c(5,5))
times[which(times$low == 0.5 & times$timepoint == 600), ]$Start <- times[which(times$low == 0.5 & times$timepoint == 600),]$Start - minutes(c(5,5))
times[which(times$low == 1.5 & times$timepoint == 140), ]$End <- times[which(times$low == 1.5 & times$timepoint == 140),]$End - minutes(c(5,5))
times[which(times$low == 0.5 & times$timepoint == 200), ]$End <- times[which(times$low == 0.5 & times$timepoint == 200),]$End - minutes(c(5,5))
times[which(times$low == 1.5 & times$timepoint == 200), ]$End <- times[which(times$low == 1.5 & times$timepoint == 200),]$End - minutes(c(5,5))
times[which(times$low == 0.5 & times$timepoint == 600), ]$End <- times[which(times$low == 0.5 & times$timepoint == 600),]$End - minutes(c(5,5))

ggplot(times, aes(ymin = low, ymax = high,
                  xmin = Start, xmax = End)) + 
  geom_rect(aes(fill = Task)) + 
  theme_minimal() 

ggplot(times %>% filter(timepoint %in% timepoints[1:5]), aes(ymin =low, ymax = high,
                  xmin = Start, xmax = End)) + 
  geom_rect(aes(fill = Task)) + 
  theme_minimal() 

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2")

ggplot(times %>% filter(Task == "Gas Sample"), aes(ymin = 0, ymax = 1,xmin = Start, xmax = End)) + 
  geom_rect(aes(fill = flume)) + 
  scale_x_datetime(breaks = scales::breaks_pretty(11))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=cbbPalette)

ggplot(times %>% filter(Task == "Nitrate Samples"), aes(xmin = 0, xmax = 1,ymin = Start, ymax = End)) + 
  geom_rect(aes(fill = flume)) + 
  theme_minimal() +
  scale_fill_manual(values=cbbPalette)

ggplot(times %>% filter(Task == "Gas Sample", timepoint %in% timepoints[1:5]), aes(xmin = 0, xmax = 1,ymin = Start, ymax = End)) + 
  geom_rect(aes(fill = flume)) + 
  theme_minimal() +
  scale_fill_manual(values=cbbPalette)

ggplot(times %>% filter(Task == "Nitrate Samples", timepoint %in% timepoints[1:5]), aes(xmin = 0, xmax = 1,ymin = Start, ymax = End)) + 
  geom_rect(aes(fill = flume)) + 
  theme_minimal() +
  scale_fill_manual(values=cbbPalette)

times %>%
  filter(Task == "Gas Sample") %>%
  mutate(sampleID = paste0("CAD_210917_", flume, (match(timepoint, timepoints))-1, "g")) %>%
  select(flume, timepoint, Start, End, sampleID) %>%
  arrange(Start) %>%
  write.csv("Gas Samples A-E.csv", 
            row.names = FALSE)

flumesList = list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15])
tasks = c("Gas Samples", "Nitrate Samples")
for (i in 1:3) {
  for (j in tasks){
    rmarkdown::render("./scripts/scheduleTable.Rmd", 
                      params = list(flumes = flumesList[[i]], sampleType = j),
                      output_file=paste0(paste0(flumesList[[i]], collapse = ""),
                                         str_replace_all(j, " ", ""),
                                         "_SampleSchedule.html"),
                      output_dir = "schedules")
  }
  
}

  
  