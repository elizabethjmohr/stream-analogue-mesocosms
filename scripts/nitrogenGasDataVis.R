library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(viridis)
library(ggforce)

### get data ####

# TODO: get these from the SQL DB
flumeMetadata <- tibble(
  flume = LETTERS[1:15],
  releaseTime = c("8:08:16", "8:32:58", "9:05:53", "9:33:10", "10:13:04",
                  "8:32:30", "8:48:30", "9:23:28", "9:47:30", "10:23:30", 
                  #Note: assumed 30 seconds for times without seconds recorded
                  "8:06:17", "8:31:43", "9:05:14", "9:32:32", "10:10:15"),
  treatment = as_factor(c(100, 850, 850, 2500, 0, 300, 300, 100, 100, 2500, 850, 300, 0, 0, 2500))
) %>%
  mutate(releaseTime = as_datetime(paste("2021-09-17", releaseTime), tz = "US/Mountain"),
         binaryTreatment = as_factor(if_else(treatment == 0, "No Caddisflies", "Caddisflies"))) %>%
  arrange(treatment) 

sampleMetadata <- read_excel(path ="/Users/elizabethmohr/Montana State University/Caddisflies & Metabolism - Documents/Data/Metadata_and_MeterData.xlsx",
                             sheet = "Samples") %>%
  mutate(Date = format(Date, format = "%Y-%m-%d"),
         Time = format(Time, format = "%H:%M:%S"),
         dateTime = ymd_hms(paste(Date, Time), tz = "US/Mountain")) %>%
  select(sampleID = SampleID, dateTime)

nGas <- read_xlsx(path = "data/2022-02-07_SIF_NGasIsotopes.xlsx",
                  sheet = "Samples") %>%
  mutate(flume = str_sub(`Sample ID`, start = 12, end = 12),
         timepoint = str_sub(`Sample ID`, start = 13, end = 13),
         N15N2 = (`15N - N2 (at-%)`*`N2 (ppm)`)) %>%
  filter(timepoint %in% as.character(0:9)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  left_join(flumeMetadata, by = "flume") %>%
  left_join(sampleMetadata, by = c("Sample ID" = "sampleID")) %>%
  mutate(timeSinceRelease = difftime(dateTime, releaseTime, units = "hours"))

#### N15-N2 vs. time ####

N15_N2VsTime <- ggplot(nGas, aes(x = timeSinceRelease, y = N15N2, color = treatment, group = flume)) + 
  geom_point()+ 
  scale_color_viridis(discrete = TRUE,
                      end = 0.85) + 
  ylab("15N-N2 (ppm)")+
  xlab("Hours since release")+
  theme_minimal_grid()

#### atom percent vs. time ####
atomPercentVsTime <- ggplot(nGas, aes(x = timeSinceRelease, y = `15N - N2 (at-%)`, color = treatment, group = flume)) + 
  geom_point()+ 
  scale_color_viridis(discrete = TRUE,
                      end = 0.85) + 
  ylab("15N-N2/14N-N2 (at-%)")+
  xlab("Hours since release")+
  theme_minimal_grid()

#### N2 vs. time ####

N2VsTime <- ggplot(nGas, aes(x = timeSinceRelease, y = `N2 (ppm)`, color = treatment, group = flume)) + 
  geom_point()+ 
  scale_color_viridis(discrete = TRUE,
                      end = 0.85) + 
  xlab("Hours since release")+
  ylab("N2 (ppm)")+
  theme(legend.title = element_blank())+
  theme_minimal_grid()

# color points by "position" in rack
N2VsTimeAndPosition <- ggplot(nGas, aes(x = timeSinceRelease, y = `N2 (ppm)`, color =Position, group = flume)) + 
  geom_point()+ 
  scale_color_viridis(end = 0.85) + 
  xlab("Hours since NO3 release")+
  ylab("N2 (ppm)")+
  geom_ellipse(aes(x0 = 3.3, y0 = 23100, a = 0.5, b = 800, angle = 0), color = "black")+
  geom_ellipse(aes(x0 = 3.3, y0 = 19200, a = 0.5, b = 1000, angle = 0), color = "black")+
  geom_ellipse(aes(x0 = 7.9, y0 = 15500, a = 0.5, b = 1800, angle = 0), color = "black")+
  geom_ellipse(aes(x0 = 7.9, y0 = 20000, a = 0.6, b = 2000, angle = 0), color = "black")+
  theme_minimal_grid()

