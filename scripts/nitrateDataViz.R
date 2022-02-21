library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(viridis)
library(ggbrace)

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
         binaryTreatment = as_factor(if_else(treatment == 0, "No Caddisflies", "Caddisflies"))) %>%
  arrange(treatment) %>%
  add_row(.after=12) %>%
  add_row(.after=13) %>%
  add_row(.after=9) %>%
  add_row(.after=10) %>%
  add_row(.after=6) %>%
  add_row(.after=7) %>%
  add_row(.after=3) %>%
  add_row(.after=4) %>%
  mutate(flumeNumber = as_factor(1:23))

sampleMetadata <- read_excel(path ="/Users/elizabethmohr/Montana State University/Caddisflies & Metabolism - Documents/Data/Metadata_and_MeterData.xlsx",
                             sheet = "Samples") %>%
  mutate(Date = format(Date, format = "%Y-%m-%d"),
         Time = format(Time, format = "%H:%M:%S"),
         dateTime = ymd_hms(paste(Date, Time), tz = "US/Mountain")) %>%
  select(sampleID = SampleID, dateTime)

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
         k = -(1/timeSinceRelease)*log(NOx_ugN_L/lag(NOx_ugN_L)))

#### nitrate vs. time ####
nitrateVsTime <- ggplot(nitrate, aes(x = timeSinceRelease, y = NOx_ugN_L, color = treatment, group = flume)) + 
  geom_point()+
  geom_line()+
  scale_color_viridis(discrete = TRUE, end = 0.9, direction = -1)  + 
  ylab("nitrate concentration (ppb N)")+
  xlab("hours since release")+
  labs(color = "caddisflies per\n square meter")+
  theme_minimal_hgrid()+
  theme(legend.title = element_text(size = 8))

#### k vs. time ####
kVsTime <- ggplot(nitrate %>%
                       filter(timeSinceRelease > 0.35, 
                              timeSinceRelease <20,
                              k >0), aes(x = timeSinceRelease, y = k, color = treatment, group = flume)) + 
  geom_point()+
  geom_line()+
  scale_color_viridis(discrete = TRUE, end = 0.9, direction = -1)  + 
  ylab(bquote(fraction~of~NO[3]^-1~ removed~per~hour))+
  xlab("hours since release")+
  labs(color = "caddisflies per\n square meter")+
  theme_minimal_hgrid()+
  theme(legend.title = element_text(size = 8))

#### k vs. caddis ####
means <- nitrate %>%
  filter(timeSinceRelease > 0.35, timeSinceRelease <20) %>%
  group_by(treatment) %>%
  summarise(meank = mean(k)) %>%
  mutate(flumeNumber = as_factor(seq(2,14, by = 3)))

kVsCaddis <- ggplot(nitrate%>%
                      filter(timeSinceRelease > 0.35, 
                             timeSinceRelease <20,
                             k >0), aes(x = treatment, y = k, color = treatment)) + 
  geom_jitter(size = 2, width = 0.3)+
  geom_segment(data = means, aes(y = meank, yend = meank, x = as.numeric(treatment) - 0.4, xend = as.numeric(treatment) +0.4), size = 1.3, color = "black")+
  scale_color_viridis(discrete = TRUE, end = 0.9, direction = -1) +
  ylab(bquote(fraction~of~NO[3]^-1~ removed~per~hour))+
  xlab(bquote(caddisflies~per~m^2))+
  scale_y_continuous(breaks = seq(0,0.25, 0.05))+
  theme_minimal_hgrid()+
  theme(legend.title = element_blank(),
        legend.position = "none")

#### k vs. flume ####
means <- nitrate %>%
  filter(timeSinceRelease > 0.35, timeSinceRelease <20) %>%
  group_by(treatment) %>%
  summarise(meank = mean(k)) %>%
  mutate(flumeNumber = factor(seq(2,23, by = 5), levels = 1:23))

maxVals <- nitrate %>%
  filter(timeSinceRelease > 0.35, timeSinceRelease <20) %>%
  group_by(flumeNumber) %>%
  summarise(max = max(k, na.rm = TRUE))

metadata <- flumeMetadata %>%
  filter(!is.na(flume)) %>%
  left_join(maxVals)

kVsFlume <- ggplot(nitrate%>%
                  filter(timeSinceRelease > 0.35, 
                         timeSinceRelease <20,
                         k >0), aes(x = as.numeric(flumeNumber), y = k, color = treatment)) + 
  geom_point(size = 2)+
  geom_segment(data = means, aes(y = meank, yend = meank, x =  as.numeric(flumeNumber) - 1.3, xend = as.numeric(flumeNumber) +1.3), size = 1, color = "black")+
  geom_brace(aes(x=c(0.6,3.4), y=c(-0.017,-0.023), label = "0"), inherit.data=F, rotate=180, labelsize=4) +
  geom_brace(aes(x=c(5.6,8.4), y=c(-0.017,-0.023), label = "100"), inherit.data=F, rotate=180, labelsize=4) +
  geom_brace(aes(x=c(10.6,13.4), y=c(-0.017,-0.023), label = "300"), inherit.data=F, rotate=180, labelsize=4) +
  geom_brace(aes(x=c(15.6,18.4), y=c(-0.017,-0.023), label = "85"), inherit.data=F, rotate=180, labelsize=4) +
  geom_brace(aes(x=c(20.6,23.4), y=c(-0.017,-0.023), label = "2500"), inherit.data=F, rotate=180, labelsize=4) +
  annotate("text", x = 12, y = -0.05,label = bquote(caddisflies~per~m^2), size = 5)+
  annotate("text", x = -2.5, y = -0.023, label = "Flume")+
  geom_segment(x = -2.2, y = -0.019, xend = 0, yend = -0.011, arrow = arrow(length = unit(0.2, "cm")), color = "black")+
  coord_cartesian(ylim = c(0, 0.28), xlim = c(0, 23),clip = "off", expand = 0)+
  scale_color_viridis(discrete = TRUE, end = 0.9, direction = -1) +
  ylab(bquote(fraction~of~NO[3]^-1~ removed~per~hour))+
  scale_y_continuous(breaks = seq(0,0.25, 0.05))+
  scale_x_continuous(breaks = c(1:3, 6:8, 11:13, 16:18, 21:23), labels = metadata$flume)+
  theme_minimal_hgrid()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = unit(c(1,1,3,1), "lines"))

#### Save Plots ####
height= 5
width = 7
ggsave(filename = "plots/nitrateVsTime.png", nitrateVsTime, height = height, width = width, units = "in")
ggsave(filename = "plots/kVsTime.png", kVsTime, height = height, width = width, units = "in")
ggsave(filename = "plots/kVsCaddis.png", kVsCaddis, height = height, width = width, units = "in")
ggsave(filename = "plots/kVsFlume.png", kVsFlume, height = height, width = width, units = "in")
