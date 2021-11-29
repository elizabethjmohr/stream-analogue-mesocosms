library(tidyverse)
library(lubridate)

nitrate <- read_csv("./data/Nitrate_2021-05-04.csv", 
           skip = 3,
           col_names = c("sampleID", "Date", "Time", 
                         "Blank1", "NO3_ppbN", "Blank2", 
                         "Blank3", "Blank4")) %>%
  select(c(1:3, 5)) %>%
  filter(!is.na(Date)) %>%
  mutate(dateTime = mdy_hm(paste(Date, Time), tz = "US/Mountain")) %>%
  filter(date(dateTime) == as_date("2021-03-23")) %>%
  mutate(treatment= case_when(str_sub(sampleID, 1,1) == "T" ~ "T", 
                              str_sub(sampleID, 1,1) == "C" ~ "C",
                              str_sub(sampleID, 1,1) == "A" ~ "ASW",
                              TRUE ~ "Blank")) %>%
  mutate(timepoint = if_else(treatment %in% c("T", "C"), 
                             str_sub(sampleID, 3, 4),
                             "NA_character_")) %>%
  mutate(timepoint = if_else(str_length(timepoint) == 1, paste0("0", timepoint), timepoint)) %>%
  mutate(sampleID2 = paste0("CAD_210323_", treatment, timepoint)) %>%
  mutate(sampleID2 = case_when(treatment == "ASW" ~ "CAD_210323_ASW",
                              treatment == "Blank" ~ "CAD_210323_BLA",
                              TRUE ~ sampleID2)) %>%
  mutate("NO3Concentration" = paste0(NO3_ppbN," ug/L NO3-N"))

write.csv(nitrate, file = "./sampleIDs/2021-03-23_Nutrient_Isotope_SampleIDs.csv", row.names = FALSE)

ggplot(nitrate, 
       aes(x = dateTime, y = NO3_ppbN, color = treatment)) + 
  geom_point(size = 3) + 
  theme_bw() + 
  xlab("Time") + 
  ylab("Nitrate Concentration (ppb NO3-N)")+
  labs(color = "Legend")

NGas <- read_csv("./data/NGasIsotopes_2021-07-09/Samples-Table 1.csv") %>%
  select(c(2,5,6,8))

names(NGas) <- c("sampleID", "delta15N-N2_permil", "15N-N2_at%", "N2_ppm")

NGas <- NGas %>%
  left_join(nitrate, by = "sampleID")

# N2 Concentration
ggplot(NGas, 
       aes(x = dateTime, y = N2_ppm, color = treatment)) + 
  geom_point(size = 3) + 
  theme_bw() + 
  xlab("Time") + 
  ylab("N2 Concentration (ppm)")+
  labs(color = "Legend")

# N2 Isotopes
ggplot(NGas, 
       aes(x = dateTime, y = `15N-N2_at%`, color = treatment)) + 
  geom_point(size = 3) + 
  theme_bw() + 
  xlab("Time") + 
  ylab("N15-N2 (atom %)")+
  labs(color = "Legend")

