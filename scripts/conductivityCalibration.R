library(tidyverse)

# Read in data
tap <- read_csv("./data/2021-10-19_ConductivityCalibration/tap.csv") %>%
  mutate(water = "tap")
asw <- read_csv("./data/2021-10-19_ConductivityCalibration/asw.csv") %>%
  mutate(water = "asw")

# Note: column is called conductivity but the meter actually reports specific conductance

ggplot(asw, aes(x = Volume_added_uL, y = Conductivity_uS_cm, color = water))+ 
  geom_point() + 
  geom_line()+
  ylab("Specific Conductance (uS/cm)")
