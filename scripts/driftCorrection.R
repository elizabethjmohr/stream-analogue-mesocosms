library(tidyverse)
library(lubridate)
library(driftR)

meterVals <- c(195.8, 270.5)

A <- dr_read(file = "./data/A_10104871.csv", 
             instrument = "HOBO") %>%
  mutate(dt = mdy_hms(paste(date,time), tz = "US/Mountain")) %>%
  filter(dt >= mdy_hms("09/17/2021 06:28:40", tz = "US/Mountain"),
         dt <= mdy_hms("09/18/2021 10:30:10", tz = "US/Mountain"),
         !is.na(low_range)) %>%
  dr_factor(corrFactor = corfac, 
            dateVar = date, 
            timeVar = time, 
            keepDateTime = TRUE) %>%
  mutate(correctedCond = low_range + 
           (1-corfac)*(meterVals[1]-low_range[1]) + 
           corfac*(meterVals[2] - low_range[16123]))

ggplot(A, aes(x = dt, y = low_range)) + 
  geom_point(size = 0.5) +
  geom_point(aes(y = correctedCond), color = "blue", size = 0.5)
  
