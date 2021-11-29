library(here)
library(tidyverse)
library(lubridate)
library(streamMetabolizer)
library(data.table)
source(here("functions", "readMiniDOTData.R"))

startDate <- as_date("2021-08-25")
endDate <- as_date("2021-08-31")

# TODO: get all data from SQL DB instead of local CSVs
DO <- readAllLoggerFiles("data/miniDOT", startDate, endDate)

baro <- read.csv("data/baro_mbar_2021-08-26-2021-09-10.csv") %>%
  mutate(dateTime = as_datetime(Time, tz = "US/Mountain")) %>%
  rename(P_mb = `Absolute.Pressure..mb.`)

DO <- joinBaroData(DO, baro) %>%
  mutate(DO.sat = calc_DO_sat(T_degC, P_mb)) %>%
  mutate(PAR = if_else(hour(dateTime) >= 7 & hour(dateTime) < 19, 1, 0)) %>%
  select(loggerID, dateTime, DO_mgL, T_degC, P_mb, DO.sat, PAR) %>%
  filter(dateTime >= as_datetime("2021-08-26 04:00:00", tz = "US/Mountain"), 
         dateTime < as_datetime("2021-09-01 04:00:00", tz = "US/Mountain")) %>%
  mutate(modelTime = 4 + as.numeric(difftime(dateTime, "2021-08-26 04:00:00", units = "hours")))

write.csv(DO, here("data", "DO.csv"))

DO %>% 
  ggplot(aes(x = dateTime, y = DO_mgL, group = loggerID, color = loggerID)) + 
  geom_line()





