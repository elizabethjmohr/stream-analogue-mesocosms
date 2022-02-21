library(tidyverse)
library(lubridate)
library(data.table)

#### Nutrients ####
nutrientSampleIDs <- tibble(
  flume= LETTERS[1:15], 
  timepoint = 0:14) %>%
  expand(flume, timepoint) %>%
  filter(timepoint < 11) %>%
  mutate(duplicate = FALSE) %>%
  add_row(flume = c("A", "F", "K", "B", "G", "L", "D", "I", "N"),
          timepoint = rep(c(0,8,3), each = 3), 
          duplicate = TRUE) %>% # duplicates
  add_row(flume = rep("BL", times = 3),
          timepoint = 1:3,
          duplicate = FALSE) %>%
  add_row(flume = rep("SL", times = 5),
          timepoint = 1:5,
          duplicate = FALSE) %>%
  mutate(date = if_else(timepoint<10, as_date("2021-09-17"), as_date("2021-09-18"))) %>%
  mutate(dateChar = if_else(timepoint<10, "210917", "210918")) %>%
  mutate(sampleID = paste0("CAD_", dateChar,"_", flume, timepoint, "c", if_else(duplicate, "_d", "")),
         location = paste0("Flume ", flume)) %>%
  select(date, location, flume, timepoint, sampleID) %>%
  arrange(flume, timepoint)

write.csv(nutrientSampleIDs, file = "./sampleIDs/2021-09-17_Nutrient_SampleIDs.csv", row.names = FALSE)

inOutSampleIDs <- tibble(
  date = rep(seq(from = as_date("2021-08-28"), to = as_date("2021-09-17"), by = "1 day"), each = 2),
  location = rep(c("IN", "OUT"), times = length(date)/2)) %>%
  mutate(timeOfDay = rep(c("AM", "PM"), each = 2, length.out = length(date))) %>%
  mutate(timeOfDay = if_else(date == as_date("2021-09-14"), "AM", timeOfDay)) %>%
  add_row(date = rep(as_date(c("2021-09-08", "2021-09-15")), each = 2), 
          location = rep(c("IN", "OUT"), times = 2),
          timeOfDay = rep(c("AM", "PM"), each = 2)) %>%
  mutate(year = year(date) %>% str_sub(3,4), 
         month = month(date), 
         day = day(date)) %>%
  mutate(dateChar = paste0(year, "0", month, if_else(day <10, paste0("0", day), as.character(day)))) %>%
  mutate(sampleID = paste0("CAD_", dateChar,"_", location, "_", timeOfDay)) %>%
  mutate(location = if_else(location == "IN", "Inflow", "Outflow Reservoir")) %>%
  select(date, location, sampleID) %>%
  arrange(date)

write.csv(inOutSampleIDs, file = "./sampleIDs/2021-09_InOut_SampleIDs.csv", row.names = FALSE)

synopticSampleIDs <- tibble(
  date = rep(as_date(c("2021-09-03", "2021-09-08", "2021-09-14")), each = 15),
  flume = rep(LETTERS[1:15], times = 3)) %>%
  mutate(year = year(date) %>% str_sub(3,4), 
         month = month(date), 
         day = day(date)) %>%
  mutate(dateChar = paste0(year, "0", month, if_else(day <10, paste0("0", day), as.character(day)))) %>%
  mutate(sampleID = paste0("CAD_", dateChar,"_", flume), 
         location = paste0("Flume ", flume)) %>%
  select(date, location, flume, sampleID) %>%
  arrange(date)

write.csv(synopticSampleIDs, file = "./sampleIDs/2021-09_Synoptic_SampleIDs.csv", row.names = FALSE)

#### Nutrient Isotopes ####
nitrateData <- read.csv("data/Nitrate_2021-10-02.csv",
                    skip = 3, 
                    header = FALSE, 
                    col.names = c("sampleID", "Date", "Time","Blank" ,"NOx_ugN_L")) %>%
  select(-Blank, -Date) %>%
  mutate(NOx_ugN_L = if_else(NOx_ugN_L== "< 1.5", as.numeric(0.5), as.numeric(NOx_ugN_L)))

conductivityData <- map_dfr(paste0("./data/2021-09-17_Conductivity/", list.files(path = "./data/2021-09-17_Conductivity")), 
                            function(file, skip, col_names, col_select){
                              read_csv(file = file, skip = skip, col_names = col_names, col_select = col_select) %>%
                                mutate(flume = str_sub(file, start = 32, end = 32)) %>%
                                mutate(dateTime = mdy_hms(dateTime, tz = "America/Denver"))
                              }, 
                            skip = 2, 
                            col_names = c("#","dateTime","Conductivity_uS/cm","Temp_degC","Coupler_Detached","Interval_Change","Coupler_Attached",
                                          "Host_Connected","Stopped","EndOfFile"),
                            col_select = 2:4)
# Note: date times in GMT-6 (Mountain daylight)

preSlugTime <- as_datetime("2021-09-17 7:00:00", tz = "US/Mountain")
postSlugTime <- as_datetime("2021-09-17 14:00:00", tz = "US/Mountain")
conductivities <- conductivityData %>%
  filter(dateTime == preSlugTime) %>%
  mutate(preSlugConductivity_uS_cm = `Conductivity_uS/cm`) %>%
  select(flume, preSlugConductivity_uS_cm) %>%
  left_join(conductivityData %>% filter(dateTime == postSlugTime), by = "flume") %>%
  mutate(postSlugConductivity_uS_cm = `Conductivity_uS/cm`) %>%
  select(flume, preSlugConductivity_uS_cm, postSlugConductivity_uS_cm)

spikeConcentration <- 75*1000*1000*5e-5*14.0067/(500*101.1*0.015) # ppb NO3-N
nutrientIsotopeSampleIDs <- tibble(
  flume= LETTERS[1:15], 
  timepoint = 0:14) %>%
  expand(flume, timepoint) %>%
  filter(timepoint < 10) %>%
  mutate(duplicate = FALSE) %>%
  add_row(flume = c("A", "F", "K", "B", "G", "L", "D", "I", "N"),
          timepoint = rep(c(0,8,3), each = 3), 
          duplicate = TRUE) %>% # duplicates
  add_row(flume = rep("SL", times = 5),
          timepoint = 1:5,
          duplicate = FALSE) %>%
  mutate(date = if_else(timepoint<10, as_date("2021-09-17"), as_date("2021-09-18"))) %>%
  mutate(dateChar = if_else(timepoint<10, "210917", "210918")) %>%
  mutate(ConcentrationSampleID = paste0("CAD_", dateChar,"_", flume, timepoint, "c", if_else(duplicate, "_d", "")),
         sampleID = paste0("CAD_", dateChar,"_", flume, timepoint, "i", if_else(duplicate, "_d", "")), 
         location = paste0("Flume ", flume)) %>%
  filter(!(sampleID %in% c("CAD_210917_I9i", "CAD_210917_J9i"))) %>%
  left_join(nitrateData, by = c("ConcentrationSampleID" = "sampleID" )) %>%
  mutate(OriginalNOx = NOx_ugN_L) %>%
  mutate(spiked = if_else(NOx_ugN_L <= 60, TRUE, FALSE), 
         NOx_ugN_L = if_else(NOx_ugN_L <= 60,NOx_ugN_L + spikeConcentration, NOx_ugN_L)) %>% 
  # Note: added 65 instead of 69 ppb NO3-N to sample A8i because accidentally pipetted 16 mL of sample
  mutate(enriched = if_else(timepoint == 0, "No", "Yes")) %>%
  mutate(estimatedEnrichment = case_when((spiked & enriched == "Yes") ~ ((NOx_ugN_L-spikeConcentration)*0.0489/NOx_ugN_L),
                                         (!spiked & enriched == "Yes") ~ 0.0489,
                                         TRUE ~ NA_real_)) %>%
  mutate(enrichment = if_else(enriched == "Yes", paste0(round(estimatedEnrichment * 100, 2)," at%"), "NA")) %>%
  mutate(Amount = if_else(spiked, "15.05", "25-30")) %>%
  mutate("NO3Concentration" = paste0(round(NOx_ugN_L, 0)," ug/L NO3-N")) %>%
  left_join(conductivities, by = "flume") %>%
  mutate(estimatedConductivity = if_else(timepoint == 0, preSlugConductivity_uS_cm, postSlugConductivity_uS_cm)) %>%
  mutate(Conductivity = paste0(estimatedConductivity, " uS/cm")) %>%
  select(flume, location, timepoint, sampleID, NO3Concentration, enriched, enrichment, Conductivity, Amount, spiked, OriginalNOx) %>%
  arrange(flume, timepoint) 

write.csv(nutrientIsotopeSampleIDs, file = "./sampleIDs/2021-09-17_Nutrient_Isotope_SampleIDs.csv", row.names = FALSE)

sampleIDsToSpike <- nutrientIsotopeSampleIDs %>%
  filter(spiked == TRUE) %>%
  select(sampleID, NOx_ppbN = OriginalNOx)

write.csv(sampleIDsToSpike, file = "./sampleIDs/2021-09-17_SamplesToSpike.csv", row.names = FALSE)
#### Gas Samples ####
gasSampleIDs <- tibble(
  flume= LETTERS[1:15], 
  timepoint = 0:14) %>%
  expand(flume, timepoint) %>%
  filter(timepoint < 11) %>%
  mutate(duplicate = FALSE) %>%
  add_row(flume = c("A", "F", "K", "B", "G", "L", "D", "I", "N"),
          timepoint = rep(c(0,8,3), each = 3), 
          duplicate = TRUE) %>% # duplicates 
  add_row(flume = rep("BL", times = 3),
          timepoint = c(1,2,3),
          duplicate = FALSE) %>%
  # left_join(expectedGasVals, by = "timepoint") %>%
  mutate(date = if_else(timepoint<10, as_date("2021-09-17"), as_date("2021-09-18"))) %>%
  mutate(dateChar = if_else(timepoint<10, "210917", "210918")) %>%
  mutate(sampleID = paste0("CAD_", dateChar,"_", flume, timepoint, "g", if_else(duplicate, "_d", "")),
         location = paste0("Flume ", flume))%>%
  # arrange(estimatedEnrichment_atomPerc, flume) %>%
  # mutate(enriched = if_else(timepoint == 0, "No", "Yes")) %>%
  # mutate(estimatedEnrichment_atomPerc = round(estimatedEnrichment_atomPerc, digits = 2)) %>%
  # mutate(estEnrich = if_else(timepoint > 0, paste0(estimatedEnrichment_atomPerc, "at-%"), "--")) %>%
  # select(date, location, flume, timepoint, enriched, estEnrich, sampleID) 
  select(date, location, flume, timepoint,sampleID) 

write.csv(gasSampleIDs, file = "./sampleIDs/2021-09-17_Gas_SampleIDs.csv", row.names = FALSE)

#### Blank Samples ####
blankSampleIDs <- tibble(
  number = as.character(1:25), 
  sampleID = paste0("CAD_211013_BL", if_else(str_length(number) == 1, paste0("0", number), number))
)

write.csv(blankSampleIDs, file = "./sampleIDs/2021-10-13_Blank_SampleIDs.csv", row.names = FALSE)
