library(calendR)

#### July 2021 ####
JulyDates <- seq(as.Date("2021-07-01"), as.Date("2021-07-31"), by = "1 day")
JulyEvents <- rep(NA, times = length(JulyDates))
JulyEvents[c(27, 30)]<- "Energy Labs - no rush"

png(filename = "./plots/nitrateSamplingCalenderJuly2021.png",
    width = 11,
    height = 8,
    units = "in",
    res = 100)
calendR(
  year = 2021, 
  month = 7,
  special.days = JulyEvents,
  special.col = c("lightblue"),
  title = "Nitrate Sampling Plan - July",
  text = c("Tap water", "Tap water"), 
  text.pos = c(27,30)
)
dev.off()

#### August 2021 ####
AugDates <- seq(as.Date("2021-08-01"), as.Date("2021-08-31"), by = "1 day")
AugEvents <- rep(NA, times = length(AugDates))
AugEvents[c(3, 7, 10)]<- "Energy Labs - no rush"

png(filename = "./plots/nitrateSamplingCalenderAug2021.png",
    width = 11,
    height = 8,
    units = "in",
    res = 100)
calendR(
  year = 2021, 
  month = 8,
  special.days = AugEvents,
  special.col = c("lightblue"),
  title = "Nitrate Sampling Plan - August",
  text = c("Tap water", "Tap water", "Tap water,\n ASW Conc, \nEffluent"), 
  text.pos = c(3,7,10),
  legend.pos = "bottom",
  orientation = "landscape"
)
dev.off()

#### September 2021 ####
SeptDates <- seq(as.Date("2021-09-01"), as.Date("2021-09-30"), by = "1 day")
SeptEvents <- rep(NA, times = length(SeptDates))
SeptEvents[c(14)] <- "MSU-EAL"
SeptEvents[c(17)] <- "FLBS"
png(filename = "./plots/nitrateSamplingCalenderSept2021.png",
    width = 11,
    height = 8,
    units = "in",
    res = 100)
calendR(
  year = 2021, 
  month = 9,
  special.days = SeptEvents,
  special.col = c("grey80", "dodgerblue"),
  title = "Nitrate Sampling Plan - September",
  text = c("All SAMS, \nASW, \nEffluent","15N-NO3 release \n(Sample \neverything)"), 
  text.pos = c(14, 17),
  legend.pos = "bottom",
  orientation = "landscape"
)
dev.off()