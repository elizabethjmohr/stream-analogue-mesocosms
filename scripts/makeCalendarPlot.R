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
AugEvents[c(16)] <- "Energy Labs - rush"
AugEvents[c(28, 29, 30, 31)] <- "FLBS"

png(filename = "./plots/nitrateSamplingCalenderAug2021.png",
    width = 11,
    height = 8,
    units = "in",
    res = 100)
calendR(
  year = 2021, 
  month = 8,
  special.days = AugEvents,
  special.col = c("lightblue", "yellow", "dodgerblue"),
  title = "Nitrate Sampling Plan - August",
  text = c("Tap water", 
           "Tap water", 
           "Tap water,\n ASW Conc, \nEffluent",
           rep("Influent \nand effluent", 5)), 
  text.pos = c(3,7,10, 16, 28, 29, 30, 31),
  legend.pos = "bottom",
  orientation = "landscape"
)
dev.off()

#### September 2021 ####
SeptDates <- seq(as.Date("2021-09-01"), as.Date("2021-09-30"), by = "1 day")
SeptEvents <- rep(NA, times = length(SeptDates))
SeptEvents[c(14, 17)] <- "MSU-EAL"
SeptEvents[c(1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 15, 16)] <- "FLBS"
png(filename = "./plots/nitrateSamplingCalenderSept2021.png",
    width = 11,
    height = 8,
    units = "in",
    res = 100)
calendR(
  year = 2021, 
  month = 9,
  special.days = SeptEvents,
  special.col = c("dodgerblue", "grey80"),
  title = "Nitrate Sampling Plan - September",
  text = c(rep("Influent \nand effluent", 3), 
           "All SAMS, \nInfluent, \nEffluent",
           rep("Influent \nand effluent", 4),
           "All SAMS, \nInfluent, \nEffluent",
           rep("Influent \nand effluent", 4),
           "All SAMS, \nInfluent, \nEffluent",
           rep("Influent \nand effluent", 2),
           "15N-NO3 release \n(Sample \neverything)"), 
  text.pos = c(1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 14, 15, 16, 17),
  legend.pos = "bottom",
  orientation = "landscape"
)
dev.off()