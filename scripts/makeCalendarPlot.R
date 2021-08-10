library(calendR)

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

AugDates <- seq(as.Date("2021-07-01"), as.Date("2021-07-31"), by = "1 day")
AugEvents <- rep(NA, times = length(JulyDates))
AugEvents[c(3, 7, 9)]<- "Energy Labs - no rush"
AugEvents[c(23)]<- "Energy Labs - rush"
AugEvents[c(12, 16, 19, 26)]<- "FLBS"

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
  text = c("Tap water", "Tap water", "Tap water,\n ASW Conc",
           "Tap water,\n ASW conc.,\n effluent", "Tap water,\n ASW conc.,\n effluent", "Tap water,\n ASW conc.,\n effluent", 
           "8 SAMS,\n tap water,\n ASW conc.,\n effluent", "15N-NO3 release \n(Sample \neverything)"), 
  text.pos = c(3,7,9,12,16,19,23,26),
  legend.pos = "bottom",
  orientation = "landscape"
)
dev.off()