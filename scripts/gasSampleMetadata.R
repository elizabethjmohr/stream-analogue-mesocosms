library(readxl)
library(tidyverse)
library(here)

#### LinxII Data Import and Manipulation ####
linx <- read_xlsx(path = here("data", "LINXII.xlsx"), 
                  sheet = "Data",
                  col_types = c(rep("text", times = 3), 
                                rep("numeric", times = 11)),
                  na = "NA")

# Note: rate constants were originally given in units of m^-1, and velocities in m/min, so final 
# rate constant units as calculated here are in min^-1

racewayDepth <- 0.15 # meters

linx <- linx %>% 
  filter(!NO3 == 0) %>%
  mutate(ktot_norm = ktot * Velocity * Depth/racewayDepth,
         kden_norm = kden * Velocity * Depth/racewayDepth) %>% 
  mutate(ku_norm = ktot_norm - kden_norm) %>%
  mutate(ku_norm_log = log(ku_norm),
         kden_norm_log = log(kden_norm),
         ktot_norm_log = log(ktot_norm),
         NO3_log = log(NO3), 
         ER_log = log(ER_grams_m2_day)) %>%
  filter(!is.na(ER_log))

#### Parameter Estimation ####
library(rjags)
library(coda)
library(eivtools)

# Make data into a list that can be passed to JAGS
data <- list(x = linx$NO3_log, kTot = linx$ktot_norm_log, kDen = linx$kden_norm_log, n = nrow(linx))

# Specify moments for priors
data$b0 <- c(0,0)  # means for multivariate-normal linear regression coefficient prior
data$Vb <- solve(diag(1000,2)) # precisions for multivariate-normal linear regression coefficient prior
data$s1 <- 0.1  # shape parameter for gamma-distributed error prior 
data$s2 <- 0.1  # scale parameter for gamma-distributed error prior

# Define model code
multivariate_regression <- "
model{

  betakTot ~ dmnorm(b0,Vb)      ## multivariate Normal prior on vector of regression params
  betakDen ~ dmnorm(b0,Vb)       ## multivariate Normal prior on vector of regression params
  preckTot ~ dgamma(s1,s2)   ## prior precision
  preckDen ~ dgamma(s1,s2)   ## prior precision

  for(i in 1:n){
      mukTot[i] <- betakTot[1] + betakTot[2]*x[i]   
      kTot[i]  ~ dnorm(mukTot[i], preckTot)  
      mukDen[i] <- betakDen[1] + betakDen[2]*x[i]  
      kDen[i]  ~ dnorm(mukDen[i], preckDen)  
  }
}"

# Specify number of chains, initial conditions
nChains = 3
inits <- list()
for(i in 1:nChains){
  inits[[i]] <- list(betakTot = rnorm(2,0,5), betakDen = rnorm(2,0,5), preckTot = runif(1,1/100,1/20), preckDen = runif(1,1/100,1/20))
}

# Create JAGS model object
jagsModel <- jags.model(file = textConnection(multivariate_regression),
                        data = data,
                        inits = inits,
                        n.chains = nChains)
# Sample posterior distribution 
var.out <- coda.samples (model = jagsModel,
                         variable.names = c("betakTot", "betakDen", "preckTot", "preckDen"),
                         n.iter = 5000)

# Evaluate Model
plot(var.out)
gelman.diag(var.out)
GBR <- gelman.plot(var.out)

# Remove burn-in
burnin = 1000                               
jags.burn <- window(var.out, start=burnin)  
plot(jags.burn)   

# Convert output to matrix
out <- as.matrix(jags.burn)

#### Simulation ####
library(isoToolsR)
source(here("functions", "NUptakeModels.R"))
source(here("functions", "predictionIntervals.R"))

# Specify number of realizations
n <- 5000

# Define indices to sample MCMC output
indices <- sample.int(nrow(out), n, replace = TRUE)

# Calculate percent of total nitrate that was N15
RstN <- 0.003678 # isotope ratio of nitrogen standard
AFadd <- 0.984 # atom fraction of N15-nitrate tracer
percentTracer <- calcPercentTracer(13000, RstN, AFadd)

times = c(0,10,40,85,140,200,280,360,475,600,1440)

# For each combination of parameters, generate ensemble of predictions for delta 15N-NGas vs. time
NO30 <- 220/14 # umol/L
ensemble <- predictN15(
  N15NO30 = NO30*percentTracer, 
  NO30 = NO30, 
  NO3Pre = 5/14, 
  k2 = 0.015, 
  B0Tot = out[indices,"betakTot[1]"], 
  B1Tot = out[indices,"betakTot[2]"], 
  precTot = out[indices,"preckTot"], 
  B0Den = out[indices, "betakDen[1]"],
  B1Den = out[indices,"betakDen[2]"], 
  precDen = out[indices,"preckDen"], 
  times = times
)

# Calculate median, 2.5% and 97.5% quantiles for each time
NO3PIs <- getPI(ensemble, times = times, varName = "Nitrate")
N15NO3PIs <- getPI(ensemble, times = times, varName = "N15Nitrate")
NGasPIs <- getPI(ensemble, times = times, varName = "NGas")
N15GasPIs <- getPI(ensemble, times = times, varName = "N15Gas")

# Calculate atmospheric pressure in Bozeman
# source: https://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
calcPressure <- function(elev){
  101325*(1-2.25577e-5*elev)^5.25588
}
p_boz <- calcPressure(4793*0.3048) #Pa
p_davis <- calcPressure(52*0.3048) #Pa

# Calculate saturated concentrations of N2 and N20 in Bozeman at sample temperature
T_samps <- 16 #degrees C
gases <- tibble(
  name = c("N2", "N20"),
  H0 = c(6.4e-6, 2.4e-4),
  dlnHd1T = c(1600, 2700),
  perc_air = c(0.78, 3.25e-7)
) %>%
  mutate(H_16 = H0*exp(dlnHd1T*(1/(T_samps + 273.15)-1/298.15))) %>%
  mutate(C_sat_boz = H_16*p_boz*perc_air) #mmol/L

# Assume 99% of NGas production is N2, 1% is N20
# Assume concentrations are near saturation
expectedGasVals <- tibble(
 timepoint = 0:10,
 N2_mmolL = gases$C_sat_boz[1], #mmol/L
 N2O_mmolL = gases$C_sat_boz[2], #mmol/L
 N15_N2_mmolL = gases$C_sat_boz[1]*RToAtomFrac(RstN)+(NGasPIs$Median*0.99)/1000,
 N15_N2O_mmolL = gases$C_sat_boz[2]*RToAtomFrac(RstN)+(NGasPIs$Median*0.99)/1000,
 estimatedEnrichment_atomPerc = 100*N15_N2_mmolL/N2_mmolL) %>%
  mutate(N2_ppm= 10^6*N2_mmolL/(gases$H_16[1]*p_boz),
         N2O_ppm= 10^6*N2O_mmolL/(gases$H_16[2]*p_boz))


