# This function generates a time series of N15-N2 + N15-N20 (N15Gas) and of N15-NO3 concentrations
# for each set of regression coefficients and residual error precisions.
# Assumption: Dissolved N2 gas concentration at steady state before tracer experiment

predictN15 <- function(N15NO30, NO30, NO3Pre, k2, B0Tot, B1Tot, precTot, B0Den, B1Den, precDen, times, AFPre = 0.003664522){
  kTotMu <- B0Tot + B1Tot * log(NO30) 
  kTot <- rlnorm(n = length(kTotMu), meanlog = kTotMu, sdlog = sqrt(1/precTot))
  kDenMu <- B0Den + B1Den * log(NO30) 
  kDen <- rlnorm(n = length(kDenMu), meanlog = kDenMu, sdlog = sqrt(1/precDen))
  Nitrate <- matrix(NA,n,length(times))
  N15Nitrate <- matrix(NA,n,length(times))
  NGas <- matrix(NA,n,length(times))
  N15Gas <- matrix(NA,n,length(times))
  for(i in 1:length(times)){
    Nitrate[,i] <- NO30*exp(-kTot * times[i])
    N15Nitrate[,i] <- N15NO30*exp(-kTot * times[i])
    NGas[,i] <- (kDen*NO3Pre/k2)*exp(-k2*times[i]) + NO30 * (kDen/(k2 - kTot))*(exp(-kTot*times[i]) - exp(-k2*times[i]))
    N15Gas[,i] <- N15NO30 * (kDen/(k2 - kTot))*(exp(-kTot*times[i])- exp(-k2*times[i])) 
  }
  return(list(N15Nitrate = N15Nitrate, N15Gas = N15Gas, Nitrate = Nitrate, NGas = NGas, kTot = kTot, kDen = kDen, k2 = k2, NO30 = NO30, NO3Pre = NO3Pre))
}
