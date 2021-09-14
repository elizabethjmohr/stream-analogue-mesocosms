regress <- function(nightOfData){
  # spline <- ss(as.numeric(nightOfData$dateTime), nightOfData$DO_mgL, spar = -0.4)
  nightOfData <- nightOfData %>%
    arrange(dateTime) %>%
    mutate(dOdt = 0) %>%
    mutate(def = `DO.sat` - DO_mgL)
  
  for(i in 2:(nrow(nightOfData))){
    # nightOfData[i, "dOdt"] <- predict(spline, as.numeric(nightOfData$dateTime[i]), deriv = 1)$y
    nightOfData[i, "dOdt"] <- (nightOfData[i+1, "DO_mgL"] - nightOfData[i-1, "DO_mgL"])*3600/
      (as.numeric(nightOfData[i+1, "dateTime"]) - as.numeric(nightOfData[i-1, "dateTime"]))
  } # derivative in units of per hour
  
  lmOut <- lm(dOdt ~ def, data = nightOfData[2:(nrow(nightOfData) - 1),])
  return(summary(lmOut)$coefficients)
}

nighttimeRegression <- function(DOdata, nightStart, nightEnd){
  df <- DOdata %>% 
    filter(hour(dateTime) >= nightStart | hour(dateTime) < nightEnd) %>% 
    mutate(nightOf = date(dateTime - hours(nightEnd))) %>%
    group_by(nightOf) %>%
    nest() %>%
    mutate(parameterEstimates = map(data, regress)) %>%
    mutate(interceptEstimate = map_dbl(parameterEstimates, function(table) table[1,1][[1]]),
           interceptStdError = map_dbl(parameterEstimates, function(table) table[1,2][1]),
           slopeEstimate = map_dbl(parameterEstimates, function(table) table[2,1]),
           slopeStdError = map_dbl(parameterEstimates, function(table) table[2,2])) %>%
    ungroup() %>%
    select(nightOf,interceptEstimate,interceptStdError,slopeEstimate,slopeStdError) 
  return(df)
}