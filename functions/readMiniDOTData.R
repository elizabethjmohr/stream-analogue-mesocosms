#' Read in a single day of MiniDOT data 
#'
#' @param filePath Path of the MiniDOT text file to read in
#'
#' @return Tibble with columns for time (seconds), DO (mg/L),
#'  and temperature (degrees C)
#'  
#' @export
#' 
readMiniDOT <- function(filePath){
  data <- read_csv(filePath, skip = 3)
  names(data) <- c("Time_sec", "BV_volts","T_degC", "DO_mgL", "Q")
  return(data)
}

#' Read in and concatenate all miniDOT data files for a single logger
#'
#' @param loggerPath path to directory containing all text files
#' for single logger
#' @param startDate first date of data to read in, as a Date object
#' @param endDate last date of data to read in, as a Date object
#'
#' @return Tibble with columns for logger serial number, time (seconds), 
#' DO (mg/L), and temperature (degrees C)
#' 
#' @export
#' 
readLoggerFiles <- function(loggerPath, startDate, endDate){
  fileNames <- list.files(loggerPath)
  dates <- as_date(substr(fileNames, 1,10))
  fileNames <- fileNames[dates >=startDate & dates <= endDate]
  df <- map(paste0(loggerPath, "/", fileNames), readMiniDOT) %>%
    map_dfr(bind_rows) %>%
    mutate(serial = str_sub(loggerPath, start = -6L, end = -1L))
  return(df)
}

#' Read in and concatenate all miniDOT data files for multiple loggers
#'
#' @param path path to directory containing directories for each logger
#' @param startDate first date of data to read in, as a Date object
#' @param endDate last date of data to read in, as a Date object
#'
#' @return Tibble with columns for loggerID (serial number), 
#' date/time (US Mountain time), DO (mg/L), and temperature (degrees C)
#' 
#' @export
#' 
readAllLoggerFiles <- function(path, startDate, endDate){
  dirs <- list.dirs(path)[-1] # logger directories
  
  data <- map(dirs, readLoggerFiles, startDate = startDate, endDate = endDate)  %>% 
    bind_rows() %>%
    mutate(dateTime = with_tz(as_datetime(Time_sec)), tzone = "US/Mountain") %>%
    select(loggerID = serial, dateTime, DO_mgL, T_degC)
  
  return(data)
}


#' Join barometric pressure data to oxygen data
#'
#' @param oxygenData 
#' @param baroData 
#'
#' @return Tibble with barometric pressure data joined to original DO data
#' @export
#'
#' @examples
joinBaroData <- function(oxygenData, baroData){
  baroData <- baroData %>% filter(dateTime >= min(oxygenData$dateTime), dateTime <= max(oxygenData$dateTime))
  b <- data.table(baroData)
  o <- data.table(oxygenData)
  setkey(b, dateTime)
  setkey(o, dateTime)
  tibble(b[o, roll = "nearest"]) %>% select(dateTime, loggerID, DO_mgL, T_degC, P_mb)
}



