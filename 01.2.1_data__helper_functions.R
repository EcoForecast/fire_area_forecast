##` @title get_days       
##` @param first_data        date we started downloading data
##` @param most_recent_date  Present date 
##` @param data_type         What data should be checked. Can look for "GEFS", "MOD14A2", or "VNP14A1"
##` @param data_path         The path to all data files
##` @day_interval            The number of days incorperated into a product
##` @description             This function checks data that is downloaded, and returns only the days that correspond to the day_interval. Will return a warning if today might contain data that hasn't been downloaded. 
library(lubridate, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
get_days <- function(first_date = '20170101', most_recent_date = Sys.time(),day_interval = 8, data_type = "GEFS", data_path = '/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/'){
  
  full_data_path <- paste(data_path, data_type, sep = "") # Make data_path type specific
  most_recent_date <- format(most_recent_date, "%Y%m%d")
  recent_date_lubri <- lubridate::as_date(as.character(most_recent_date))
  first_data_lubri <- lubridate::as_date(as.character(first_date))
  
  ### IF GEFS
  if(data_type == "GEFS"){
    dir <- list.dirs(full_data_path)[-1]
    len = length(dir)
    dates <- rep(NA, len)
    
    
    ## Begin file-based loop
    
    i=0
    for (f in dir){
      i=i+1
      if (i==len){  # right now the 0328 data is missing
        break
      }
      date = basename(f)
      
      
      timeDiff <- lubridate::as_date(as.character(date)) - first_data_lubri # get 
      timeDiff <- as.numeric(timeDiff) 
      if((timeDiff %% day_interval) == 0){
        dates[i] <-  date
      }
      
    }  
    
    dates <- na.omit(dates)
    lubri_format <- lubridate::as_date(dates)
    most_recent_data <- max(lubri_format) 
    
    timeDiff <- recent_date_lubri - first_data_lubri # check to see if we are missing most recent data
    timeDiff <- as.numeric(timeDiff) 
    if((timeDiff %% day_interval) == 0){
      recent_day_is_usefull <- TRUE
    }else{
      recent_day_is_usefull <- FALSE
    }
    
    if(recent_day_is_usefull && (recent_date_lubri > most_recent_data)){
      warning(paste("There is a useable", data_type, "product that has yet to be downloaded."))
    }
    
    if(lubridate::as_date(as.character(most_recent_date))){
      
    }
    dates <- as.matrix(dates)
  }
  
  ### IF MODIS
  if(data_type == "MOD14A2"){
  full_data_path <- paste(full_data_path, "/2017/MOD14A2.csv", sep="")
  modis_current <- read.csv(full_data_path)
  modis_current <- as.matrix(modis_current)
  
  ### Get dates in same format as GEFS. Not strictly nessisary. 
  dates <- modis_current[,2]
  dates <- lubridate::as_date(dates)
  dates <- dates[dates >= first_data_lubri]
  
  dates <- format(dates, "%Y%m%d")
  dates <- na.omit(dates)
  dates <- as.matrix(dates)
    
  }
  
  ### IF VIIRS
  if(data_type == "VNP14A1"){
    full_data_path <- paste(full_data_path, "/2019/VNP14A1.csv", sep="")
    viirs_current <- read.csv(full_data_path)
    viirs_current <- as.matrix(modis_current)
    
    ### Get dates in same format as GEFS. Not strictly nessisary. 
    dates <- viirs_current[,2]
    dates <- lubridate::as_date(dates)
    dates <- dates[dates >= first_data_lubri]
    
    dates <- format(dates, "%Y%m%d")
    dates <- na.omit(dates)
    dates <- as.matrix(dates)
    
  }
  if(data_type != "MOD14A2" && data_type != "GEFS"&& data_type != "VNP14A1"){
    stop("Only Modis and GEFS implemented")
  }
  
  return(dates)
}

