#GEO RUN: 

#setwd("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/")

#.libPaths("/usr2/postdoc/kzarada/R/x86_64-pc-linux-gnu-library/3.5")
.libPaths("/usr3/graduate/tmccabe/mccabete/R/library")

#install.packages("googlesheets", repos='http://cran.us.r-project.org', dependencies = TRUE)
#install.packages("googleAuthR", repos='http://cran.us.r-project.org')
#install.packages("XML", repos='http://cran.us.r-project.org' )
#install.packages("stringi", repos='http://cran.us.r-project.org')
#install.packages('curl', repos='http://cran.us.r-project.org')
#install.packages("digest", repos='http://cran.us.r-project.org')
#install.packages("udunits2",repos='http://cran.us.r-project.org', configure.args = "--with-udunits2-lib=/share/pkg/udunits/2.2.20/install/lib --with-udunits2-include=/share/pkg/udunits/2.2.20/install/include")
#install.packages("ncdf4",repos='http://cran.us.r-project.org')
#install.packages("rlang", repos = 'http://cran.us.r-project.org')
#install.packages("purrr", repos='http://cran.us.r-project.org')
#install.packages("Rcpp", repos='http://cran.us.r-project.org')
#install.packages("tidyselect", repos='http://cran.us.r-project.org')
#install.packages("dplyr", repos='http://cran.us.r-project.org')
#install.packages('rnoaa', repos='http://cran.us.r-project.org')
#install.packages('lubridate', repos='http://cran.us.r-project.org')

library(ncdf4, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library") #lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library"

#library(googlesheets)
#library(googleAuthR)
library(XML, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(curl, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(dplyr, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(scales, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(lazyeval, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(lubridate, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library") #lubridate must be before rnoaa
library(tidyr, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(xml2, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(rnoaa) #must be my library

library(udunits2)

#function to download GEFS
download.NOAA_GEFS <- function(outfolder, lat.in, lon.in, sitename, start_date = Sys.time(), end_date = (as.POSIXct(start_date, tz="UTC") + lubridate::days(8)),
                               overwrite = FALSE, verbose = FALSE, ...) {
  
  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date <- as.POSIXct(end_date, tz = "UTC")
  
  #It takes about 2 hours for NOAA GEFS weather data to be posted.  Therefore, if a request is made within that 2 hour window,
  #we instead want to adjust the start time to the previous forecast, which is the most recent one avaliable.  (For example, if
  #there's a request at 7:00 a.m., the data isn't up yet, so the function grabs the data at midnight instead.)
  if (abs(as.numeric(Sys.time() - start_date, units="hours")) <= 2) {
    start_date = start_date - lubridate::hours(2)
    end_date = end_date - lubridate::hours(2)
  }
  
  
  #Set the end forecast date (default is the full 16 days)
  if (end_date > start_date + lubridate::days(16)) {
    end_date = start_date + lubridate::days(16)
  }
  
  #Round the starting date/time down to the previous block of 6 hours.  Adjust the time frame to match.
  forecast_hour = (lubridate::hour(start_date) %/% 6) * 6 #Integer division by 6 followed by re-multiplication acts like a "floor function" for multiples of 6
  forecast_hour <- as.numeric(forecast_hour)
  increments = as.integer(as.numeric(end_date - start_date, units = "hours") / 6) #Calculating the number of forecasts between start and end dates.
  increments = increments + ((lubridate::hour(end_date) - lubridate::hour(start_date)) %/% 6) #These calculations are required to use the rnoaa package.
  
  end_hour = sprintf("%04d", ((as.numeric(forecast_hour) + (increments * 6)) %% 24) * 100)  #Calculating the starting hour as a string, which is required type to access the 
  #data via the rnoaa package
  forecast_hour = sprintf("%04d", forecast_hour * 100)  #Having the end date as a string is useful later, too.
  
  #Recreate the adjusted start and end dates.
  start_date = as.POSIXct(paste0(lubridate::year(start_date), "-", lubridate::month(start_date), "-", lubridate::day(start_date), " ", 
                                 substring(forecast_hour, 1,2), ":00:00"), tz="UTC")
  end_date = start_date + lubridate::hours(increments * 6)
  
  #Bounds date checking
  #NOAA's GEFS database maintains a rolling 12 days of forecast data for access through this function.
  #We do want Sys.Date() here - NOAA makes data unavaliable days at a time, not forecasts at a time.
  NOAA_GEFS_Start_Date = as.POSIXct(Sys.Date(), tz="UTC") - lubridate::days(11)  #Subtracting 11 days is correct, not 12.
  
  
  
  #################################################
  #NOAA variable downloading
  #Uses the rnoaa package to download data
  
  #We want data for each of the following variables. Here, we're just getting the raw data; later, we will convert it to the 
  #cf standard format when relevant.
  noaa_var_names = c("Temperature_height_above_ground_ens",  "Total_precipitation_surface_6_Hour_Accumulation_ens")
  
  #These are the cf standard names
  cf_var_names = c("air_temperature",  
                   "precipitation_flux")
  cf_var_units = c("K", "kgm-2s-1")  #Negative numbers indicate negative exponents
  
  # This debugging loop allows you to check if the cf variables are correctly mapped to the equivalent
  # NOAA variable names.  This is very important, as much of the processing below will be erroneous if 
  # these fail to match up.
  # for (i in 1:length(cf_var_names)) {
  #  print(sprintf("cf / noaa   :   %s / %s", cf_var_names[[i]], noaa_var_names[[i]]))
  #}
  
  noaa_data = list()
  
  
  #Downloading the data here.  It is stored in a matrix, where columns represent time in intervals of 6 hours, and rows represent
  #each ensemble member.  Each variable gets its own matrix, which is stored in the list noaa_data.
  for (i in 1:length(noaa_var_names)) {
    noaa_data[[i]] = rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1:increments, 
                                 forecast_time = forecast_hour, date=format(start_date, "%Y%m%d"))$data
    
  }
  
  #Fills in data with NaNs if there happens to be missing columns.
  for (i in 1:length(noaa_var_names)) {
    if (!is.null(ncol(noaa_data[[i]]))) { # Is a matrix
      nans <- rep(NaN, nrow(noaa_data[[i]]))
      while (ncol(noaa_data[[i]]) < increments) {
        noaa_data[[i]] <- cbind(noaa_data[[i]], nans)
      }
    } else {   # Is a vector
      while (length(noaa_data[[i]]) < increments) {
        noaa_data[[i]] <- c(noaa_data[[i]], NaN);
      }
    }
  }
  
  ###################################################
  # Not all NOAA data units match the cf data standard.  In this next section, data are processed to
  # confirm with the standard when necessary.
  # The following is a list of variables which need to be processed:
  # 1. NOAA's relative humidity must be converted to specific humidity
  # 2. NOAA's measure of precipitation is the accumulation over 6 hours; cf's standard is precipitation per second
  
  #Convert NOAA's relative humidity to specific humidity
  #humid_index = which(cf_var_names == "specific_humidity")
  
  #Temperature, pressure, and relative humidity are required to calculate specific humidity.
  #humid_data = noaa_data[[humid_index]]
  temperature_data = noaa_data[[which(cf_var_names == "air_temperature")]]
  #pressure_data = noaa_data[[which(cf_var_names == "air_pressure")]]
  
  #Depending on the volume and dimensions of data you download, sometimes R stores it as a vector and sometimes
  #as a matrix; the different cases must be processed with different loops.
  #(The specific corner case in which a vector would be generated is if only one hour is requested; for example, 
  #only the data at time_idx 1, for example).
  
  # rh2qair <-  function (rh, T, press = 101325) 
  # {
  #  stopifnot(T[!is.na(T)] >= 0)
  #  Tc <- udunits2::ud.convert(T, "K", "degC")
  #    es <- 6.112 * exp((17.67 * Tc)/(Tc + 243.5))
  #    e <- rh * es
  #    p_mb <- press/100
  #    qair <- (0.622 * e)/(p_mb - (0.378 * e))
  #    return(qair)
  #  }
  
  
  # if (as.logical(nrow(humid_data))) {
  #    for (i in 1:length(humid_data)) {
  #     humid_data[i] = rh2qair(humid_data[i], temperature_data[i], pressure_data[i])
  #    }
  # } else {
  #   for (i in 1:nrow(humid_data)) {
  #      for (j in 1:ncol(humid_data)) {
  #        humid_data[i,j] = rh2qair(humid_data[i,j], temperature_data[i,j], pressure_data[i,j])
  #      }
  #    }
  #  }
  
  
  
  
  #Update the noaa_data list with the correct data
  #noaa_data[[humid_index]] <- humid_data
  
  # Convert NOAA's total precipitation (kg m-2) to precipitation flux (kg m-2 s-1)
  #NOAA precipitation data is an accumulation over 6 hours.
  precip_index = which(cf_var_names == "precipitation_flux")
  
  #The statement udunits2::ud.convert(1, "kg m-2 6 hr-1", "kg m-2 s-1") is equivalent to udunits2::ud.convert(1, "kg m-2 hr-1", "kg m-2 s-1") * 6,
  #which is a little unintuitive. What will do the conversion we want is what's below:
  noaa_data[[precip_index]] = udunits2::ud.convert(noaa_data[[precip_index]], "kg m-2 hr-1", "kg m-2 6 s-1")  #There are 21600 seconds in 6 hours
  
  #############################################
  # Done with data processing.  Now writing the data to the specified directory. Each ensemble member is written to its own file, for a total
  # of 21 files.  
  if (!dir.exists(outfolder)) {
    dir.create(outfolder, recursive=TRUE, showWarnings = FALSE)
  }
  
  # Create a data frame with information about the file.  This data frame's format is an internal PEcAn standard, and is stored in the BETY database to
  # locate the data file.  The data file is stored on the local machine where the download occured.  Because NOAA GEFS is an 
  # ensemble of 21 different forecast models, each model gets its own data frame.  All of the information is the same for 
  # each file except for the file name.
  results = data.frame(
    file = "",                            #Path to the file (added in loop below).
    mimetype = "application/x-netcdf",    #Format the data is saved in
    formatname = "CF Meteorology",        #Type of data
    startdate = paste0(format(start_date, "%Y-%m-%dT%H:%M:00")),    #starting date and time, down to the second
    enddate = paste0(format(end_date, "%Y-%m-%dT%H:%M:00")),        #ending date and time, down to the second
    dbfile.name = "NOAA_GEFS",            #Source of data (ensemble number will be added later)
    stringsAsFactors = FALSE
  )
  
  results_list = list()
  
  #Each ensemble gets its own file.
  #These dimensions will be used for all 21 ncdf4 file members, so they're all declared once here.
  #The data is really one-dimensional for each file (though we include lattitude and longitude dimensions
  #to comply with the PEcAn standard).
  time_dim = ncdf4::ncdim_def(name="time", 
                              paste(units="hours since", format(start_date, "%Y-%m-%dT%H:%M")), 
                              seq(6, 6 * increments, by = 6),
                              create_dimvar = TRUE)
  lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat.in, create_dimvar = TRUE)
  lon_dim = ncdf4::ncdim_def("longitude", "degree_east", lon.in, create_dimvar = TRUE)
  
  dimensions_list = list(time_dim, lat_dim, lon_dim)
  
  nc_var_list = list()
  for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
    nc_var_list[[i]] = ncdf4::ncvar_def(cf_var_names[i], cf_var_units[i], dimensions_list, missval=NaN)
  }
  
  #For each ensemble
  for (i in 1:21) { # i is the ensemble number
    #Generating a unique identifier string that characterizes a particular data set.
    identifier = paste("NOAA_GEFS", sitename, i, format(start_date, "%Y-%m-%dT%H:%M"), 
                       format(end_date, "%Y-%m-%dT%H:%M"), sep=".")
    
    ensemble_folder = file.path(outfolder)
    
    #Each file will go in its own folder.
    #if (!dir.exists(ensemble_folder)) {
    # dir.create(ensemble_folder, recursive=TRUE, showWarnings = TRUE)
    #}
    
    flname = file.path(ensemble_folder, paste(identifier, "nc", sep = "."))
    
    #Each ensemble member gets its own unique data frame, which is stored in results_list
    #Object references in R work differently than in other languages. When adding an item to a list, R creates a copy of it
    #for you instead of just inserting the object reference, so this works.
    results$file <- flname
    results$dbfile.name <- flname
    results_list[[i]] <- results
    
    if (!file.exists(flname) | overwrite) {
      nc_flptr = ncdf4::nc_create(flname, nc_var_list, verbose=verbose)
      
      #For each variable associated with that ensemble
      for (j in 1:length(cf_var_names)) {
        # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
        ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], noaa_data[[j]][i,])
      }
      
      ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
    } #else {
    #PEcAn.logger::logger.info(paste0("The file ", flname, " already exists.  It was not overwritten."))
    #}
    
  }
  
  return(results_list)
}#download.NOAA_GEFS


#outfolder <- "/Users/tess/Documents/work/Gefs_download2015/" ## For local debugging 
#start_date <- "2019-04-30 12:00:00 EDT" # for local debugigng
#start_date <- as.POSIXct(start_date) # Doesn't work for local debudding but IF put wrong day (ie 31st on month with only 30 days)
#test_day <- "2019-04-30 12:00:00 EDT" # for local debugging
#day <- "20190430"
lat.in <- -36.962324
lon.in <- 149.455727
sitename <- "Southeastern_national_forest"
start_date <- Sys.time()
day <- format(start_date, "%Y%m%d")
outfolder <- paste("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS/", day, "/",  sep = "")

download.NOAA_GEFS(outfolder= outfolder, lat.in =lat.in, lon.in= lon.in, sitename = sitename, start_date = start_date)

###### Create a simple to interpret csv from ensemble .nc files
## How many numbers will there be, based on the number of days requested
day_number <- 8
hour_number <- day_number *4
flist_8_days <- list.files(path = outfolder)
flist_8_days <- flist_8_days[grep("NOAA*", flist_8_days)]
#days_pattern <- paste(format(start_date, "%Y-%m-%dT%H:%M"), ".2019-", sep="")
#flist_8_days <- flist_8_days[grep(days_pattern, flist_8_days)]

day_index <- sort(rep(1:day_number, 4)) ## what day measurement came from

## Setup the empty files
tempurature <- matrix(data = NA, nrow = length(day_index), ncol =  length(flist_8_days))
precipitation <- matrix(data = NA, nrow = length(day_index), ncol =  length(flist_8_days))

precipitation <- as.data.frame(precipitation)
tempurature <- as.data.frame(tempurature)

colnames(tempurature) <- paste(rep("ensemble", 21), as.character(c(1:21)), sep= "_") # Ensemble ID's 
colnames(precipitation) <- paste(rep("ensemble", 21), as.character(c(1:21)), sep= "_") # Ensemble ID's 


for (i in seq_along(flist_8_days)){
  tmp_nc <- nc_open( paste(outfolder, flist_8_days[i], sep = ""))
  precip_tmp <- ncvar_get(tmp_nc, "precipitation_flux")
  temp_tmp <- ncvar_get(tmp_nc, "air_temperature")
  
  if(length(precip_tmp) != length(day_index)){
    diff <- length(day_index) - length(precip_tmp) # length(day_index) should be maximum 32
    precip_tmp <- c(precip_tmp, rep(NA, diff))
    temp_tmp <- c(temp_tmp, rep(NA, diff))
  }
  
  precipitation[,i] <-  precip_tmp
  tempurature[,i] <- temp_tmp
  nc_close(tmp_nc)
}

tempurature <- cbind(day_index, tempurature)
precipitation <- cbind(day_index, precipitation)

write.csv(tempurature, paste(outfolder,day,"_", "full_ensemble_tempurature.csv", sep = ""), row.names = FALSE) # Useful for writing out full ensemble temp & precip
write.csv(precipitation, paste(outfolder,day,"_", "full_ensemble_precipitation.csv", sep = ""), row.names = FALSE)

## Aggrigate to some composite number (Will likely need to make more sophisticated)
#  for now, just two numbers. 
#  We can incorperate all the ensemble members/ hours more formally when we write the model.
eight_day_max_temp <- max(tempurature[,-1])
precipitation <- colMeans(precipitation[,-1])

eight_day_mean_precip <- mean(precipitation)

current_meteorology <- as.data.frame(cbind(eight_day_max_temp,eight_day_mean_precip ))

## Write out results
write.csv2(current_meteorology, paste(outfolder,day, "current_meteorology.csv", sep = "")) ## Can be overwritten. Least information file. 

