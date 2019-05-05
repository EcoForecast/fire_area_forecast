##' Calculates cummulative Tair (within one year for one ensemble)
##'
##' @param lat The site latitude
##' @param long The site longitude
##' @param years The desired years to download
##' @param vars The varible names for desired varibles. Works for Tair and precip. 
##' @import xts
##' @export
load_ERA5_Tair <- function(lat,long,years, outpath = "/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/ERA5/tair", vars = c("t2m", "tp") ) {
  source('/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/nc_extractor_Ens.R')
  
  out <- ERA5_extract_ENS(lat=lat, long=long,years=years,var=vars, outpath = outpath)
  TairsOutput <- numeric()
  PrecipOutput <- numeric()
  temp_index <- 1
  precip_index <- 2
  
  
  ### Generate an 8 day index for MODIS
  length_data <- length(out[[1]][,1]) # in 3-hour increments
  nyears <- xts::nyears(out[[1]][,1])
  ndays <- xts::ndays(out[[1]][,1])
  daily <- as.data.frame(xts::apply.daily(out[[1]][,1],mean))
  
  
  end_points <- xts::endpoints(daily, on ="years" )
  end_points <- end_points + 1 ## Endpoints starts at zero, MODIS and VIIRS doesn't
  years_index <- matrix(NA, nrow = 46, ncol = nyears) 
  
  for(i in 1:nyears){ ## Make index of every 8 days for individual years (needs to be looped because year/8 not perfect, will always start at 01/01 every year)
    print(i)
    years_index[,i ] <- seq(end_points[i],to = end_points[i +1], by = 8)
  }
  
    every_eight_days_index <- as.vector(years_index)
  
  for(e in 1:10){
    t <- out[[e]]
    ### get temp
    tDaily <- as.data.frame(xts::apply.daily(t[,temp_index],mean))
    test8day <- xts::period.apply(tDaily, every_eight_days_index, mean)
    test8day <- test8day %>% mutate(Date=rownames(.))
    TairsOutput <- cbind(TairsOutput,test8day[,1])
    
    ### get precip
    tDaily <- as.data.frame(xts::apply.daily(t[,precip_index],mean))
    test8day <- xts::period.apply(tDaily, every_eight_days_index, mean)
    test8day <- test8day %>% mutate(Date=rownames(.))
    PrecipOutput <- cbind(PrecipOutput,tDaily[,1])
    
    #colnames(TairsOutput) <- paste(rep("ensemble", 10), as.character(c(1:10)), sep= "_") # Ensemble ID's 
    #colnames(PrecipOutput) <- paste(rep("ensemble", 10), as.character(c(1:10)), sep= "_") # Ensemble ID's 
    
  }
  PrecipOutput[PrecipOutput < 0] <- 0 # previously had VERY small nagative value, seems to be a 0 replacement. 
  era5 <- list(PrecipOutput,TairsOutput )
  return(era5)
}


## Quick test of the function
#library(raster, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
#lat <- -36.962324
#long <- 149.455727
#years <- c(2017, 2018)
#vars = c("t2m", "tp")
 
#test <- load_ERA5_Tair(lat = lat, long = long, years = years, vars = vars)

