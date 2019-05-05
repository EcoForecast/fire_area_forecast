#Note: This is a function from  Hamze Dokoohaki that I have copied and edited locally so that I can controll dependencies and file paths
# On GEO dependencies loaded for  R version 3.5.2


library(ncdf4, lib.loc = "/projectnb/dietzelab/mccabete/R/library") # package for netcdf manipulation
library(raster, lib.loc = "/projectnb/dietzelab/mccabete/R/library")# package for raster manipulation
#library(rgdal) # package for geospatial analysis
#library(ggplot2) # package for plotting
library(purrr, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(dplyr, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(xts, lib.loc = "/projectnb/dietzelab/mccabete/R/library")


#---------------- Extarct ERA5 data
#' ERA5_extract
#'
#' @param lat latitude
#' @param long longitude
#' @param years years to be extracted
#' @param vars variables to be extarcted. If NULL all the variables will be returned.
#' @details For the list of variables check out the documentation at \link{https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation#ERA5datadocumentation-Spatialgrid}.
#' Temperature is returned in degree C.
#'
#' @return a list of xts objects with all the variables for the requested years
#' @export
#' @example
#'  #-36.962324, 149.455727 #Australia Southeastern National Forest
ERA5_extract_ENS <-
  function(lat = -36.962324,        
           long = 149.455727,
           years = c(2017, 2018),
           vars = NULL, outpath = NULL) {
    tryCatch({
      # for each ensemble
      one.year.out <- c(1:10) %>%
        map(function(ens) {
          # for each year
          point.data <- years %>%
            map(function(year) {
              ncfile <-
                paste0("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/ERA5",
                       "/ERA5_",
                       year,
                       ".nc")
              
              #msg
              print(paste0("For ensemble #", ens," ", year, " is being processed !"))
              #open the file
              nc_data <- nc_open(ncfile)
              # time stamp
              t <- ncvar_get(nc_data, "time")
              tunits <- ncatt_get(nc_data, 'time')
              tustr <- strsplit(tunits$units, " ")
              timestamp = as.POSIXct(t * 3600, tz = 'GMT', origin = tustr[[1]][3])
              try(nc_close(nc_data))
              
              
              # set the vars
              if (is.null(vars))
                vars <- names(nc_data$var)
              # for the variables extract the data
              all.data.point <- vars %>%
                map_dfc(function(vname) {
                  #browser()
                  brick.tmp <-
                    brick(ncfile, varname = vname, level = ens)
                  nn <-
                    raster::extract(brick.tmp, SpatialPoints(cbind(long, lat)), method = 'simple') %>%
                    as.numeric()
                  # replacing the missing/filled values with NA
                  nn[nn == nc_data$var[[vname]]$missval] <- NA
                  # send out the extracted var as a new col
                  nn
                  
                }) %>%
                `colnames<-`(paste0(vars))
              #close the connection
              
              # send out as xts object
              xts::xts(all.data.point, order.by = timestamp)
            })
          
          #binding the years
          point.data <- do.call("rbind.xts", point.data)
          #Merge mean and the speard
          return(point.data)
          
        }) %>%
        setNames(c(1:10))
      
      
    }, error = function(e) {
      print(paste0(conditionMessage(e), " just happened!"))
      
    })
    
  }


