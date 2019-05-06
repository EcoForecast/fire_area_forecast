library(rjags, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(raster, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")

source("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/02.1.1_Run_JAGS_Model.R")
source("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/load_ERA_nc_vars.R")

#source("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/01.2.1_data__helper_functions.R")
### Read in Modis data
modis_2018 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2018/MOD14A2.csv")
modis_2017 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2017/MOD14A2.csv")
modis <- c(modis_2017$X2, modis_2018$X2)

# Read in VIIRS

viirs_2018 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/2018/VNP14A1_M.csv")
viirs <- c(rep(NA, 46), viirs_2018$fire_area) ## Needs to be same length as modis

### Era5 data
lat <- -36.962324
long <- 149.455727
years <- c(2017, 2018)
vars = c("t2m", "tp")

era5 <- load_ERA5_Tair(lat = lat, long = long, years = years, vars = vars)

temp <- era5[1]
precip <- era5[2]

precip <- apply(as.data.frame(precip), 1, mean)
temp <- apply(as.data.frame(temp), 1, max)


  #temp <- temp_list[, i]
  #precip <- precip_list[, i]
run_JAGS_model(ensemble_name = "testing", temp = temp, precip = precip, modis= modis, viirs =viirs_2018$fire_area )


