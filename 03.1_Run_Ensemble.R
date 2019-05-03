


library(rjags, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")

source("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/02.1.1_Run_JAGS_Model.R")

source("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/01.2.1_data__helper_functions.R")
### Read in Modis data
modis <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2019/MOD14A2.csv")
modis_days <- get_days(data_type = "MOD14A2")
dates <- format(as.POSIXct(modis$X1), "%Y%m%d")
modis <- modis$X2[dates %in% modis_days]

### Read in temp and precip ensembles
met_days <- get_days(data_type = "GEFS")
temp_list <- matrix(ncol = 21, nrow = length(met_days))
precip_list <- matrix(ncol = 21, nrow = length(met_days))

for (i in seq_along(met_days)){
  file_path <- paste("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS", met_days[i], sep = "/" )
  temp_name <- paste(met_days[i], "_full_ensemble_tempurature.csv", sep ="")
  precip_name <- paste(met_days[i], "_full_ensemble_precipitation.csv", sep ="")
  
  temp_path <- paste (file_path, temp_name, sep = "/")
  precip_path <- paste (file_path, precip_name, sep = "/")
  temp_object <- read.csv(temp_path)
  temp_object <- na.omit(temp_object[,-1])
  precip_object <- read.csv(precip_path)
  precip_object <- na.omit(precip_object[,-1])

  ### Sumamrize to 8-day agggrigate (super basic)
  for (j in 1:21){
    precip_list[i,j] <- mean(precip_object[,j])
    temp_list[i,j] <- max(temp_object[,j])
  }
}

### Loop over ensemble members (21)
ensemble_names <- names(temp_object)

for (i in seq_along(ensemble_names)){
  temp <- temp_list[, i]
  precip <- precip_list[, i]
  run_JAGS_model(ensemble_name = ensemble_names[i], temp = temp, precip = precip, modis= modis)
}

