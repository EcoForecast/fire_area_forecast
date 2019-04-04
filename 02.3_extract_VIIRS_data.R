### process on the MODIS data
# read in the FireMask sub dataset from MOD14A2 files
# calculate active fire area and write out a csv file
# caution: you will need to run module load R_earth in the bash beforehand

install.packages("gdalUtils")
library(gdalUtils)
library(rgdal)
library(raster)

# arguments provided as in and out folders
infolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/2019/" 
outfolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/2019/"

### here is a test. fire_data[26] stores number of fire pixels, fire_data[80] stores start date
# fire_data <- paste0(infolder, "VNP14A1.A2019033.h29v12.001.2019035010349.h5")
# gdalinfo(fire_data)[26]
# gdalinfo(fire_data)[80]


### here is a loop to read in all hdf datas and write out a single csv containing:
# 1. the starting date of record
# 2. the number of fire pixels
setwd(infolder)
days <- as.numeric(dir(pattern = ""))
start.doy <- min(days)
end.doy <- max(days)
out <- matrix(NA,length(days),2)

for(i in 1:length(days)){
  #if(days[i]<100 == TRUE){
  # directory <- paste(infolder,0,days[i],'/',sep='')
  #}else{
  # directory <- paste(infolder,days[i],'/',sep='')
  #}
  directory <- paste0(0, days[i],'/', sep='')
  file <- paste0(directory,dir(path = directory))
  fireArea <- as.numeric(gsub("FirePix=","",gdalinfo(file)[26])) * 1000^2
  startDate <- as.Date(gsub("StartDate=","",gdalinfo(file)[80]))
  out[i,1] <- format(startDate,format="%Y-%m-%d")
  out[i,2] <- fireArea
}

setwd(outfolder)
write.csv(data.frame(out),"VNP14A1.csv")