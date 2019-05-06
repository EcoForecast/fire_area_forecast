# # install the packages
# repos = "http://cran.us.r-project.org"
# get.pkg <- function(pkg){
#   loaded <- do.call("require",list(package=pkg))
#   if(!loaded){
#     print(paste("trying to install",pkg))
#     install.packages(pkg,dependencies=TRUE,repos=repos)
#     loaded <- do.call("require",list(package=pkg))
#     if(loaded){
#       print(paste(pkg,"installed and loaded"))
#     } 
#     else {
#       stop(paste("could not install",pkg))
#     }    
#   }
# }
# 
# get.pkg("sp")
# get.pkg("Rcpp")
# get.pkg("gdalUtils")
# #get.pkg("rgdal")
# 
# library(sp)
# library(gdalUtils)
# library(raster)
# 
# # loop through the data in the list
# EVI_dir <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD13A2/2019"
# out_dir <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD13A2/"
# sub.folders <- list.dirs(EVI_dir, recursive=TRUE)[-1]
# 
# evi.files <- list()
# for (j in seq_along(sub.folders)) {
#   evi.files[[j]] <- dir(sub.folders[j],"\\.hdf$")
# }
# #test <- lapply(evi.files, function(x) if(identical(x, character(0))) NA_character_ else x)
# evi.list <- evi.files[lapply(evi.files,length)>0]
# 
# 
# for (i in 1:length(evi.list)){
#   image_name = evi.list[i][[1]]
#   image_dir = paste0(sub.folders[i], '/', image_name)
#   
#   gdalinfo(image_dir)
#   EVI_data <- get_subdatasets(image_dir)
#   gdal_translate(EVI_data[2], dst_dataset = "EVI.tif")
#   EVI_raster <- raster("EVI.tif")
#   EVI_mean <- round(cellStats(EVI_raster, stat='mean', na.rm=TRUE)/100000000, 4)
#   
#   date <- as.Date(substr(image_name,10,16), "%Y%j")
#   tab <- cbind(as.character(date), EVI_mean)
#   interval_date <- date + 8
#   interval_tab <- cbind(as.character(interval_date), EVI_mean)
#   tmp_tab <- rbind(tab, interval_tab)
#   if (i == 1){
#     out_tab <- tmp_tab
#   }else{
#     out_tab <- rbind(out_tab, tmp_tab)
#   }
# 
#   #if wanted to plot
#   #plot(rast, main= "EVI of Australia South East National Park")
# }
# colnames(out_tab) <- c("date", "mean_EVI")
# csv_filename <- paste0(out_dir, "EVI_data.csv")
# 
# write.csv(out_tab, csv_filename)



## ----------------------------------------------------------------------------------##
### process on the MODIS data
# read in the FireMask sub dataset from MOD14A2 files
# calculate active fire area and write out a csv file
# caution: you will need to run module load R_earth in the bash beforehand

install.packages("gdalUtils", repos = 'http://cran.us.r-project.org')
library(gdalUtils)
library(rgdal)
library(raster)

# arguments provided as in and out folders
infolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2016/" 
outfolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2016/"

### here is a loop to read in all hdf datas and write out a single csv containing:
# 1. the starting date of record
# 2. the number of fire pixels
#setwd(infolder)
sub.folders <- list.dirs(infolder, recursive=TRUE)[-1]

fire.files <- list()
for (j in seq_along(sub.folders)) {
  fire.files[[j]] <- dir(sub.folders[j],"\\.hdf$")
}
#test <- lapply(evi.files, function(x) if(identical(x, character(0))) NA_character_ else x)
fire.list <- fire.files[lapply(fire.files,length)>0]
out <- matrix(NA,length(fire.list),2)

for (i in 1:length(fire.list)){
  image_name = fire.list[i][[1]]
  image_dir = paste0(sub.folders[i], '/', image_name)  

  fireArea <- as.numeric(gsub("FirePix=","",gdalinfo(image_dir)[26])) * 1000^2
  startDate <- as.Date(gsub("StartDate=","",gdalinfo(image_dir)[80]))
  out[i,1] <- format(startDate,format="%Y-%m-%d")
  out[i,2] <- fireArea
  print(i)
}

setwd(outfolder)
write.csv(data.frame(out),"MOD14A2.csv")



## ----------------------------------------------------------------------------------##
### process on the VIIRS

# arguments provided as in and out folders
infolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/2018/" 
outfolder <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/2018/"

sub.folders <- list.dirs(infolder, recursive=TRUE)[-1]

fire.files <- list()
for (j in seq_along(sub.folders)) {
  fire.files[[j]] <- dir(sub.folders[j],"\\.h5$")
}
#test <- lapply(evi.files, function(x) if(identical(x, character(0))) NA_character_ else x)
fire.list <- fire.files[lapply(fire.files,length)>0]
out <- matrix(NA,length(fire.list),2)

for (i in 1:length(fire.list)){
  image_name = fire.list[i][[1]]
  image_dir = paste0(sub.folders[i], '/', image_name)  
  
  fire_data <- get_subdatasets(image_dir)
  gdal_translate(fire_data[1], dst_dataset = "VIIRS_fire.tif")
  fire_raster <- raster("VIIRS_fire.tif")
  fire_raster[fire_raster < 7] <- NA
  pixel_freq <- freq(fire_raster)
  fireArea <- (1440000 - pixel_freq[which(is.na(pixel_freq[,1])), 2]) * 375^2
  
  startDate <- as.Date(gsub("RangeBeginningDate=","",gdalinfo(image_dir)[67]))
  out[i,1] <- format(startDate,format="%Y-%m-%d")
  out[i,2] <- fireArea
  print(i)
}



# colnames(out_tab) <- c("date", "mean_EVI")
# csv_filename <- paste0(out_dir, "EVI_data.csv")
# 
# write.csv(out_tab, csv_filename)


setwd(outfolder)
write.csv(data.frame(out),"VNP14A1.csv")