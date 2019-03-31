# install the packages
repos = "http://cran.us.r-project.org"
get.pkg <- function(pkg){
  loaded <- do.call("require",list(package=pkg))
  if(!loaded){
    print(paste("trying to install",pkg))
    install.packages(pkg,dependencies=TRUE,repos=repos)
    loaded <- do.call("require",list(package=pkg))
    if(loaded){
      print(paste(pkg,"installed and loaded"))
    } 
    else {
      stop(paste("could not install",pkg))
    }    
  }
}

#get.pkg("sp")
#get.pkg("Rcpp")
#get.pkg("gdalUtils")
#get.pkg("rgdal")

library(sp)
library(gdalUtils)
library(raster)

# loop through the data in the list
EVI_dir <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD13A2/2019"
out_dir <- "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/MOD13A2/"
sub.folders <- list.dirs(EVI_dir, recursive=TRUE)[-1]

evi.files <- list()
for (j in seq_along(sub.folders)) {
  evi.files[[j]] <- dir(sub.folders[j],"\\.hdf$")
}
#test <- lapply(evi.files, function(x) if(identical(x, character(0))) NA_character_ else x)
evi.list <- evi.files[lapply(evi.files,length)>0]


for (i in 1:length(evi.list)){
  image_name = evi.list[i][[1]]
  image_dir = paste0(sub.folders[i], '/', image_name)
  
  gdalinfo(image_dir)
  EVI_data <- get_subdatasets(image_dir)
  gdal_translate(EVI_data[2], dst_dataset = "EVI.tif")
  EVI_raster <- raster("EVI.tif")
  EVI_mean <- round(cellStats(EVI_raster, stat='mean', na.rm=TRUE)/100000000, 4)
  
  date <- as.Date(substr(image_name,10,16), "%Y%j")
  tab <- cbind(as.character(date), EVI_mean)
  interval_date <- date + 8
  interval_tab <- cbind(as.character(interval_date), EVI_mean)
  tmp_tab <- rbind(tab, interval_tab)
  if (i == 1){
    out_tab <- tmp_tab
  }else{
    out_tab <- rbind(out_tab, tmp_tab)
  }

  #if wanted to plot
  #plot(rast, main= "EVI of Australia South East National Park")
}
colnames(out_tab) <- c("date", "mean_EVI")
csv_filename <- paste0(out_dir, "EVI_data.csv")

write.csv(out_tab, csv_filename)






