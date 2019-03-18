### Visualization of GEFS data
#  Plotting the different ensemble tempurature 
#  and precipitation data. 
#
#

library(tidyverse)

## arguments provided
outfolder <- "/Users/tess/Documents/work/Gefs_download2015/" ## For local debugging 
#lat.in <- -36.962324
#lon.in <- 149.455727
#sitename <- "Southeastern_national_forest"
#start_date <- Sys.time() ## Note, take this argument from a different script, if not visualizing recently downloaded data, will not work 
#day <- format(start_date, "%Y%m%d")
#outfolder <- paste("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS/", day, "/",  sep = "")

## Read in data
temp_ens <- read.csv( paste(outfolder,day,"_", "full_ensemble_tempurature.csv", sep = ""), header = TRUE)
precip_ens <- read.csv( paste(outfolder,day,"_", "full_ensemble_precipitation.csv", sep = ""), header = TRUE)

## Not ggplot best practice
temp_plot <- ggplot(temp_ens) + 
  geom_line(aes(1:32, ensemble_1))+ 
  geom_line(aes(1:32, ensemble_2))+
  geom_line(aes(1:32, ensemble_3))+
  geom_line(aes(1:32, ensemble_4))+
  geom_line(aes(1:32, ensemble_5))+
  geom_line(aes(1:32, ensemble_6))+
  geom_line(aes(1:32, ensemble_7))+ 
  geom_line(aes(1:32, ensemble_8))+
  geom_line(aes(1:32, ensemble_9))+
  geom_line(aes(1:32, ensemble_10))+
  geom_line(aes(1:32, ensemble_11))+
  geom_line(aes(1:32, ensemble_12))+
  geom_line(aes(1:32, ensemble_13))+
  geom_line(aes(1:32, ensemble_14))+
  geom_line(aes(1:32, ensemble_15))+
  geom_line(aes(1:32, ensemble_16))+
  geom_line(aes(1:32, ensemble_17))+
  geom_line(aes(1:32, ensemble_18))+
  geom_line(aes(1:32, ensemble_19))+
  geom_line(aes(1:32, ensemble_20))+
  geom_line(aes(1:32, ensemble_21))+
  labs(x= "Hours", y = "Tempurature Degrees K")

temp_plot

precip_plot <- ggplot(precip_ens) + 
  geom_line(aes(1:32, ensemble_1))+ 
  geom_line(aes(1:32, ensemble_2))+
  geom_line(aes(1:32, ensemble_3))+
  geom_line(aes(1:32, ensemble_4))+
  geom_line(aes(1:32, ensemble_5))+
  geom_line(aes(1:32, ensemble_6))+
  geom_line(aes(1:32, ensemble_7))+ 
  geom_line(aes(1:32, ensemble_8))+
  geom_line(aes(1:32, ensemble_9))+
  geom_line(aes(1:32, ensemble_10))+
  geom_line(aes(1:32, ensemble_11))+
  geom_line(aes(1:32, ensemble_12))+
  geom_line(aes(1:32, ensemble_13))+
  geom_line(aes(1:32, ensemble_14))+
  geom_line(aes(1:32, ensemble_15))+
  geom_line(aes(1:32, ensemble_16))+
  geom_line(aes(1:32, ensemble_17))+
  geom_line(aes(1:32, ensemble_18))+
  geom_line(aes(1:32, ensemble_19))+
  geom_line(aes(1:32, ensemble_20))+
  geom_line(aes(1:32, ensemble_21))+
  labs(x= "Hours", y = "Precipitation Flux kgm-2s-1")

precip_plot
