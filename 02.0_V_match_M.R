rm(list=ls())

#function to abbrevaite paste
"%+%" <- function(x,y) paste(x,y,sep="")

#match the VIIRS data to MODIS
year = 2019
csv_dir = "/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNP14A1/"%+%year
file_name <- paste0(csv_dir, "/VNP14A1.csv", sep="")
stats_all <- read.csv(file_name, header=T, sep=",")

periods = ceiling(nrow(stats_all)/8)
out_tab = as.data.frame(stats_all[seq(1,66,8),2])
max_fire_area = matrix(NA, periods, 1)

for (i in 1:periods){
  if (i != periods){
    start = 8*i - 7
    end = 8*i
    max_fire_area[i] = max(stats_all[start:end,3])
  }
  else{
    start = 8*i - 7
    end = nrow(stats_all)
    max_fire_area[i] = max(stats_all[start:end,3])
  }
  
}

out_tab = cbind(out_tab, max_fire_area)
colnames(out_tab) = c("date","fire_area")

out_csv <- paste(csv_dir%+%"/VNP14A1_M.csv")
write.csv(out_tab, file = out_csv)

