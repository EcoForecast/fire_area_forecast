# This script is going to download the MODIS EVI/FIRE and VIIRS data from the http server
# -- VNR14A1
# translate the matlab code to the R code
rm(list = ls())

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
get.pkg("RCurl")
library(pracma)
library(base)
library(arules)


# give the year of data wanted be downloading
# could write to a function if required multiple years
iyear = 2019

# data starting from Feburary of 2000
if(iyear == 2000){
  s_icomp = 16
  e_icomp = 366
} else if(iyear == 2019){
  s_icomp = 1
  e_icomp = 150
} else{
  s_icomp = 1
  e_icomp = 366
}
accept_tile_string <- "*h29v12*.hdf"

# data temporal resolution
interval <- 1

idate <- paste0(as.character(iyear), "/01/01")
starting_date <- as.Date(idate)

# download data
for(icomp in 33:120){
  if((icomp >= s_icomp) & (icomp <= e_icomp)){
    idoy = (icomp - 1) * interval + 1
    this_date = starting_date + (icomp-1) * interval
    this_year = as.numeric(format(this_date,'%Y'))
    this_month = as.numeric(format(this_date,'%m'))
    this_day = as.numeric(format(this_date,'%d'))
    date_str = sprintf('%04d.%02d.%02d', this_year, this_month, this_day)
    
    save_root = sprintf('/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/VNR14A1/%04d/%03d/', 
                        iyear, idoy)
    
    
    if(dir.exists(save_root) == F){
      dir.create(file.path(save_root), recursive = TRUE)
    }
    setwd(save_root)
    
    cmd = sprintf('wget --user yingtong_zhang --password zyt921113TT -A %s -r -nH -np -nv --cut-dirs=4 https://e4ftl01.cr.usgs.gov/VIIRS/VNP14A1.001/%s/', accept_tile_string, date_str)
    
    system(cmd)
    fprintf('YEAR=%04d,Idoy=%03d,Icomp=%02d done!\n',iyear,idoy,icomp)
    
  }
  
}

