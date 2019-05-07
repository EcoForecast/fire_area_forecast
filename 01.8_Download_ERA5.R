library(Matrix, "/share/pkg/r/3.5.2/install/lib64/R/library")
library(jsonlite, "/share/pkg/r/3.5.2/install/lib64/R/library")
library(reticulate)
library(future, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(rlang, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(purrr, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")
library(furrr)
setwd("/projectnb/dietzelab/mccabete/Fire_forecast_509/data/ERA5/")
reticulate::use_python("/share/pkg/python/2.7.13/")

plan(multiprocess)

c(2016:2018) %>%
  map(function(year) {
    
    cdsapi <-import("cdsapi")
    c <- cdsapi$Client()
    
    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = 'ensemble_members',
        'format' = 'netcdf',
        'day' = list('01','02','03',
                     '04','05','06',
                     '07','08','09',
                     '10','11','12',
                     '13','14','15',
                     '16','17','18',
                     '19','20','21',
                     '22','23','24',
                     '25','26','27',
                     '28','29','30',
                     '31'),
        'time' = list('00:00','03:00','06:00',
                      '09:00','12:00','15:00',
                      '18:00','21:00'),
        'month' = list('01','02','03',
                       '04','05','06',
                       '07','08','09',
                       '10','11','12'),
        'year' = as.character(year),
        "area" = "-27.194/137.37/-37.225/156.54",            # the Southeast corner  of Australia
        'variable' = list( "2m_temperature",
                          "total_precipitation","surface_thermal_radiation_downwards")
      ),
      paste0('ERA5_',year,'.nc')
    )
  })


