#!/bin/bash -l
#$ -V
#$ -l h_rt=96:00:00
#$ -N Run_model_calibration
#$ -j y
#$ -pe omp 8

module load R/3.5.2
Rscript /usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/03.1_Run_Historical_calibration.R
