#!/bin/bash -l
#$ -V
#$ -l h_rt=24:00:00
#$ -N extract_Mfire
#$ -j y
#$ -l mem_total=98G


module purge
module load R_earth #/3.1.0
Rscript /projectnb/dietzelab/mccabete/Fire_forecast_509/scripts/tess_geo_fork/fire_area_forecast/02.0_extract_data.R

