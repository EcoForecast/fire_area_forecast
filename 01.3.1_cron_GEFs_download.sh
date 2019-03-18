#!/bin/bash -l
#$ -V
#$ -l h_rt=96:00:00
#$ -N GEFs_download_cron
#$ -j y
#$ -l mem_total=98G
#$ -l mem_per_core=8G
#$ -pe omp 8

Rscript /usr3/graduate/tmccabe/mccabete/Fire_forecast_509/scripts/fire_area_forecast/01.2_GEFs_download.R
