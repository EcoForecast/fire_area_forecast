#!/bin/bash -l
#$ -V
#$ -l h_rt=96:00:00
#$ -N GEF_download
#$ -j y
#$ -l mem_total=98G
#$ -l mem_per_core=8G
#$ -pe omp 8

module load R/3.4.2
module load udunits/2.2.20

Rscript /usr3/graduate/shijuan/Desktop/fire_area_forecast/01.2_GEFs_download.R
