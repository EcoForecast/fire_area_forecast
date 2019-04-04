#!/bin/bash -l
#$ -V
#$ -l h_rt=96:00:00
#$ -N extract_data.R
#$ -j y
#$ -l mem_total=98G
#$ -l mem_per_core=8G
#$ -pe omp 8

module purge
module load R_earth/3.1.0
Rscript /usr3/graduate/zhangyt/codes/fire_area_forecast/02.0_extract_data.R