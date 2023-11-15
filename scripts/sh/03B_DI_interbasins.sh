#!/bin/bash
#SBATCH --partition=milkun
#SBATCH -N 1 -n 10 
#SBATCH --mem=20G
#SBATCH --array 1-9
#SBATCH --output=scripts/sh/logs/03_DI_interbasins%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


module load R-4.0.4
Rscript scripts/R/03_DI_interbasins.R
