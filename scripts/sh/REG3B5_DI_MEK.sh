#!/bin/bash
#SBATCH --partition=milkun
#SBATCH -N 1 -n 10
#SBATCH --mem=20G
#SBATCH --array 1-6
#SBATCH --output=scripts/sh/logs/REG3B5_DI_MEK%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


Rscript scripts/R/REG3B5_DI_MEK.R
