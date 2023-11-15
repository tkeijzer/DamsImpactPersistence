#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --nodes=1
#SBATCH --mem=30G
#SBATCH --array 1-9
#SBATCH --output=scripts/sh/logs/04B_Select_TedescoSU_IUCN%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


module load R-4.0.4
Rscript scripts/R/04B_Select_TedescoSU_IUCN.R
