#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --nodes=1
#SBATCH --array 1-11728%60
#SBATCH --mem=5G
#SBATCH --output=scripts/sh/logs/iucnhablist/01B_IUCN_habitat_list%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


module load R-4.0.4
Rscript scripts/R/01B_IUCN_habitat_list.R
