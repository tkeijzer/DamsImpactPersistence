#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --nodes=10
#SBATCH --mem=5G
#SBATCH --output=scripts/sh/logs/01F_IUCN_threat_dams%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


module load R-4.0.4
Rscript scripts/R/01F_IUCN_threat_dams.R
