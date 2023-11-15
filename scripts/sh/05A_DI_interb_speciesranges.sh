#!/bin/bash
#SBATCH --partition=milkun
#SBATCH -N 1
#SBATCH --mem=30G
#SBATCH --output=scripts/sh/logs/05A_DI_interb_speciesranges
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


Rscript scripts/R/05A_DI_interb_speciesranges.R
