#!/bin/bash
#SBATCH --partition=milkun
#SBATCH -N 1 -n 10
#SBATCH --time=04:00:00
#SBATCH --mem=10G
#SBATCH --output=scripts/sh/logs/05C_DI_create_CRL_PAF_PLR_tabs_%a.log
#SBATCH --mail-type=END
#SBATCH --mail-user="your@mail.com"


Rscript scripts/R/05C_DI_create_CRL_PAF_PLR_tabs.R
