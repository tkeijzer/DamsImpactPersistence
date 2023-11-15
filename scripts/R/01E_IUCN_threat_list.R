
library(rredlist)
library(dplyr)
library(sf)
#SBATCH --nodes=1
#SBATCH --array 1-10143%60
#SBATCH --mem=5G
source("scripts/SETTINGS_Dams_impact.R")

# PERHAPS FIRST DO FOR EACH SPECIES SEPERATELY THE THREAT DF SAVE AS RDS
# FOR SPECIES WITHOUTH THREAT DATA SAVE AN EMPTY DF?

#ncores=10

#WHEN USING FISHSUIT
#dir_fishsuit="I:/tamara/FishSuit/" #local
#dir_fishsuit="/vol/milkunI/tamara/FishSuit/" #cluster
#ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
#dir_threats="/vol/milkunI/tamara/CC_dams_R/Fishdata/iucn_threat_list/" # make sure this folder is there

#WHEN NOT USING FISHSUIT
ids <- read.csv("Fishdata/species_traits_extra.csv") %>% dplyr::select(id_no,binomial)
dir_("Fishdata/iucn_threat_list")
dir_threats="Fishdata/iucn_threat_list/" # make sure this folder is there

token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) # 1 to 10143

sp=ids$binomial[i]
hab <- rl_threats(name = sp,key = token)$result

if(class(hab)=="list"){hab=data.frame()} #empty dataframe if no threats info


saveRDS(hab,paste0(dir_threats,sp,'.rds'))