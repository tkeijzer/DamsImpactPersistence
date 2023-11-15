# Retrieve habitat type IUCN species
# Code adapted from FishSuit
# Sometimes does not work on linux radboud, Error: Not Found (HTTP 404), could try Sys.sleep(2)

# Tamara Keijzer
# October 2022

#SBATCH --nodes=1
#SBATCH --array 1-11728%60
#SBATCH --mem=5G
source("scripts/SETTINGS_Dams_impact.R")

library("rredlist")



dir_habitat <- dir_(paste0('Fishdata/iucn_habitat_list/'))

#my personal token
token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'


# load species data
sp <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0(dir_spec,'fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
sp <- sp %>% filter(binomial !="") # this is not ok (also remove when using species data)


sp_names <- sp %>% select(binomial) %>% distinct() %>% .$binomial
#11,727 names

i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#manually did the species that had errors (checked by ordering on size log file)
#sp_again=c(7337:7341,7343,
#           7345:7347,7368:7371,
#           7402,7404,7408:7421,7430:7438,
#           2095,9252)

# retrieve the habitat type per species
#for(i in sp_again) {
  n=sp_names[i]
  hab <- rl_habitats(as.character(n),key = token)$result$habitat
  saveRDS(hab,paste0(dir_habitat,n,'.rds'))
  
  Sys.sleep(2)
  
#}
  
  # "Acipenser brevirostrum Delaware/Chesapeake Bay subpopulation" keep getting error