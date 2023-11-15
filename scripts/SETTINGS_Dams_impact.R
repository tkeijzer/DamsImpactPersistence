#settings for dams_impact scripts

local_model_folder="I:/tamara/CC_dams_R" # NOTE: inside all scripts not run on cluster change this


### function to create a directory if it does not exist
### returns the string of the directory
dir_ <- function(name_dir){
  if(!dir.exists(name_dir)) dir.create(name_dir)
  return(name_dir)
}

# functions for Connectivity (interbasins)
source('scripts/R/functions_connectivity.R')


# number of cores available on the machine
ncores = 10


# packages needed
library(sf); library(foreach)
#; library(rfishbase); library(ggplot2);  library(viridis) ; library(vroom)
library(data.table);library(dplyr); library(spatialEco);
library(parallel)
# not fully loaded: foreign, rnaturalearth,


# settings for sf
sf_use_s2(FALSE)

# set fishbase version
#options(FISHBASE_VERSION="21.06")

#function
`%!in%` = Negate(`%in%`)

### DAMS dataset used
dams_used <- "GDATall" # GDAT (only >15m dams), GDATall or GrG2FH (GRanD, GOODD, FhRED)


### MVRS - body size relation used

# Type
# use to create folder structure with different outputs for different relations used
mvrs_type="TSuspsel_meansp_minbin" # default
#mvrs_type="Meansp_minbin" # sensitivity

#Type
if(mvrs_type=="TSuspsel_meansp_minbin"){
  int_left=3.58
  slope_left=-1.98
  int_right=0.74
  slope_right=1.42
  bp=6
}
if(mvrs_type=="Meansp_minbin"){
  int_left=3.35
  slope_left=-1.28
  int_right=0.73
  slope_right=1.31
  bp=6
}

calculate_MVRS <- function(length_value){
  MVRS = 10**(ifelse(length_value<=bp, int_left+slope_left*log10(length_value),  int_right+slope_right*log10(length_value)))
}





### list of directories ### <<<<<<<<<--------- TO MODIFY ACCORDINGLY




#dir_(paste0(dir_figs,"paper/"))
#dir_(paste0(dir_figs,"paper/maps/"))
#dir_tabs <- dir_('Dams_impact/tabs/')

#processed data
dir("Dams_impact")
dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
dir_proc_out <- dir_(paste0('Dams_impact/proc/',mvrs_type,'_',dams_used,'/')) # specific for relation

# species data
dir_spec <- "Fishdata/Fish_hybas/"

#fishsuit data
dir_fishsuit <- "/vol/milkunI/tamara/FishSuit/" #RU
#dir_fishsuit <- "I:/tamara/FishSuit/" # own computer


# location of hydrobasins shapefiles
# data is freely accessible @ https://hydrosheds.org/downloads
# the entire dataset is needed (divided in custom continents 'af','ar','as','au','eu','gr','na','sa','si')
dir_hybas12 <- 'Hybas_lakeL12'
dir_hybas08 <- 'Hybas_lake'

# IUCN data
# data is freely accessible @ https://www.iucnredlist.org/resources/spatial-data-download
# fishdata hydrobasins based
#level 08 tables
dir_sp08 <- 'Fishdata/Fish_hybas'
#level 12
dir_sp12 <- dir_('Fishdata/Fish_hybas/HBlevel12withlakes')



# dams data
# data is freely accessible @
file_grand_dams <- "Dams_data/DAMS/GRanD_dams_v1_3.shp"
file_good2_dams <- "Dams_data/DAMS/GOOD2_unsnapped.shp"
file_frhed_dams <- "Dams_data/DAMS/17_0116_future_dams_update_final_v2.csv"

# or use GDAT dams
file_gdat_dams <- "Dams_data/DAMS/GDAT_data_v1/GDAT_data_v1/data/GDAT_v1_dams.shp"

# regional dams datasets
file_nir_dams <- "Dams_data/Regional/DamsUS/nation.gpkg"
file_mek_dams <- "Dams_data/Regional/DamsMekong/regionaldams.csv"
file_bra_dams <- "Dams_data/Regional/DamsBrazil/Aproveitamento_Hidreletricos_AHE.shp"



# for plotting maps
#crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# for assigning species to KG climate zones
#file_KG_ras <- '~/data/Beck_KG_V1_present_0p0083.tif'

# save settings MVRS relationship in txt file in dir_proc_out
fileConn<-file(paste0(dir_proc_out,"relation.txt"))
writeLines(c(paste0("Int_left=",int_left),
             paste0("Slope_left=",slope_left),
             paste0("int_right=",int_right),
             paste0("Slope_right=",slope_right),
             paste0("Breakpoint=",bp)), fileConn)
close(fileConn)
