#settings for dams_impact scripts

### function to create a directory if it does not exist
### returns the string of the directory
dir_ <- function(name_dir){
  if(!dir.exists(name_dir)) dir.create(name_dir)
  return(name_dir)
}

# functions for Connectivity (interbasins)
source('R/functions_connectivity.R')


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
dams_used <- "GDATall" # GDAT, GDATall or GrG2FH


### MAR/MVA relation used

# Type
# use to create folder structure with different outputs for different relations used
#mva_type="Minspec_Q001bin"
#mva_type="Minspec_Q0025bin"
#mva_type="Minspec_minbin" 
#mva_type="Q005spec_minbin"
#mva_type="Tspsel_minsp_minbin"
#mva_type="TSuspsel_minsp_minbin" # sensitivity previously
#mva_type="TSuspsel_meansp_minbin_threatened_in" # sensitivity
#mva_type="TSuspsel_meansp_minbin_threatened_in_extinctin" # sensitivity
mva_type="TSuspsel_meansp_minbin" # default
#mva_type="Meansp_minbin" # sensitivity



if(mva_type=="Minspec_Q001bin"){
# Calculate MVA with formula
# TODO The formula in use (should be in MASTER)
int_left=2.93
slope_left=-2.44
int_right=0.01
slope_right=0.84
bp=6
}

#Type
if(mva_type=="Minspec_Q0025bin"){
# Calculate MVA with formula
# TODO The formula in use (should be in MASTER)
int_left=2.86
slope_left=-1.67
int_right=0.58
slope_right=0.65
bp=6
}

if(mva_type=="Minspec_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=2.93
  slope_left=-3.02
  int_right=-1.48
  slope_right=1.41
  bp=6
}

if(mva_type=="Q005spec_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=2.9
  slope_left=-1.7
  int_right=0.42
  slope_right=0.9
  bp=6
}

if(mva_type=="Tspsel_minsp_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.85
  slope_left=-3.76
  int_right=-0.63
  slope_right=1.08
  bp=6
}

if(mva_type=="TSuspsel_minsp_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.85
  slope_left=-3.76
  int_right=-0.65
  slope_right=1.11
  bp=6
}

if(mva_type=="TSuspsel_meansp_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.58
  slope_left=-1.98
  int_right=0.74
  slope_right=1.42
  bp=6
}

if(mva_type=="Meansp_minbin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.35
  slope_left=-1.28
  int_right=0.73
  slope_right=1.31
  bp=6
}

if(mva_type=="TSuspsel_meansp_minbin_threatened_in"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.37
  slope_left=-2.04
  int_right=0.05
  slope_right=1.69
  bp=6
}

if(mva_type=="TSuspsel_meansp_minbin_threatened_in_extinctin"){
  # Calculate MVA with formula
  # TODO The formula in use (should be in MASTER)
  int_left=3.40
  slope_left=-2.11
  int_right=-0.09
  slope_right=1.79
  bp=6
}



calculate_MVA <- function(length_value){
  MVA = 10**(ifelse(length_value<=bp, int_left+slope_left*log10(length_value),  int_right+slope_right*log10(length_value)))
}
# could decide to do <= or < in mva formula (where to include species with length bp)

#habareas <- habareas %>% mutate(MVA=exp((5.455 + 1.343*log(Length)))) #try carvajal formula
# result is in log so should transform back using 10**


### list of directories ### <<<<<<<<<--------- TO MODIFY ACCORDINGLY
#dir_figs <- dir_('Dams_impact/')

#dir_figs <- dir_(paste0('Dams_impact/figs/',mva_type,'_',dams_used,'/'))
#dir_(paste0(dir_figs,"paper/"))
#dir_(paste0(dir_figs,"paper/maps/"))

#dir_tabs <- dir_('Dams_impact/tabs/')

#processed data
dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
dir_proc_out <- dir_(paste0('Dams_impact/proc/',mva_type,'_',dams_used,'/')) # specific for relation

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
dir_sp12 <- 'Fishdata/Fish_hybas/HBlevel12withlakes'



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

# save settings MAR/MVA relationship in txt file in dir_proc_out
fileConn<-file(paste0(dir_proc_out,"relation.txt"))
writeLines(c(paste0("Int_left=",int_left),
             paste0("Slope_left=",slope_left),
             paste0("int_right=",int_right),
             paste0("Slope_right=",slope_right),
             paste0("Breakpoint=",bp)), fileConn)
close(fileConn)
