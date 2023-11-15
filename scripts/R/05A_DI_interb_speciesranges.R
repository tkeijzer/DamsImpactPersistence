# Dams impact combine interbasins and species range data
# GoNEXUS
# Tamara Keijzer
# November 2022
# updated April 2023

#setwd("I:/tamara/CC_dams_R")
#SBATCH -N 1
#SBATCH --mem=30G
source("SETTINGS_Dams_impact.R")

dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))

#problems with memory in parallel if multiple cores
ncores=1

#tried to do it in parallel but gave errors with point.in.poly
#testing it in R on linux did not show any problems, so do an array 1:10143 (all species, nrow(ids))
#i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
# Input data -------------------------------------------------------------------
continent = c('af','ar','as','au','eu','gr','na','sa','si')



# species data on hydrobasins lakes level 12
cat('Reading fish l12 data..\n')
fish12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  tab <- read.csv(paste0(dir_sp12, '/fish_hybas_hybas12withLakes_',cont,'.csv'))
  return(tab)
}
fish12 <- fish12 %>% dplyr::select(-id_no)

# hydrobasins lakes level 12 (for sf)
cat('Reading hb12..\n')
hblakes12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp'))
  return(poly)
}
valid <- st_is_valid(hblakes12) # which are valid geometries
hblakes12[valid==F,] <- st_make_valid(hblakes12[valid==F,])# make valid so no self intersection errors
#hb12points <- st_point_on_surface(hblakes12) %>% select(HYBAS_ID)


# interbasins due to dams
interbasins <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  if(dams_used == "GrG2FH"){tab <- readRDS(paste0('Dams_impact/proc/InterBasins_tab_GrG2FH_',cont,'.rds'))}
  if(dams_used == "GDAT"){tab <- readRDS(paste0('Dams_impact/proc/InterBasins_tab_GDAT_',cont,'.rds'))}
  if(dams_used == "GDATall"){tab <- readRDS(paste0('Dams_impact/proc/InterBasins_tab_GDATall_',cont,'.rds'))}
  return(tab)
}


# species traits info
traits <- read.csv('Fishdata/species_traits_extra.csv')
ids <- traits %>% dplyr::select(id_no,binomial)

# to add area to subbasins
hb_area <- hblakes12  %>% as_tibble() %>% dplyr::select(-geometry) %>% dplyr::select(HYBAS_ID,SUB_AREA)



# Combine interbasins and species data -----------------------------------------

# For marine & brackish & diadromous species:
# if subbasin connected to outlet TRUE

# For non-marine/non-brackish/non-diadromous species:
# use MVRS if length data is available
# In next script only these species are selected






#for each species in ids
#function to get a table when running in parallel
build_tab <- function(i){
  id_no_sp <- ids$id_no[i]
  #get the species data
  name <- ids %>% filter(id_no==id_no_sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
  sp <- fish12 %>% filter(binomial == name) # only keep this species range
  sp$id_no <- id_no_sp # add id
  #add interbasins info and hydrobasin area to data
  sp_ib <- left_join(sp,interbasins)
  sp_ib <- left_join(sp_ib,hb_area)
  # calculate total area with no dams scenario (in main basin) and current/future dams (in interbasins)
  sp_ib <- sp_ib %>% group_by(MAIN_BAS) %>% mutate(SUM_AREA_nodams = sum(SUB_AREA))
  sp_ib <- sp_ib %>% group_by(MAIN_BAS,INTER_ID_cur) %>% mutate(SUM_AREA_damscur = sum(SUB_AREA))
  if(dams_used == "GrG2FH"){sp_ib <- sp_ib %>% group_by(MAIN_BAS, INTER_ID_fut) %>% mutate(SUM_AREA_damsfut = sum(SUB_AREA))}
  
  # get species traits
  marine <- traits %>% filter(binomial==name) %>% pull(Marine)
  brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
  length <- traits %>% filter(binomial==name) %>% pull(Length)
  lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
  diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
  
  
  if(!is.na(length)){
    MVRS <- calculate_MVRS(length)
  } 
  
  # if lentic_only, dont produce results. Otherwise check if marine/brackish or not
  if(lentic_only ==1){
    sp_ib$hab_nodams=NA
    sp_ib$hab_curdams=NA
    if(dams_used == "GrG2FH"){sp_ib$hab_futdams=NA}
  }else{
    
    # if marine/brackish/diadromous
    if(marine ==1 | brackish ==1 | diadromous == "t"){
      #dont calculate MVRS, set subbasins connected to outlet to true, others false
      # INTER_NEXT=0 means that the interbasin/hydrobasin is connected to the outlet
      sp_ib$hab_nodams=TRUE
      sp_ib$hab_curdams=FALSE
      sp_ib$hab_curdams[which(sp_ib$INTER_NEXT_cur==0)] <- TRUE
      if(dams_used == "GrG2FH"){
        sp_ib$hab_futdams=FALSE
        sp_ib$hab_futdams[which(sp_ib$INTER_NEXT_fut==0)] <- TRUE
      }
    }else{
      # if not marine/brackish
      # do MVRS
      
      # check if species has length data
      if(is.na(length)){
        sp_ib$hab_nodams=NA
        sp_ib$hab_curdams=NA
        if(dams_used == "GrG2FH"){sp_ib$hab_futdams=NA}
      }else{
        # if species has length data, do MVRS
        sp_ib$hab_nodams <- sp_ib$SUM_AREA_nodams>=MVRS
        sp_ib$hab_curdams <- sp_ib$SUM_AREA_damscur>=MVRS
        if(dams_used == "GrG2FH"){sp_ib$hab_futdams <- sp_ib$SUM_AREA_damsfut>=MVRS}
      }
    }
  }
  #save as RDS
  cat('Write polygons file \n')
  saveRDS(sp_ib,paste0(dir_out,id_no_sp,'.rds'))
  
  
}


#build_tab(i)

# do it parallel
# if ncores is set to 1 it is not in parallel practically
parallel::mcmapply(build_tab,1:nrow(ids),mc.silent = TRUE,mc.cores = ncores)
#gives errors in some cores so use array for now
# could not find the cause of the error. it has something to do with point.in.poly and parallel running...
#array 1-10143
cat('Done! \n')


