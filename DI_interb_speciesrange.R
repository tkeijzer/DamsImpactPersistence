# Dams impact combine interbasins and species range data
# GoNEXUS
# Tamara Keijzer
# November 2022
# updated April 2023

#setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")

dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_raster <- dir_(paste0(dir_proc_out,"model_DI_occurrence_raster/"))

#tried to do it in parrallel but gave errors with point.in.poly
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


# get species id_no to link to fishsuit output
ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
#ids <- ids %>% as_tibble() %>% dplyr::select(-geom) 
# in this id list species without fishbase data are already removed

# species traits info
traits <- read.csv(paste0(dir_fishsuit,"proc/species_traits_extra.csv")) # from fishsuit, species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no)

# to add area to subbasins
hb_area <- hblakes12  %>% as_tibble() %>% dplyr::select(-geometry) %>% dplyr::select(HYBAS_ID,SUB_AREA)

# optional variables
#not_coastal_mbs <- hblakes %>% as_tibble() %>% dplyr::select(-geometry) %>% 
#  group_by(MAIN_BAS) %>% summarise(coastal=mean(COAST)) %>% 
#  filter(coastal ==0) %>% dplyr::pull(MAIN_BAS)







# Combine interbasins and species data -----------------------------------------

# For marine & brackish species:
# if subbasin connected to outlet TRUE

# For non-marine/non-brackish species:
# use MVA if length data is available







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
  marine <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Marine)
  brackish <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Brackish)
  length <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Length)
  lentic_only <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(lentic_only)
  
  
  if(!is.na(length)){
    MVA <- calculate_MVA(length)
  } 
  
  # if lentic_only, dont produce results. Otherwise check if marine/brackish or not
  if(lentic_only ==1){
    sp_ib$hab_nodams=NA
    sp_ib$hab_curdams=NA
    if(dams_used == "GrG2FH"){sp_ib$hab_futdams=NA}
  }else{
    
    # if marine/brackish
    if(marine ==1 | brackish ==1){
      #dont calculate MVA, set subbasins connected to outlet to true, others false
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
        # do MVA
      
        # check if species has length data
        if(is.na(length)){
          sp_ib$hab_nodams=NA
          sp_ib$hab_curdams=NA
          if(dams_used == "GrG2FH"){sp_ib$hab_futdams=NA}
        }else{
          # if species has length data, do MVA
          sp_ib$hab_nodams <- sp_ib$SUM_AREA_nodams>=MVA
          sp_ib$hab_curdams <- sp_ib$SUM_AREA_damscur>=MVA
          if(dams_used == "GrG2FH"){sp_ib$hab_futdams <- sp_ib$SUM_AREA_damsfut>=MVA}
          }
        }
      }
  #save as RDS
  cat('Write polygons file \n')
  saveRDS(sp_ib,paste0(dir_out,id_no_sp,'.rds'))
  
  
  sp_ib <- left_join(sp_ib,hblakes12 %>% select(HYBAS_ID,geometry))
  #save as RDS in "raster format" (points)
  #select relevant column in large df
  if(dams_used == "GDAT" || dams_used == "GDATall"){
  sp_ib_smaller <- sp_ib %>% dplyr::select(id_no,binomial,
                                           HYBAS_ID,MAIN_BAS, #info on hb
                                           INTER_ID_cur,INTER_NEXT_cur, #interbasins number # outlet or not
                                           #INTER_ID_fut, INTER_NEXT_fut, 
                                           hab_nodams,hab_curdams,#hab_futdams, # occurrence
                                           geometry)
  }
    
  if(dams_used == "GrG2FH"){
  sp_ib_smaller <- sp_ib %>% dplyr::select(id_no,binomial,
                                           HYBAS_ID,MAIN_BAS, #info on hb
                                           INTER_ID_cur,INTER_NEXT_cur, #interbasins number # outlet or not
                                           INTER_ID_fut, INTER_NEXT_fut, 
                                           hab_nodams,hab_curdams,hab_futdams, # occurrence
                                           geometry)
  }
  
  sp_DI_sf <- sp_ib_smaller %>% st_as_sf()
  
  #load species point to sample sp_ib_smaller
  pts <- readRDS(paste0(dir_fishsuit,'proc/ssp/single_points/',id_no_sp,'.rds'))
  
  #skip species with no gridcell in pcrglob: nrow(pts)=0
  # do this later, the intersection is very slow I think this is the issue... or the linux is now very slow
  #if(nrow(pts)!=0){
  #  pts_info <- cbind(pts %>% dplyr::select(row_no,area),st_coordinates(pts)) %>% as.data.frame()
  #  colnames(pts_info) <- c("row_no","area","x", "y")
  #  
  #  #DI_points <- point.in.poly(pts,sp_DI_sf)@data %>% dplyr::distinct(row_no,.keep_all=TRUE) # in previous R version on linux
  #  DI_points <- st_intersection(pts,sp_DI_sf) %>% as.data.frame() %>% select(-geometry)%>%dplyr::distinct(row_no,.keep_all=TRUE) 
#  
#    #combine species points and polygons sp_DI_sf
#    DI_info <-  left_join( #used cbind in previous R version on linux
#      pts_info, #coordinates for raster
#      DI_points) # info on polygon they fall into
#    #sometimes point fall on hydrobasins borders, so remove duplicated of row_no (chooses 1 subbasin result)
#    cat('Write raster file \n')
#    # There may be NAs due to the transfer of HB8 to HB12, although this should be minimal
#    saveRDS(DI_info,paste0(dir_out_raster,id_no_sp,'.rds'))
#    #saveRDS(DI_points,paste0(dir_out_raster,id_no_sp,'_test.rds'))
#  }
}




# do it parrallel
parallel::mcmapply(build_tab,1:nrow(ids),mc.silent = TRUE,mc.cores = ncores)
#gives errors in some cores so use array for now
# could not find the cause of the error. it has something to do with point.in.poly and parallel running...
#array 1-10143
cat('Done! \n')

#detectCores()
#for testing
#parallel::mcmapply(build_tab,1:100,mc.silent = TRUE,mc.cores = ncores)

#test=array()
#er=c(1766,1768,1857,1866,1890,1918,2624,2632,2916,2917,3909,5478,5599,7062,7092,7587,8304,8955,9381,9382,9383,9384,9387,9566,9626)
#for(i in 1:length(er)){
#  id_no_sp=er[i]
#  pts <- readRDS(paste0(dir_fishsuit,'proc/ssp/single_points/',id_no_sp,'.rds'))
#  test[i]=nrow(pts)
#}

