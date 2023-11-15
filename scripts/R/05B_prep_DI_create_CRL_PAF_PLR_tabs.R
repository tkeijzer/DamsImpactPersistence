# prep for DI_create_SR_ESH_tabs.R


# do this outside of cluster otherwise errors




setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")



if(dams_used == "GrG2FH"){
# per interbasin stuff

# created the SR_cur and SR_fut outside linux (self intersection error)
# first create a template (and sf) with interbasins current and future
continent = c('af','ar','as','au','eu','gr','na','sa','si')
tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- readRDS(paste0(dir_proc,'InterBasins_tab_GrG2FH_',cont,'.rds')) 
  return(poly)
}

cat('Reading hb12..\n')
hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
  return(poly)
}

tab_hb <- left_join(tab,hb12) %>% st_as_sf()

#inter_HYBAS_ID_cur/fut is the hybas_id of the most downstream subbasin in the interbasin/fragment
#create a template with unique interbasin ids (use inter_hybas_id), for current and future seperately

#test <- tab_hb[1:4000,] %>% filter(INTER_HYBAS_ID_cur!=0)

#some basins dont have dams (in current and future), they will have INTER_HYBAS_ID zero, we'd like to remove these from the selection
SR_cur <- tab_hb %>% filter(INTER_HYBAS_ID_cur!=0) %>% group_by(INTER_HYBAS_ID_cur) %>%  
  summarise(INTER_NEXT_cur=min(INTER_NEXT_cur), 
            MAIN_BAS=min(MAIN_BAS),
            sbnr = n(),
            ib_area=sum(SUB_AREA))  # unique interbasin id and indication whether its the outlet (then next is zero), total area, and geometry - current dams 
SR_cur$outlet=NA
SR_cur$outlet[which(SR_cur$INTER_NEXT_cur !=0)] <- 0 # no outlet
SR_cur$outlet[which(SR_cur$INTER_NEXT_cur ==0)] <- 1 # yes outlet
SR_cur$INTER_NEXT_cur=NULL


SR_fut <- tab_hb %>% filter(INTER_HYBAS_ID_fut!=0) %>% group_by(INTER_HYBAS_ID_fut) %>% 
  summarise(INTER_NEXT_fut=min(INTER_NEXT_fut), 
            MAIN_BAS=min(MAIN_BAS),
            sbnr = n(),
            ib_area=sum(SUB_AREA))  # unique interbasin id and indication whether its the outlet (then next is zero), total area, and geometry - future dams 
SR_fut$outlet=NA
SR_fut$outlet[which(SR_fut$INTER_NEXT_fut !=0)] <- 0 # no outlet
SR_fut$outlet[which(SR_fut$INTER_NEXT_fut ==0)] <- 1 # yes outlet
SR_fut$INTER_NEXT_fut=NULL

st_write(SR_cur,paste0(dir_proc,dams_used,"_interbasins_cur.gpkg"), append=F)
st_write(SR_fut,paste0(dir_proc,dams_used,"_interbasins_fut.gpkg"), append=F)


# made the tab with main basins outside linux

#cat('Reading hb12..\n')
hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
  return(poly)
}

tab_mb <- hb12  %>% group_by(MAIN_BAS) %>% 
  summarise(MAIN_BAS=min(MAIN_BAS),
            mb_area=sum(SUB_AREA))
# indicate whether the main basins are fragmented (now or future)
ib_cur <- read_sf(paste0(dir_proc,dams_used,"_interbasins_cur.gpkg")) %>% as.data.frame()
ib_fut <- read_sf(paste0(dir_proc,dams_used,"_interbasins_fut.gpkg")) %>% as.data.frame()

tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)
tab_mb$frag_fut <- ifelse(tab_mb$MAIN_BAS %in% ib_fut$MAIN_BAS,T,F)
st_write(tab_mb,paste0(dir_proc,dams_used,"_mainbasinslayer.gpkg"), append=F)
}






if(dams_used == "GDAT" ){
  # per interbasin stuff
  
  # created the SR_cur and SR_fut outside linux (self intersection error)
  # first create a template (and sf) with interbasins current and future
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- readRDS(paste0(dir_proc,'InterBasins_tab_GDAT_',cont,'.rds')) 
    return(poly)
  }
  
  cat('Reading hb12..\n')
  hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
    return(poly)
  }
  
  tab_hb <- left_join(tab,hb12) %>% st_as_sf()
  
  #inter_HYBAS_ID_cur/fut is the hybas_id of the most downstream subbasin in the interbasin/fragment
  #create a template with unique interbasin ids (use inter_hybas_id), for current and future seperately
  
  #test <- tab_hb[1:4000,] %>% filter(INTER_HYBAS_ID_cur!=0)
  
  #some basins dont have dams (in current and future), they will have INTER_HYBAS_ID zero, we'd like to remove these from the selection
  SR_cur <- tab_hb %>% filter(INTER_HYBAS_ID_cur!=0) %>% group_by(INTER_HYBAS_ID_cur) %>%  
    summarise(INTER_NEXT_cur=min(INTER_NEXT_cur), 
              MAIN_BAS=min(MAIN_BAS),
              sbnr = n(),
              ib_area=sum(SUB_AREA))  # unique interbasin id and indication whether its the outlet (then next is zero), total area, and geometry - current dams 
  SR_cur$outlet=NA
  SR_cur$outlet[which(SR_cur$INTER_NEXT_cur !=0)] <- 0 # no outlet
  SR_cur$outlet[which(SR_cur$INTER_NEXT_cur ==0)] <- 1 # yes outlet
  SR_cur$INTER_NEXT_cur=NULL
  
  
  st_write(SR_cur,paste0(dir_proc,dams_used,"_interbasins_cur.gpkg"), append=F)
  
  
  # made the tab with main basins outside linux
  
  #cat('Reading hb12..\n')
  hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
    return(poly)
  }
  
  tab_mb <- hb12  %>% group_by(MAIN_BAS) %>% 
    summarise(MAIN_BAS=min(MAIN_BAS),
              mb_area=sum(SUB_AREA))
  # indicate whether the main basins are fragmented (now or future)
  ib_cur <- read_sf(paste0(dir_proc,dams_used,"_interbasins_cur.gpkg")) %>% as.data.frame()
  
  tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)
  st_write(tab_mb,paste0(dir_proc,dams_used,"_mainbasinslayer.gpkg"), append=F)
}


if( dams_used == "GDATall"){
  # per interbasin stuff
  
  # created the SR_cur and SR_fut outside linux (self intersection error)
  # first create a template (and sf) with interbasins current and future
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- readRDS(paste0(dir_proc,'InterBasins_tab_GDATall_',cont,'.rds')) 
    return(poly)
  }
  
  cat('Reading hb12..\n')
  hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
    return(poly)
  }
  
  tab_hb <- left_join(tab,hb12) %>% st_as_sf()
  
  #inter_HYBAS_ID_cur/fut is the hybas_id of the most downstream subbasin in the interbasin/fragment
  #create a template with unique interbasin ids (use inter_hybas_id), for current and future seperately
  
  #test <- tab_hb[1:4000,] %>% filter(INTER_HYBAS_ID_cur!=0)
  
  #some basins dont have dams (in current and future), they will have INTER_HYBAS_ID zero, we'd like to remove these from the selection
  SR_cur <- tab_hb %>% filter(INTER_HYBAS_ID_cur!=0) %>% group_by(INTER_HYBAS_ID_cur) %>%  
    summarise(INTER_NEXT_cur=min(INTER_NEXT_cur), 
              MAIN_BAS=min(MAIN_BAS),
              sbnr = n(),
              ib_area=sum(SUB_AREA))  # unique interbasin id and indication whether its the outlet (then next is zero), total area, and geometry - current dams 
  SR_cur$outlet=NA
  SR_cur$outlet[which(SR_cur$INTER_NEXT_cur !=0)] <- 0 # no outlet
  SR_cur$outlet[which(SR_cur$INTER_NEXT_cur ==0)] <- 1 # yes outlet
  SR_cur$INTER_NEXT_cur=NULL
  
  
  st_write(SR_cur,paste0(dir_proc,dams_used,"_interbasins_cur.gpkg"), append=F)
  
  
  # made the tab with main basins outside linux
  
  #cat('Reading hb12..\n')
  hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
    return(poly)
  }
  
  tab_mb <- hb12  %>% group_by(MAIN_BAS) %>% 
    summarise(MAIN_BAS=min(MAIN_BAS),
              mb_area=sum(SUB_AREA))
  # indicate whether the main basins are fragmented (now or future)
  ib_cur <- read_sf(paste0(dir_proc,dams_used,"_interbasins_cur.gpkg")) %>% as.data.frame()
  
  tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)
  st_write(tab_mb,paste0(dir_proc,dams_used,"_mainbasinslayer.gpkg"), append=F)
}

