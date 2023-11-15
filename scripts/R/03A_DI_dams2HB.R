# Dams to HybasLakes12
# GoNEXUS
# Tamara Keijzer
# October 2022
# updated April 2023

# Adapted from Valerio CI preprocessing georeference_dams.R

#setwd("I:/tamara/CC_dams_R")
source("scripts/SETTINGS_Dams_impact.R")





#-------------------------------------------------------------------------
#>> Dams data



if(dams_used == "GrG2FH"){

#GRanD v1.3
grand <- st_read(file_grand_dams) #7,320
# GOOD2 unsnapped
good2 <- st_read(file_good2_dams) #32,613

dams_cur <- rbind(
  cbind(data.frame(ID = grand$GRAND_ID,database = 'GRanD'),st_coordinates(grand)),
  cbind(data.frame(ID = good2$DAM_ID,database = 'GOOD2'),st_coordinates(good2))
)


# convert to sf spatial points
sdams_cur <- st_as_sf(dams_cur,coords = c('X','Y'),crs=4326)

st_write(sdams_cur,'Dams_impact/proc/dams_current.gpkg')

# future dams from Zarfl
dams_fut <- read.csv(file_frhed_dams)
# convert to spatial points
sdams_fut <- st_as_sf(dams_fut,coords = c('Lon_2016','Lat_2016'),crs=4326)

st_write(sdams_fut,'Dams_impact/proc/dams_future.gpkg')

#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))

# intersect dams and hydrobasins
sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
sdams_fut_hb <- st_intersection(hb_data,sdams_fut)


# save only the data frame with the intersected metadata
# this only contains unique hydrobasins ids
saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_current_hydrobasins.rds')
saveRDS(sdams_fut_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_future_hydrobasins.rds')

#>> count

# > nrow(sdams_cur) #original no. dams
# [1] 39933
# > nrow(sdams_cur_hb) #no. dams referenced on hybas12
# [1] 39912
# > length(unique(sdams_cur_hb$HYBAS_ID)) #no. of dammed hybasins
# [1] 22247

# > nrow(sdams_fut)
# [1] 3682
# > nrow(sdams_fut_hb)
# [1] 3681
# > length(unique(sdams_fut_hb$HYBAS_ID))
# [1] 2927


# # visual check
# hb_sel <- hb_data[hb_data$HYBAS_ID %in% sdams_cur_hb$HYBAS_ID,]
# write_sf(hb_sel,'Dams_impact/visual_check/hb_sel_check.gpkg')
# write_sf(sdams_cur_hb,'Dams_impact/visual_check/dams_cur_hb_check.gpkg')
}






if(dams_used == "GDAT"){
  
  #GDAT v1
  gdat <- st_read(file_gdat_dams) #35,140, 3,360 geometries empty
  #filter on large dams >= 15 m
  gdat <- gdat %>% filter(Height >= 15) #21,199
  gdat <- gdat %>% filter(!is.na(Long)) #20,106
  #dams_cur <- cbind(gdat,st_coordinates(gdat))
  dams_cur <- gdat
  
  
  # convert to sf spatial points
  sdams_cur <- st_as_sf(dams_cur,coords = c('Long','Lat'),crs=4326)
  
  st_write(sdams_cur,'Dams_impact/proc/dams_gdat_15m.gpkg')
  
  #>> Hydrobasins data
  
  # read hydrobasins data
  hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
  
  # intersect dams and hydrobasins
  sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
  
  
  # save only the data frame with the intersected metadata
  # this only contains unique hydrobasins ids
  saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_gdat_15m_hydrobasins.rds')
  
  #>> count
  
  # > nrow(sdams_cur) #original no. dams
  # [1] 20106
  # > nrow(sdams_cur_hb) #no. dams referenced on hybas12
  # [1] 20061
  # > length(unique(sdams_cur_hb$HYBAS_ID)) #no. of dammed hybasins
  # [1] 14499
  
}


if(dams_used == "GDATall"){
  
  #GDAT v1
  gdat <- st_read(file_gdat_dams) #35,140, 3,360 geometries empty
  gdat <- gdat %>% filter(!is.na(Long)) #31,780
  #dams_cur <- cbind(gdat,st_coordinates(gdat))
  dams_cur <- gdat
  
  
  # convert to sf spatial points
  sdams_cur <- st_as_sf(dams_cur,coords = c('Long','Lat'),crs=4326)
  
  st_write(sdams_cur,'Dams_impact/proc/dams_gdat_all.gpkg')
  
  #>> Hydrobasins data
  
  # read hydrobasins data
  hb_data <- foreach(i = c('af','ar','as','au','eu','gr','na','sa','si'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
  
  # intersect dams and hydrobasins
  sdams_cur_hb <- st_intersection(hb_data,sdams_cur)
  
  
  # save only the data frame with the intersected metadata
  # this only contains unique hydrobasins ids
  saveRDS(sdams_cur_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_gdat_all_hydrobasins.rds')
  
  #>> count
  
  # > nrow(sdams_cur) #original no. dams
  # [1] 31780
  # > nrow(sdams_cur_hb) #no. dams referenced on hybas12
  # [1] 31717
  # > length(unique(sdams_cur_hb$HYBAS_ID)) #no. of dammed hybasins
  # [1] 22077
  
}



