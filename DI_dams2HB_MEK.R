# DI_dams2HB_MEK
# MEKONG dataset dams
# Tamara Keijzer
# April 2023


# Adapted from Valerio CI preprocessing

#setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")

#-------------------------------------------------------------------------
#>> Dams data

# also open GDAT database
gdat <- st_read(file_gdat_dams) #35,140, 3,360 geometries empty
gdat <- gdat %>% filter(!is.na(Long)) #31,780

MEK_all <- read.csv(file_mek_dams,stringsAsFactors = F) %>%
  as_tibble() %>%
  mutate(X = as.numeric(Long),Y=as.numeric(Lat)) %>%
  filter(!is.na(X)) %>%
  filter(!is.na(Y)) %>%
  filter(Status %in% c('OP','COMM','UNCON')) %>%
  mutate(ID = 1:nrow(.)) %>%
  st_as_sf(.,coords = c('X','Y'),crs=4326)%>%
  mutate(database = 'MEK_all') %>%
  select(ID,database,Height.m)
st_write(MEK_all,'Dams_impact/proc/dams_MEK.gpkg', append=T)

MEK_l <- MEK_all %>%
  filter(Height.m > 15) %>%
  mutate(database = 'MEK_l') %>%
  select(ID,database,Height.m)


MEK_s <- MEK_all %>%
  filter(ID %!in% MEK_l$ID) %>%
  mutate(database = 'MEK_s') %>%
  select(ID, database)

# MEK_l <- read_sf('data/OpenDevelopmentMekong/OpenDevelopmentMekong_all_dams.shp') %>%
#   filter(!Project_na %in% names_grand) %>% #24
#   filter(Height_m > 15) %>%
#   mutate(database = 'MEK_l') %>%
#   mutate(ID = 1:nrow(.)) %>%
#   select(ID,database) %>%
#   st_transform(st_crs(sdams_cur))
# 
# MEK_s <- read_sf('data/OpenDevelopmentMekong/OpenDevelopmentMekong_all_dams.shp') %>%
#   filter(!Project_na %in% names_grand) %>% #24
#   filter(Height_m <= 15) %>%
#   mutate(database = 'MEK_s') %>%
#   mutate(ID = 1:nrow(.)) %>%
#   select(ID,database) %>%
#   st_transform(st_crs(sdams_cur))



#filter GDAT on large dams >= 15 m
gdat_large <- gdat %>% filter(Height >= 15) #20,106

gdat <- gdat %>%
  mutate(database = 'GDAT') %>%
  select(ID = Feature_ID,database,geometry) 

gdat_large <- gdat_large %>%
  mutate(database = 'GDAT') %>%
  select(ID = Feature_ID,database,geometry) 


#-------------------------------------------------------------------------
#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('as'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp')) %>%
  filter(MAIN_BAS %in% c(4120017020,4120023810,4120023060)) #ID of Mekong, Irrawaddi, Salween

# pdf('check.pdf')
# plot(st_geometry(hb_data))
# dev.off()

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,MEK_all) #551

sdams_hb_large <- st_intersection(hb_data,MEK_l) #201

sdams_hb_GDATonly <- st_intersection(hb_data,gdat) #770

sdams_hb_GDATonly_large <- st_intersection(hb_data,gdat_large) #324


# also combination of datasets
sdams_hb_bothdatasets <- rbind(sdams_hb,sdams_hb_GDATonly)
sdams_hb_bothdatasets_large <- rbind(sdams_hb_large,sdams_hb_GDATonly_large)


# save only the data frame with the intersected metadata
saveRDS(sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_REG.rds')
saveRDS(sdams_hb_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_REG_large.rds')
saveRDS(sdams_hb_GDATonly %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_GDATonly.rds')
saveRDS(sdams_hb_GDATonly_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_GDATonly_15m.rds')
saveRDS(sdams_hb_bothdatasets %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_BOTH.rds')
saveRDS(sdams_hb_bothdatasets_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_MEK_hydrobasins_BOTH_large.rds')


# 451 HB units in regional dataset, 184 in regional dataset large dams
# 651 HB units in GDAT , 299 in gdat large dams
# bothdatasets: 671 HB units, 315 units large dams

# 220 HB units in GDAT  but not in regional data
# 20 HB units in regional data but not in GDAT
