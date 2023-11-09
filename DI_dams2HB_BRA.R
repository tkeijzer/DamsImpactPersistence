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


# for this dataset now filtered out 'minidams' (Tipo_1=="CGH") n=1221 
# large dams for brazil
BRA_l <- read_sf(file_bra_dams) %>%
  filter(Tipo_1=="UHE") %>%
  mutate(database = 'BRA_l') %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID,database) %>%
  st_transform(st_crs(4326))

BRA_s <- read_sf(file_bra_dams) %>%
  filter(Tipo_1=="PCH") %>%
  mutate(database = 'BRA_all') %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID,database) %>%
  st_transform(st_crs(4326))

BRA_all <- read_sf(file_bra_dams) %>%
  filter(Tipo_1=="UHE"|Tipo_1=="PCH") %>%
  mutate(database = 'BRA_all') %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID,database, Tipo_1) %>%
  st_transform(st_crs(4326))
st_write(BRA_all,'Dams_impact/proc/dams_BRA.gpkg', append=T)


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
hb_data <- foreach(i = c('sa'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
# extract only US HB units
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'Brazil',returnclass = 'sf'),"ESRI:54009"),
                     st_transform(hb_data,"ESRI:54009"),
                     sparse = T)
hb_data <- hb_data[sel[[1]],]
# pdf('check.pdf')
# plot(st_geometry(hb_data))
# dev.off()

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,BRA_all) #2572

sdams_hb_large <- st_intersection(hb_data,BRA_l) #496

sdams_hb_GDATonly <- st_intersection(hb_data,gdat) #4483

sdams_hb_GDATonly_large <- st_intersection(hb_data,gdat_large) #1907

# also combination of datasets
sdams_hb_bothdatasets <- rbind(sdams_hb,sdams_hb_GDATonly)
sdams_hb_bothdatasets_large <- rbind(sdams_hb_large,sdams_hb_GDATonly_large)

# save only the data frame with the intersected metadata
saveRDS(sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_REG.rds')
saveRDS(sdams_hb_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_REG_large.rds')
saveRDS(sdams_hb_GDATonly %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_GDATonly.rds')
saveRDS(sdams_hb_GDATonly_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_GDATonly_15m.rds')
saveRDS(sdams_hb_bothdatasets %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_BOTH.rds')
saveRDS(sdams_hb_bothdatasets_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_BRA_hydrobasins_BOTH_large.rds')


# 1909 HB units in regional dataset, 468 in regional dataset large dams
# 2933 HB units in GDAT , 1383 in gdat large dams
# bothdatasets: 4007 HB units, 1689 units large dams

# 2098 HB units in GDAT  but not in regional data
# 1074 HB units in regional data but not in GDAT

