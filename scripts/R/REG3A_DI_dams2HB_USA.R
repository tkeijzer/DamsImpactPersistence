# DI_dams2HB_USA
# NIR dataset dams
# Tamara Keijzer
# April 2023
# Adapted from Valerio CI preprocessing https://github.com/vbarbarossa/connectfish

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")

#-------------------------------------------------------------------------
#>> Dams data

# also open GDAT database
gdat <- st_read(file_gdat_dams) #35,140, 3,360 geometries empty
gdat <- gdat %>% filter(!is.na(Long)) #31,780

# open NID data
NID <- read_sf(file_nir_dams) #91,609 dams

# purposes to include
purp <- strsplit(unique(NID$purposeIds),';') %>% do.call('c',.) %>% unique(.) %>% .[!. %in% c('Fire Protection, Stock, Or Small Fish Pond', 'Debris Control', 'Tailings') & !is.na(.)]

NID <- NID %>%
  # filter records based on purp
  filter(lapply(strsplit(purposeIds,';'),function(x) sum(x %in% purp) > 0 ) %>% do.call('c',.)) %>% #74,310 dams left
  # create column with height in meters (from feet)
  mutate(height_m = as.numeric(nidHeight)*0.3048) %>%
  # create column with storage in cubic meters (from acres-feet)
  mutate(storage_m3 = as.numeric(nidStorage)*1233.48) %>%
  # convert to sf
  st_as_sf(.,coords = c('longitude','latitude'),crs=4326)
st_write(NID,'Dams_impact/proc/dams_USA.gpkg', append=T)

NID_large <- NID %>%
  filter(height_m >= 15)

NID_cols <- NID %>% 
  mutate(database = 'NID_all') %>%
  select(ID = nidId,database,geom) #74310

NID_large_cols <- NID_large %>%
  mutate(database = 'NID_l') %>%
  select(ID = nidId,database,geom) #6125

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
hb_data <- foreach(i = c('na','ar'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
# extract only US HB units
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf'),"ESRI:54009"),
                     st_transform(hb_data,"ESRI:54009"),
                     sparse = T)
hb_data <- hb_data[sel[[1]],]
# pdf('check.pdf')
# plot(st_geometry(hb_data))
# dev.off()

# intersect dams and hydrobasins
sdams_hb <- st_intersection(hb_data,NID_cols)

sdams_hb_large <- st_intersection(hb_data,NID_large_cols)

sdams_hb_GDATonly <- st_intersection(hb_data,gdat)

sdams_hb_GDATonly_large <- st_intersection(hb_data,gdat_large)

# also combination of datasets
sdams_hb_bothdatasets <- rbind(sdams_hb,sdams_hb_GDATonly)
sdams_hb_bothdatasets_large <- rbind(sdams_hb_large2,sdams_hb_GDATonly_large)

# save only the data frame with the intersected metadata
saveRDS(sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_REG.rds')
saveRDS(sdams_hb_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_REG_large.rds')
saveRDS(sdams_hb_GDATonly %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_GDATonly.rds')
saveRDS(sdams_hb_GDATonly_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_GDATonly_15m.rds')
saveRDS(sdams_hb_bothdatasets %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_BOTH.rds')
saveRDS(sdams_hb_bothdatasets_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct(),'Dams_impact/proc/dams_USA_hydrobasins_BOTH_large.rds')

#stats
b=sdams_hb_bothdatasets %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()
b_l=sdams_hb_bothdatasets_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()
r=sdams_hb %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()
r_l=sdams_hb_large2 %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()
g=sdams_hb_GDATonly %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()
g_l=sdams_hb_GDATonly_large %>% as_tibble() %>% select(HYBAS_ID) %>% distinct()

table(g$HYBAS_ID %in% r$HYBAS_ID)
table(r$HYBAS_ID %in% g$HYBAS_ID)

# 23478 HB units in regional dataset, 4392 in regional dataset large dams
# 5156 HB units in GDAT , 4775 in gdat large dams
# bothdatasets: 24228 HB units, 5899 units large dams

# 750 HB units in GDAT  but not in regional data
# 19072 HB units in regional data but not in GDAT
