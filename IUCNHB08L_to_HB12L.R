# IUCN data from HB08lakes to HB12lakes
# Tamara Keijzer
# October 2022

#setwd('~/CCDams_combo/R/')

#setup
library(dplyr);library(foreach);library(sf);library(spatialEco)
sf::sf_use_s2(FALSE)

#setwd('I:/tamara/CC_dams_R')

# load species data
cat('Reading species data..\n')
fish <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0(dir_sp08, '/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
fish <- fish[fish$presence %in% c(1,2),]  #only keep polygons indicating current presence


# Link table to HBL08withlakes, see which point HBL12withlakes (points) fall into the species range

# per continent on cluster
slurm_arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
c <- as.numeric(slurm_arrayid)
continent = c('af','ar','as','au','eu','gr','na','sa','si')
cont=continent[c]

# link to hydrobasins
# hydrobasins of the continent
hblakes <- read_sf(paste0(dir_hybas08,'/hybas_lake_',cont,'_lev08_v1c.shp')) 
valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,])# make valid so no self intersection errors
hblakes <- hblakes %>% select(HYBAS_ID)
hb_points <- st_point_on_surface(hblakes)
cat('loaded hb08..\n')


hblakes12 <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) 
valid <- st_is_valid(hblakes12) # which are valid geometries
hblakes12[valid==F,] <- st_make_valid(hblakes12[valid==F,])# make valid so no self intersection errors
hb12points <- st_point_on_surface(hblakes12) %>% select(HYBAS_ID)
cat('loaded hb12..\n')

cat('linking hb08 and hb12..\n')
tab=na.omit(point.in.poly(hb12points,hblakes)@data )
names(tab)[names(tab) == 'HYBAS_ID.x'] <- 'HYBAS_ID'
names(tab)[names(tab) == 'HYBAS_ID.y'] <- 'HYBAS_ID08'


cat('Fish data and hb..\n')
# inner join so species living on this continent remain and MAIN_BAS is added
fish_hb <- inner_join(fish,tab, by = c('hybas_id'='HYBAS_ID08')) 
cat('linked to hb..\n')

write.csv(fish_hb,paste0(dir_sp12,'/fish_hybas_hybas12withLakes_',cont,'.csv'),row.names = F)
#options(scipen=999)