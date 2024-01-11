# compare species richness according to IUCN and according to Tedesco in Tedesco basins.
# create SI fig S1
#

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")
library(sf); library(dplyr); library(ggplot2); library(raster);library(foreach)

# load species data
cat('Reading species data..\n')
fish <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0('Fishdata/Fish_hybas/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}

# Link table to HBL08withlakes, see which point HBL8withlakes (points) fall into the species range

# per continent on cluster
continent = c('af','ar','as','au','eu','gr','na','sa','si')


# link to hydrobasins
# hydrobasins of the continent
cat('Reading hb data..\n')
hblakes <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas08,'/hybas_lake_',cont,'_lev08_v1c.shp')) %>% st_point_on_surface()
  return(poly)
}
valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,]) # make valid so no self intersection errors

# inner join 
fish_hb <- inner_join(fish,hblakes, by = c('hybas_id'='HYBAS_ID')) # mind some species with hb12lakes or hb8nolakes refs are not included

class(fish_hb)
fish_hb_sf <- fish_hb %>% st_as_sf()
sp <- fish_hb
names(sp)[names(sp) == 'hybas_id'] <- 'HYBAS_ID'
hbas <- hblakes

# sample Tedesco basins
bas <- read_sf('Tedesco/Basin042017_3119.shp') %>% mutate(id = 1:nrow(.)) 
bas_ras <-  fasterize::fasterize(sf = bas,raster = raster(res = 1/12), field = 'id')
hbas$ws <- extract(bas_ras,hbas)
hbas_tab <- hbas %>% as_tibble() %>% dplyr::select(-geometry) %>% filter(!is.na(ws))

tab <- left_join(sp,hbas_tab %>% dplyr::select(HYBAS_ID,ws)) %>%
  group_by(ws) %>%
  summarize(sp_no = length(unique(binomial)))

tab <- left_join(tab,bas %>% as_tibble() %>% dplyr::select(BasinName,ws = id,-geometry))

# merge with tedesco table
# use occurrences table
ted_occ <- read.csv('Tedesco/Occurrence_Table.csv',sep = ';') %>%
  as_tibble() %>%
  rename(BasinName = X1.Basin.Name) %>%
  group_by(BasinName) %>%
  summarise(no_sp_ted = length(unique(X5.Fishbase.Species.Code))) %>% 
  left_join(bas %>% as_tibble() %>% dplyr::select(BasinName,ws = id,-geometry))

tab <- inner_join(tab,ted_occ) #2194 basins

# calculate difference

# load and merge with the shapefile for plotting
ws <- inner_join(bas,tab) %>%
  mutate(
    ratio.perc = sp_no/no_sp_ted * 100,
    diff.perc = (sp_no - no_sp_ted)/no_sp_ted * 100,
    diff = sp_no - no_sp_ted
  )

# for visualization
ws$ratio.perc.corr <- ws$ratio.perc
ws$ratio.perc.corr[ws$ratio.perc > 100] <- 110
ws$ratio.perc.corr <- as.integer(round(ws$ratio.perc.corr,0))

ws.melt <- reshape2::melt(ws %>% as_tibble() %>% dplyr::select(-geometry),measure.vars = c('sp_no','no_sp_ted'))
levels(ws.melt$variable) <- c('IUCN (2022)','Tedesco et al. (2017)')

ws.melt <- left_join(bas,ws.melt,by = 'BasinName') %>% filter(!is.na(value))
# base layers
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- rnaturalearth::ne_countries(returnclass = "sf")[,1] %>%
  st_transform(crs_custom)

url="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_wgs84_bounding_box.zip"
temp=tempfile()
temp2=tempfile()
download.file(url,temp)
unzip(zipfile=temp,exdir=temp2)
bb <- read_sf(file.path(temp2,"ne_110m_wgs84_bounding_box.shp")) %>% st_transform(crs_custom)
unlink(c(temp,temp2))


url="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_graticules_30.zip"
temp=tempfile()
temp2=tempfile()
download.file(url,temp)
unzip(zipfile=temp,exdir=temp2)
graticules <- read_sf(file.path(temp2,"ne_110m_graticules_30.shp")) %>% st_transform(crs_custom)
unlink(c(temp,temp2))

cols=viridis::plasma(100)

library(ggplot2); library(viridis)
p1 <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = ws.melt %>% st_transform(crs_custom),aes(fill = value),alpha = 1,lwd = NA) +
  #facet_grid(scenario~.)+
  scale_fill_viridis_c(
    trans = 'log10',direction = -1
  )+
  theme_minimal() +
  facet_grid(variable~.) +
  theme(text = element_text(size = 12),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(6,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 12),
        legend.title = element_blank()
  )



p2 <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = ws %>% st_transform(crs_custom),aes(fill = ratio.perc.corr),alpha = 1,lwd = NA) +
  scale_fill_viridis_c(
    breaks = c(seq(0,100,20),110),
    labels = c(seq(0,80,20),'>100',''),
    limits = c(0,110),
    option = 'C'
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(6,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 12),
        legend.title = element_blank()
  )

library(ggpubr)
figure2=ggarrange(p1,p2, 
                  labels = c("(a) ", "(b)"),
                  font.label = list(size = 12),
                  ncol = 1, nrow = 2,
                  heights = c(2,1.2),
                  legend="bottom"
)
dev.off()
ggsave('Dams_impact/figs/FigS1_compare_SR_Tedesco_IUCNdata.jpg',figure2, height = 200, width = 178,
       dpi = 1000,units = 'mm')


#saveRDS(ws,'Dams_impact/proc/compare_SR_tedesco.rds')
#write.csv(as.data.frame(ws)[,-which(colnames(ws) == 'geometry')],'Dams_impact/proc/compare_SR_tedesco.csv',row.names = F)
