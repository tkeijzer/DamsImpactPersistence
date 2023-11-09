# Tedesco compare with IUCN hydrobasins data
# Tamara Keijzer
# October 2022


#set-up
#setwd("~/CCDams_combo/R")

library(dplyr);library(sf);library(foreach);library(sp);library(spatialEco)
sf::sf_use_s2(FALSE)


#load data
#Tedesco basins
Tbas <- read_sf("Tedesco/Basin042017_3119.shp")
valid <- st_is_valid(Tbas) # which are valid geometries
Tbas[valid==F,] <- st_make_valid(Tbas[valid==F,])# make valid so no self intersection errors
#Tedesco species
Tsp <- read.csv("Tedesco/Occurrence_table_fb21names.csv")

#IUCN data
cat('Reading species data..\n')
fish <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0('Fishdata/Fish_hybas/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
fish <- fish[fish$presence %in% c(1,2),] 

# link to fishbase
cat('link to fishbase..\n')
fb <- read.csv("Fishdata/Fishbase/iucn_hybas_fb.csv")
fish <- merge(fish,fb,all.x=T)

#make IUCN data spatial

# per continent on cluster
slurm_arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
c <- as.numeric(slurm_arrayid)
continent = c('af','ar','as','au','eu','gr','na','sa','si')
cont=continent[c]

# link to hydrobasins
# hydrobasins of the continent
#hblakes <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
#  poly <- read_sf(paste0('Hybas_lake/hybas_lake_',cont,'_lev08_v1c.shp'))
#  return(poly)
#}
hblakes <- read_sf(paste0('Hybas_lake/hybas_lake_',cont,'_lev08_v1c.shp')) 
valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,])# make valid so no self intersection errors
cat('loaded hb08..\n')

fish_hb <- inner_join(fish,hblakes, by = c('hybas_id'='HYBAS_ID')) %>% st_as_sf() # contains points in continent
fish_hb_points <- st_point_on_surface(fish_hb)
#write_sf(fish_hb_points,"Tedesco/test_speciespoints.gpkg")

# link the points with the polygon they fall in
tab=point.in.poly(fish_hb_points,Tbas)@data  # remove species not in tedesco basins

basins_cont=unique(tab$BasinName)

#split into the tedesco basins
Bas_tab <- foreach(i = 1:length(basins_cont),.combine='rbind') %do% {
  bas <- tab %>% filter(BasinName == basins_cont[i])
  iucn_species <- unique(bas$fb_name)
  tedesco_species <- Tsp %>% filter(X1.Basin.Name == basins_cont[i]) %>% distinct(fb_name)
  tedesco_species <- tedesco_species$fb_name
  bas_res <- data.frame(T_BasinName =basins_cont[i],
                        Tsp_nr=length(tedesco_species),
                        IUCN_nr=length(iucn_species),
                        match_nr=sum(tedesco_species %in% iucn_species),
                        completeness=sum(tedesco_species %in% iucn_species)/length(tedesco_species)
  )
  
  return(bas_res)
}

write.csv(Bas_tab,paste0("Tedesco/comp/Comparenr_Tedesco_IUCNspec_table_",cont,".csv"),row.names = F)

#make Bas_tab sf
Bas_tab_sf <- left_join(Bas_tab,Tbas, by=c("T_BasinName" = "BasinName")) %>% st_as_sf()

# select main basins
mb<- hblakes %>% group_by(MAIN_BAS) %>% summarize(do_union=T)
mb_points <- st_point_on_surface(mb)
mb_Tbas <- point.in.poly(mb_points,Bas_tab_sf)@data

write.csv(mb_Tbas,paste0("Tedesco/comp/HBmb_select_percsp_Tedesco_",cont,".csv"),row.names = F)

#hbl8choose <- left_join(mb_Tbas,Bas_tab)
