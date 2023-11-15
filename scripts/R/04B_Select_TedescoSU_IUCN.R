#Tedesco selection of species data in main basins
# only select fish data in basin if Tedesco says the species is present there.
# April 2023
# Tamara Keijzer

# can be done locally or on cluster
#SBATCH --nodes=1
#SBATCH --mem=30G
#SBATCH --array 1-9

local =TRUE # TRUE or FALSE

if(local==TRUE){
  local_model_folder="I:/tamara/CC_dams_R"
  setwd(local_model_folder)
}

source("scripts/SETTINGS_Dams_impact.R")

library(dplyr);library(sf);library(foreach);library(sp);library(spatialEco)
sf::sf_use_s2(FALSE)

# data
# IUCN fishdata
cat('Reading species data..\n')
fish <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0('Fishdata/Fish_hybas/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
fish <- fish[fish$presence %in% c(1,2),] 
cols_fish <- colnames(fish)

#validated names IUCN
fb <- read.csv("Fishdata/Fishbase/iucn_hybas_fb.csv")
fish <- merge(fish,fb,all.x=T)

#hydrobasins
# all
cat('Reading hb data..\n')


# per continent
slurm_arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
c <- as.numeric(slurm_arrayid)
continent = c('af','ar','as','au','eu','gr','na','sa','si')
cont=continent[c]
hblakes <- read_sf(paste0(dir_hybas08,'/hybas_lake_',cont,'_lev08_v1c.shp')) 

valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,]) # make valid so no self intersection errors

# fish range data to points
cat('Fish range data to points..\n')
fish_hb <- inner_join(fish,hblakes, by = c('hybas_id'='HYBAS_ID')) %>% st_as_sf() # contains points in continent
fish_hb_points <- st_point_on_surface(fish_hb)


# Tedesco basins sf
cat('Read Tedesco data..\n')
Tbas <- read_sf("Tedesco/Basin042017_3119.shp")
valid <- st_is_valid(Tbas) # which are valid geometries
Tbas[valid==F,] <- st_make_valid(Tbas[valid==F,])# make valid so no self intersection errors

# Tedesco occurrence
# Tedesco validated names
Tsp <- read.csv("Tedesco/Occurrence_table_fb21names.csv") # created in Tedesco/Add_fb21_names.R


# select only things this continent
# link the points with the polygon they fall in
tab=point.in.poly(fish_hb_points,Tbas)@data  # remove species not in tedesco basins
# point.in.poly is faster than st intersects, though on the new cluster it does not work

#rm(fish_hb,fish_hb_points,poly,hblakes)

basins_cont=na.omit(unique(tab$BasinName))
cat('Nr Tedesco basins:..\n')
print(length(basins_cont))

# info on species non-native or extinct

ted_occ <- Tsp %>%
  as_tibble() %>%
  rename(BasinName = X1.Basin.Name) %>%
  select(BasinName,binomial = X6.Fishbase.Valid.Species.Name, Status = X3.Native.Exotic.Status)

su_occ <- read.csv('Tedesco/Su_et_al_Science_table.csv') %>%
  as_tibble() %>%
  rename(BasinName = Basin.Name) %>%
  select(BasinName,binomial = Species.Latin.Name, Status)
su_occ$Status[su_occ$Status == 'extinction/extirpation'] <- 'extinct'
su_occ$Status[su_occ$Status == 'introduction'] <- 'exotic'

exotic <- ted_occ %>%
  bind_rows(su_occ) %>%
  distinct() %>%
  filter(Status == 'exotic') %>%
  select(BasinName,binomial) %>%
  mutate(binomial  = gsub('\\.',' ',binomial)) %>%
  arrange(BasinName)

extinct <- su_occ %>%
  distinct() %>%
  filter(Status == 'extinct') %>%
  select(BasinName,binomial) %>%
  mutate(binomial  = gsub('\\.',' ',binomial)) %>%
  arrange(BasinName)


# make table

if(local==TRUE){
  #locally or ...
  cat('foreach..\n')
  # for each tedesco basin
  Bas_tab <- foreach(i = 1:length(basins_cont),.combine='rbind') %do% {
    basin <- basins_cont[i]
    #basin_outline <- Tbas %>% filter(BasinName==basin)
    # get the occurrence list for the basin
    Tsp_basin <- Tsp %>% filter(X1.Basin.Name==basin)
    
    
    tab_basin <- tab %>% filter(BasinName==basin)
    #remove extinct or exotic species from occurrence list
    e <- exotic[as.character(exotic$BasinName) == basin,] %>% pull(binomial)
    e <- unique(c(e,extinct[as.character(extinct$BasinName) == basin,] %>% pull(binomial)))
    Tsp_basin <- Tsp_basin[!Tsp_basin$fb_name %in% e,]
    
    # filter on species in occurrence list
    sel_fish <- tab_basin %>% filter(fb_name %in% Tsp_basin$fb_name)
    
    # only keep basic columns (create new rds fish range dataset)
    #sel_fish <- sel_fish %>% as.data.frame() %>% select(-geometry)
    sel_fish <- sel_fish[,cols_fish]
    return(sel_fish)
    
  }
}



if(local==FALSE){
  # or parallel
  create_tab <- function(i){
    basin <- basins_cont[i]
    #basin_outline <- Tbas %>% filter(BasinName==basin)
    # get the occurrence list for the basin
    Tsp_basin <- Tsp %>% filter(X1.Basin.Name==basin)
    
    tab_basin <- tab %>% filter(BasinName==basin)
    # filter on species in occurrence list
    sel_fish <- tab_basin %>% filter(fb_name %in% Tsp_basin$fb_name)
    
    # only keep basic columns (create new rds fish range dataset)
    #sel_fish <- sel_fish %>% as.data.frame() %>% select(-geometry)
    sel_fish <- sel_fish[,cols_fish]
    return(sel_fish)
    
  }
  
  Bas_tab <- do.call('rbind',parallel::mclapply(1:nrow(Tbas),get_ib_ids,mc.silent = TRUE,mc.cores = 10))
}


test=tab %>% filter(!is.na(BasinName))
cat('NR observations and species before selection:..\n')
print(nrow(test))
print(length(unique(test$binomial)))

cat('NR observations and species after selection:..\n')
print(nrow(Bas_tab))
print(length(unique(Bas_tab$binomial)))

saveRDS(Bas_tab,paste0("Tedesco/comp/Select_TedescoSu_IUCNspec_",cont,".rds"))




#only Tedesco occurrence selection

# some stats
sum(T_fb %in% IUCN_fb) #8,814 matches
sum(IUCN_fb %in% T_fb)




