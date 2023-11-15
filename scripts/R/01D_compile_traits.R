# Code adapted from FishSuit


#always connect to local folder first
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")

libinv(c('sf','dplyr','rfishbase','foreach'))
# set fishbase version
#options(FISHBASE_VERSION="19.04")
options(FISHBASE_VERSION="21.06")


cat('Reading species data..\n')
fish <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0('Fishdata/Fish_hybas/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
fish <- fish[fish$presence %in% c(1,2),]  #only keep polygons indicating current presence


# per continent on cluster
continent = c('af','ar','as','au','eu','gr','na','sa','si')

# link to hydrobasins
# hydrobasins of the continent
cat('Reading hb data..\n')
hblakes <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0('Hybas_lake/hybas_lake_',cont,'_lev08_v1c.shp'))
  return(poly)
}
valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,]) # make valid so no self intersection errors
#hb_points <- st_centroid_within_poly(hblakes)
# inner join 
fish_hb <- inner_join(fish,hblakes, by = c('hybas_id'='HYBAS_ID'))


cat('Reading fb data..\n')
infb <- read.csv("Fishdata/Fishbase/iucn_hybas_fb21names.csv")
infb <- infb[,1:2]%>% filter(!is.na(fb_name))
cat('Compiling filtered dataset..\n')


# filter out species without validated names against fishbase
sp_filtered <- fish_hb %>% as.data.frame() %>% dplyr::select(binomial)%>%
  filter(binomial %in% infb$binomial) 


# create an integer id
id_tab <- data.frame(id_no = 1:length(sp_filtered$binomial %>% unique), binomial = sp_filtered$binomial %>% unique)
df <- id_tab




# LOTIC-LENTIC HABITAT -----------------------------------------------------------------------------------------------

fbval <- read.csv("Fishdata/Fishbase/iucn_hybas_fb21names.csv")
#THESE ARE THE SPECIES WITH PRESENCE 1 OR 2 SO LESS THAN 11726
df <- left_join(df,fbval)

# use both IUCN and fishbase data to assign lotic-lentic habitat type
hiucn <- read.csv('Fishdata/iucn_habitat_type.csv') %>%
  as_tibble %>%
  dplyr::select(binomial,lotic,lotic_brackish,lentic)
# include estuaries (didnt do this when producing iucn_habitat_type see dams folder)
hiucn$lotic = hiucn$lotic + hiucn$lotic_brackish
hiucn$lotic[which(hiucn$lotic==2)] <-1
hiucn$lotic_brackish=NULL
# hiucn$lentic_only <- 0
# hiucn$lentic_only[hiucn$lentic == 1 & (hiucn$lotic == 0 | is.na(hiucn$lotic))] <- 1

hfishbase <- ecology(df$fb_name) %>%
  dplyr::select(fb_name = Species, lotic = Stream, lentic = Lakes) %>%
  distinct(fb_name,.keep_all = T)
hfishbase[hfishbase == -1] <- 1
hfishbase <- left_join(hfishbase,fbval)

hab <- bind_rows(
  # make first piece of table based on iucn data 
  data.frame(
    binomial = df$binomial[df$binomial %in% hiucn$binomial]
  ) %>% as_tibble() %>%
    left_join(hiucn)
  ,
  # and remaining species based on fishbase
  data.frame(
    binomial = df$binomial[!df$binomial %in% hiucn$binomial]
  ) %>% as_tibble() %>%
    left_join(hfishbase %>% dplyr::select(binomial, lotic,lentic))
) %>%
  arrange(binomial)
hab$lentic_only <- 0
hab$lentic_only[hab$lentic == 1 & (hab$lotic == 0 | is.na(hab$lotic))] <- 1

sum(hab$lentic_only) #1160 # T 1091

# FISHBASE TRAITS ----------------------------------------------------------------------------------------------------
# length, trophic, commercial importance, marine
fb1 <- species(df$fb_name) %>%
  dplyr::select(fb_name = Species, Length = Length, importance = Importance, Marine = Saltwater, Freshwater = Fresh, Brackish = Brack, Migration = AnaCat) %>%
  distinct(fb_name,.keep_all = T)

fb1$Migration <- as.factor(fb1$Migration) 
levels(fb1$Migration) <- c(NA,rep('Diad.',5),'Non.','Ocea.','Ocea.','Pota.','Pota.') #check this

# assign diadromous-non diadromous category
fb1$diad <- 'f'
fb1$diad[fb1$fb_name %in% fb1$fb_name[fb1$Migration == 'Diad.']] <- 't'

fb2 <- ecology(df$fb_name) %>%
  dplyr::select(fb_name = Species, FoodTroph) %>%
  distinct(fb_name,.keep_all = T)
  

fb2$FoodTroph[is.na(fb2$FoodTroph)] <- 0
fb2$foodtrophcat[fb2$FoodTroph > 0] <- 'Herbi.'
fb2$foodtrophcat[fb2$FoodTroph > 2.19 & fb2$FoodTroph <= 2.79] <- 'Omni.'
fb2$foodtrophcat[fb2$FoodTroph > 2.79] <- 'Carni.'
fb2$FoodTroph[fb2$FoodTroph == 0] <- NA


fb <- inner_join(fb1,fb2)

apply(fb,2,function(x) sum(is.na(x)))
# > nrow(fb)
# [1] 12934
# > apply(fb,2,function(x) sum(is.na(x)))
# binomial       length   importance    FoodTroph foodtrophcat 
# 0          753         9960         9346         9346 

# Tam 2021 version iucn hb
#nrow(fb)
#[1] 10091 unique names
#> apply(fb,2,function(x) sum(is.na(x)))
#fb_name       Length   importance       Marine   Freshwater     Brackish    Migration         diad    FoodTroph 
#0          674         7721            0            0            0         8739            0         7987 
#foodtrophcat 
#7987 


# IUCN code ------------------------------------------------------------------------------------------------------
#iucn data for threat status**************************************************
token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'
library(rredlist)
iucn_code <- foreach(ts = c("DD", "LC", "NT", "VU", "EN","CR", "EW", "EX", "LRlc", "LRnt", "LRcd"),.combine = 'rbind') %do%{
  t <- rl_sp_category(ts,key = token)$result %>% 
    as_tibble() %>%
    mutate(code = ts) %>%
    dplyr::select(binomial = scientific_name,code)
  return(t)
} %>% arrange(binomial)

# MERGE TABLES --------------------------------------------------------------------------------------

tab <- df %>%
  left_join(hab) %>%
  left_join(fb) %>%
  left_join(iucn_code)

write.csv(tab,'Fishdata/species_traits_extra.csv',row.names = F)

