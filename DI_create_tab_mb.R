# produce DI tabs and figures polygons based


#setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")

dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))

#dir_fishsuit="I:/tamara/FishSuit/"
ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
traits <- read.csv(paste0(dir_fishsuit,"proc/species_traits_extra.csv")) # from fishsuit, species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no)


if(dams_used == "GrG2FH"){
#########################################################################################################################################################################################
# MAIN BASIN level
#########################################################################################################################################################################################


cat('Per main basin calculations.. \n')

# made the tab with main basins outside linux

#cat('Reading hb12..\n')
#hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
#  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
#  return(poly)
#}

#tab_mb <- hb12  %>% group_by(MAIN_BAS) %>% 
#  summarise(MAIN_BAS=min(MAIN_BAS),
#            mb_area=sum(SUB_AREA))

# indicate whether the main basins are fragmented (now or future)
#ib_cur <- read_sf(paste0(dir_out_tabs,"interbasins_cur.gpkg")) %>% as.data.frame()
#ib_fut <- read_sf(paste0(dir_out_tabs,"interbasins_fut.gpkg")) %>% as.data.frame()

#tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)
#tab_mb$frag_fut <- ifelse(tab_mb$MAIN_BAS %in% ib_fut$MAIN_BAS,T,F)
#st_write(tab_mb,paste0(dir_out_tabs,"mainbasinslayer.gpkg"), append=F)

tab_mb <- read_sf(paste0(dir_out_tabs,"mainbasinslayer.gpkg"))

#add columns for variables
tab_mb[,'occ'] <- NA
tab_mb[,'Rtot'] <- 0 #total range of fish
tab_mb[,'Aspec_cur'] <- 0 #affected species
tab_mb[,'Arange_cur'] <- 0 # total affected range
tab_mb[,'Aspec_fut'] <- 0 # affected species
tab_mb[,'Arange_fut'] <- 0 # total affected range
tab_mb[,'occ_cur'] <- NA
tab_mb[,'occ_fut'] <- NA



# add to occ tab when the species lives there
# sum area (by basin) and add
# then only keep FALSE hab_cur
#     if nrow(t)!=0
#               add+1 to basins remaining
#               sum area by main bas and add
# then only keep FALSE hab_fut
#     if nrow(t)!=0
#               add+1 to basins remaining
#               sum area by main bas and add

#nrow(ids
for(i in 1:nrow(ids)){
  sp <- ids$id_no[i]
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams,hab_futdams)
  
  name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
  # get species traits
  marine <- traits %>% filter(binomial==name) %>% pull(Marine)
  brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
  length <- traits %>% filter(binomial==name) %>% pull(Length)
  lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
  
  if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0){
    #filter out rows of ts that are already excluded in th due to the quantile threshold
    t <- t[which(t$hab_nodams,1),] # filter out areas already too small
    
    if(nrow(t)!=0){
      #species occurrence in basin
      mb_occ=which(tab_mb$MAIN_BAS %in% t$MAIN_BAS) #which rows (tab_mb) do the species occur in
      tab_mb$occ[mb_occ] <- apply(cbind(tab_mb$occ[mb_occ],rep(1,length(mb_occ))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
      
      # total species range in basin
      rangearea <- t %>% group_by(MAIN_BAS)%>% summarise(ind_range=sum(SUB_AREA))# total area species per main basin
      # add to basin total
      # dont know how to do this nicely so I'll just left join and add the columns
      tab_mb <- left_join(tab_mb,rangearea)
      tab_mb$Rtot <- rowSums(tab_mb[,c("Rtot", "ind_range")]%>% as.data.frame()%>%dplyr::select(-geom), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
      tab_mb$ind_range=NULL #remove this row 
      
      #species richness after accounting for dams
      tcurtrue=t[which(t$hab_curdams==T),]
      mb_occ_cur=which(tab_mb$MAIN_BAS %in% tcurtrue$MAIN_BAS) #which rows (tab_mb) do the species occur in
      tab_mb$occ_cur[mb_occ_cur] <- apply(cbind(tab_mb$occ_cur[mb_occ_cur],rep(1,length(mb_occ_cur))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
      
      #species richness after accounting for future dams
      tfuttrue=t[which(t$hab_futdams==T),]
      mb_occ_fut=which(tab_mb$MAIN_BAS %in% tfuttrue$MAIN_BAS) #which rows (tab_mb) do the species occur in
      tab_mb$occ_fut[mb_occ_fut] <- apply(cbind(tab_mb$occ_fut[mb_occ_fut],rep(1,length(mb_occ_fut))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
      
      
      #subbasins affected with current dams
      tcur <- t[which(t$hab_curdams==F),]
      if(nrow(tcur)!=0){ # if nothing false add nothing (no affected area or species)
        #affected species
        AS_cur=which(tab_mb$MAIN_BAS %in% tcur$MAIN_BAS)
        tab_mb$Aspec_cur[AS_cur] <- apply(cbind(tab_mb$Aspec_cur[AS_cur],rep(1,length(AS_cur))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
        
        #affected area
        affrange<- tcur %>% group_by(MAIN_BAS)%>% summarise(aff_range=sum(SUB_AREA))# total area species per main basin
        tab_mb <- left_join(tab_mb,affrange)
        tab_mb$Arange_cur <- rowSums(tab_mb[,c("Arange_cur", "aff_range")]%>% as.data.frame()%>%dplyr::select(-geom), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
        tab_mb$aff_range=NULL #remove this row 
      }
      
      
      #subbasins affected with future dams
      tfut <- t[which(t$hab_futdams==F),] 
      if(nrow(tfut)!=0){ # if nothing false add nothing (no affected area or species)
        #affected species
        AS_fut=which(tab_mb$MAIN_BAS %in% tfut$MAIN_BAS)
        tab_mb$Aspec_fut[AS_fut] <- apply(cbind(tab_mb$Aspec_fut[AS_fut],rep(1,length(AS_fut))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
        
        #affected area
        affrange <- tfut %>% group_by(MAIN_BAS)%>% summarise(aff_range=sum(SUB_AREA))# total area species per main basin
        tab_mb <- left_join(tab_mb,affrange)
        tab_mb$Arange_fut <- rowSums(tab_mb[,c("Arange_fut", "aff_range")]%>% as.data.frame()%>%dplyr::select(-geom), na.rm=TRUE)#CHECK IF THIS NOW WORKS...
        tab_mb$aff_range=NULL #remove this row 
      }
      #summary(tab_mb)
    }
  }
}



tab_mb$PAFspec_cur=1-(tab_mb$occ-tab_mb$Aspec_cur)/tab_mb$occ
tab_mb$PAFspec_fut=1-(tab_mb$occ-tab_mb$Aspec_fut)/tab_mb$occ
tab_mb$PAFrange_cur=1-(tab_mb$Rtot-tab_mb$Arange_cur)/tab_mb$Rtot
tab_mb$PAFrange_fut=1-(tab_mb$Rtot-tab_mb$Arange_fut)/tab_mb$Rtot
tab_mb <- tab_mb %>% dplyr::mutate(dplyr::across(.cols=(ncol(tab_mb)-3):ncol(tab_mb),~ifelse(is.nan(.), NA, .)))

tab_mb$Pextinct_spec_cur=tab_mb$occ-tab_mb$occ_cur
tab_mb$Pextinct_spec_fut=tab_mb$occ-tab_mb$occ_fut

cat('Write files.. \n')


saveRDS(tab_mb%>% as.data.frame() %>% dplyr::select(-geom),paste0(dir_out_tabs,'mbres_fwonly.rds'))
st_write(tab_mb,paste0(dir_out_tabs,"mbres_fwonly.gpkg"), append=F)
cat('successfully written to ',dir_out_tabs,'\n')
}


if(dams_used == "GDAT"){
  
  #########################################################################################################################################################################################
  # MAIN BASIN level
  #########################################################################################################################################################################################
  
  
  cat('Per main basin calculations.. \n')
  
  # made the tab with main basins outside linux
  
  #cat('Reading hb12..\n')
  #hb12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  #  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA) # keep geometry to create sf of interbasins
  #  return(poly)
  #}
  
  #tab_mb <- hb12  %>% group_by(MAIN_BAS) %>% 
  #  summarise(MAIN_BAS=min(MAIN_BAS),
  #            mb_area=sum(SUB_AREA))
  
  # indicate whether the main basins are fragmented (now or future)
  #ib_cur <- read_sf(paste0(dir_out_tabs,"interbasins_cur.gpkg")) %>% as.data.frame()
  #ib_fut <- read_sf(paste0(dir_out_tabs,"interbasins_fut.gpkg")) %>% as.data.frame()
  
  #tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)
  #tab_mb$frag_fut <- ifelse(tab_mb$MAIN_BAS %in% ib_fut$MAIN_BAS,T,F)
  #st_write(tab_mb,paste0(dir_out_tabs,"mainbasinslayer.gpkg"), append=F)
  
  tab_mb <- read_sf(paste0(dir_out_tabs,"mainbasinslayer.gpkg"))
  
  #add columns for variables
  tab_mb[,'occ'] <- NA
  tab_mb[,'Rtot'] <- 0 #total range of fish
  tab_mb[,'Aspec_cur'] <- 0 #affected species
  tab_mb[,'Arange_cur'] <- 0 # total affected range
  tab_mb[,'occ_cur'] <- NA
  
  
  
  # add to occ tab when the species lives there
  # sum area (by basin) and add
  # then only keep FALSE hab_cur
  #     if nrow(t)!=0
  #               add+1 to basins remaining
  #               sum area by main bas and add
  
  #nrow(ids
  for(i in 1:nrow(ids)){
    sp <- ids$id_no[i]
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams,hab_futdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0){
      #filter out rows of ts that are already excluded in th due to the quantile threshold
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)!=0){
        #species occurrence in basin
        mb_occ=which(tab_mb$MAIN_BAS %in% t$MAIN_BAS) #which rows (tab_mb) do the species occur in
        tab_mb$occ[mb_occ] <- apply(cbind(tab_mb$occ[mb_occ],rep(1,length(mb_occ))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
        
        # total species range in basin
        rangearea <- t %>% group_by(MAIN_BAS)%>% summarise(ind_range=sum(SUB_AREA))# total area species per main basin
        # add to basin total
        # dont know how to do this nicely so I'll just left join and add the columns
        tab_mb <- left_join(tab_mb,rangearea)
        tab_mb$Rtot <- rowSums(tab_mb[,c("Rtot", "ind_range")]%>% as.data.frame()%>%dplyr::select(-geom), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
        tab_mb$ind_range=NULL #remove this row 
        
        
        #species richness after accounting for dams
        tcurtrue=t[which(t$hab_curdams==T),]
        mb_occ_cur=which(tab_mb$MAIN_BAS %in% tcurtrue$MAIN_BAS) #which rows (tab_mb) do the species occur in
        tab_mb$occ_cur[mb_occ_cur] <- apply(cbind(tab_mb$occ_cur[mb_occ_cur],rep(1,length(mb_occ_cur))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
        
        
        
        
        #subbasins affected with current dams
        tcur <- t[which(t$hab_curdams==F),]
        if(nrow(tcur)!=0){ # if nothing false add nothing (no affected area or species)
          #affected species
          AS_cur=which(tab_mb$MAIN_BAS %in% tcur$MAIN_BAS)
          tab_mb$Aspec_cur[AS_cur] <- apply(cbind(tab_mb$Aspec_cur[AS_cur],rep(1,length(AS_cur))),1,function(x) sum(x,na.rm=T)) # add a one to these rows
          
          #affected area
          affrange<- tcur %>% group_by(MAIN_BAS)%>% summarise(aff_range=sum(SUB_AREA))# total area species per main basin
          tab_mb <- left_join(tab_mb,affrange)
          tab_mb$Arange_cur <- rowSums(tab_mb[,c("Arange_cur", "aff_range")]%>% as.data.frame()%>%dplyr::select(-geom), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
          tab_mb$aff_range=NULL #remove this row 
        }
        
        
        
      }
    }
  }
  
  tab_mb$PAFspec_cur=1-(tab_mb$occ-tab_mb$Aspec_cur)/tab_mb$occ
  tab_mb$PAFrange_cur=1-(tab_mb$Rtot-tab_mb$Arange_cur)/tab_mb$Rtot
  tab_mb <- tab_mb %>% dplyr::mutate(dplyr::across(.cols=(ncol(tab_mb)-3):ncol(tab_mb),~ifelse(is.nan(.), NA, .)))
  
  tab_mb$Pextinct_spec_cur=tab_mb$occ-tab_mb$occ_cur
  
  
  cat('Write files.. \n')
  
  
  saveRDS(tab_mb%>% as.data.frame() %>% dplyr::select(-geom),paste0(dir_out_tabs,'mbres_fwonly.rds'))
  st_write(tab_mb,paste0(dir_out_tabs,"mbres_fwonly.gpkg"), append=F)
  cat('successfully written to ',dir_out_tabs,'\n')
}