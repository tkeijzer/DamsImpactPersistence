# produce DI tabs and figures polygons based


#setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")

dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))

#dir_fishsuit="I:/tamara/FishSuit/"
ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
traits <- read.csv(paste0(dir_fishsuit,"proc/species_traits_extra.csv")) # from fishsuit, species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no)


#tabs <- list.files(dir_out,pattern='.rds')
#ids <- as.character(
#  sapply(
#    list.files(dir_out,pattern='.rds'),function(x) strsplit(strsplit(x,'_')[[1]][1],'\\.')[[1]][1]
#  )
#)




if(dams_used == "GrG2FH"){
  ###########################
  ####### PER SPECIES #######
  ###########################
  
  
  
  damscen <- c("nodams","curdams","futdams")
  esh_calc <- function(i){ #,sn
    sp <- ids$id_no[i]
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <- traits %>% filter(binomial==name) %>% pull(diad)
    
    # only produce result if a species had length data, is not lentic_only, and is not marine or brackish
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
        # read the table
        t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams,hab_futdams, INTER_ID_cur,INTER_ID_fut)
        t <- t[complete.cases(t),] #remove rows that contain one or more NAs
        
        d <- data.frame(
          id_no = sp,#do all rpc*year combos cur/fut dams, and hist without/cur dams
          no_sbs = nrow(t), # this can be zero when e.g. all dams info is NA due to missing length data
          no_mb = if(nrow(t)!=0){length(unique(t$MAIN_BAS))}else{0},
          area_total_km2 = sum(t$SUB_AREA),
          ESHkm_curdams = NA,
          ESH_curdams = NA,
          ESHkm_futdams = NA,
          ESH_futdams = NA,
          fragmented_cur = NA,
          fragmented_fut = NA
        )
        
        #filter out rows of ts that are already excluded 
        t <- t[which(t$hab_nodams,1),] # filter out areas already too small
        
        if(nrow(t)==0){ #when areas are all too small without dams
          d$area_adjusted_km2 = 0 # add column in case nrow(t)=0
        }
        
        
        if(nrow(t)!=0){ 
          
            
            
            d$area_adjusted_km2 <- sum(t$SUB_AREA) # total area
            
            # for columns for all combinations dams impact and CC impact
            #sum the areas that are still suitable
            if(nrow(t[which(t$hab_curdams,1),'SUB_AREA'] !=0)){ # on linux sum doesnt work for things with zero rows
              d[,"ESHkm_curdams"] <- d$area_adjusted_km2-sum(t[which(t$hab_curdams,1),'SUB_AREA']) # total minus suitable
              d[,"ESH_curdams"] <- 100-sum(t[which(t$hab_curdams,1),'SUB_AREA'])/d$area_adjusted_km2*100
            }else{ # if all is false
              d[,"ESHkm_curdams"] <- d$area_adjusted_km2
              d[,"ESH_curdams"] <- 100
            }
            if(nrow(t[which(t$hab_futdams,1),'SUB_AREA'] !=0)){ # on linux sum doesnt work for things with zero rows
              d[,"ESHkm_futdams"] <- d$area_adjusted_km2-sum(t[which(t$hab_futdams,1),'SUB_AREA']) # total minus suitable
              d[,"ESH_futdams"] <- 100-sum(t[which(t$hab_futdams,1),'SUB_AREA'])/d$area_adjusted_km2*100
            }else{
              d[,"ESHkm_futdams"] <- d$area_adjusted_km2
              d[,"ESH_futdams"] <- 100}
            
            # TRUE/FALSE if the species range is fragmented by current or future dams
            # check for each main basin if subbasins fall into different interbasins (count number of different interbasins)
            # if all values are 1, species ranges are not fragmented within basins, if a value is larger than 1, species range is fragmented by dams
            count_fragments <- t %>% group_by(MAIN_BAS) %>% summarise(fr_cur = as.integer(n_distinct(INTER_ID_cur)), fr_fut = as.integer(n_distinct(INTER_ID_fut)) ) 
            d[,"fragmented_cur"] <- !all(count_fragments$fr_cur==1) # if all is 1 (TRUE) the range is not fragmented (so should become FALSE in fragm_cur)
            d[,"fragmented_fut"] <- !all(count_fragments$fr_fut==1) # if all is 1 (TRUE) the range is not fragmented (so should become FALSE in fragm_cur)
            
          }
          
        return(d)
    }
  }
  
  cat('Produce ESH tab ',dir_out_tabs,'\n')
  # calculate ESH for each scenario and year span
  ESH_tab <- 
    do.call('rbind',
            parallel::mcmapply(esh_calc,1:nrow(ids),SIMPLIFY = F,mc.cores = ncores)
    )
  
  row.names(ESH_tab) <- NULL
  ESH_tab2 <- inner_join(ids,ESH_tab)
  
  out <- paste0(dir_out_tabs,'ESHfragm_tabDI_fwonly.csv')
  write.csv(ESH_tab2,out,row.names = F)
  
  cat(' ESH tab successfully written to ',dir_out_tabs,'\n')
  cat(paste0(rep('-',30)),'\n\n')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  ####### SPATIALLY #######
  #########################
  
  
  #########################################################################################################################################################################################
  # HB subunit level
  #########################################################################################################################################################################################
  
  
  
  # SR per HydroBASINS subbasin unit
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  
  
  # hydrobasins lakes level 12 (for basins ids)
  cat('Reading hb12..\n')
  tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% as.data.frame() %>% dplyr::select(HYBAS_ID,MAIN_BAS)
    return(poly)
  }
  
  cat('Write SR tab hb subunits \n')
  
  #add columns for variables
  tab[,'occ'] <- NA
  tab[,'SRcurdams'] <- NA
  tab[,'SRfutdams'] <- NA
  
  # <<<<<<<< need to parallelize this
  for(i in 1:nrow(ids)){
    sp <- ids$id_no[i]
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams,hab_futdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
  
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
      #filter out rows of ts that are already excluded in th due to the quantile threshold
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)!=0){
      sb_nodams=which(tab$HYBAS_ID %in% t$HYBAS_ID)
      tab$occ[sb_nodams] <- apply(cbind(tab$occ[sb_nodams],rep(1,length(sb_nodams))),1,function(x) sum(x,na.rm=T))
      
      #which are there in current dams
      t <- t[which(t$hab_curdams,1),] 
      sb_curdams=which(tab$HYBAS_ID %in% t$HYBAS_ID)
      tab$SRcurdams[sb_curdams] <- apply(cbind(tab$SRcurdams[sb_curdams],rep(1,length(sb_curdams))),1,function(x) sum(x,na.rm=T))
      
      #which are there in future dams
      t <- t[which(t$hab_futdams,1),] # this works as it can only decrease
      sb_futdams=which(tab$HYBAS_ID %in% t$HYBAS_ID)
      tab$SRfutdams[sb_futdams] <- apply(cbind(tab$SRfutdams[sb_futdams],rep(1,length(sb_futdams))),1,function(x) sum(x,na.rm=T))
      
      
      }
    }
  }
  # if a fragment has species (not NA occurrence), and the SR (cur/fut) is still NA, 
  # it means that the fragment is not suitable for any of these species so it should be zero
  # however, this is still NA, so should change this to zero
  tab[which(is.na(tab$SRcurdams) & !is.na(tab$occ)),'SRcurdams'] <- 0
  tab[which(is.na(tab$SRfutdams) & !is.na(tab$occ)),'SRfutdams'] <- 0
  tab$Aspec_cur=tab$occ-tab$SRcurdams
  tab$Aspec_fut=tab$occ-tab$SRfutdams
  
  saveRDS(tab,paste0(dir_out_tabs,'SR_tabDI_fwonly.rds'))
  cat('successfully written SR hbunit to ',dir_out_tabs,'\n')
  
  
  
  
  #########################################################################################################################################################################################
  # INTERBASIN level
  #########################################################################################################################################################################################
  
  # SR per interbasin (fragment)
  
  # created the SR_cur and SR_fut outside linux (self intersection error)
  # see prep_DI_create_SR_ESH_tabs
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  
  
  cat('Read interbasins tabs..\n')
  
  SR_cur <- read_sf(paste0(dir_proc,dams_used,"_interbasins_cur.gpkg"))
  SR_fut <- read_sf(paste0(dir_proc,dams_used,"_interbasins_fut.gpkg"))
  
  # add species richness to these tables
  SR_cur[,'occ'] <- NA
  SR_cur[,'SRcurdams'] <- NA
  
  SR_fut[,'occ'] <- NA
  SR_fut[,'SRfutdams'] <- NA
  
  #add to occ tab when the species lives there, add to SRcurdams if they have TRUE for hab_curdams
  
  for(i in 1:nrow(ids)){
    sp <- ids$id_no[i]
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,INTER_HYBAS_ID_cur, INTER_HYBAS_ID_fut,hab_nodams,hab_curdams,hab_futdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <- traits %>% filter(binomial==name) %>% pull(diad)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
      #filter out rows of ts that are already excluded in th due to the quantile threshold
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)!=0){
        #current interbasins
        
        ib_nodams_cur=which(SR_cur$INTER_HYBAS_ID_cur %in% t$INTER_HYBAS_ID_cur) #which interbasins do the species occur in
        SR_cur$occ[ib_nodams_cur] <- apply(cbind(SR_cur$occ[ib_nodams_cur],rep(1,length(ib_nodams_cur))),1,function(x) sum(x,na.rm=T))
        
        #which are there in current dams
        t <- t[which(t$hab_curdams,1),] 
        ib_curdams=which(SR_cur$INTER_HYBAS_ID_cur %in% t$INTER_HYBAS_ID_cur)
        SR_cur$SRcurdams[ib_curdams] <- apply(cbind(SR_cur$SRcurdams[ib_curdams],rep(1,length(ib_curdams))),1,function(x) sum(x,na.rm=T))
        
        #load species data again as things are removed
        t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,INTER_HYBAS_ID_cur, INTER_HYBAS_ID_fut,hab_nodams,hab_curdams,hab_futdams)
        t <- t[which(t$hab_nodams,1),] # filter out areas already too small
        
        #future interbasins
        ib_nodams_fut=which(SR_fut$INTER_HYBAS_ID_fut %in% t$INTER_HYBAS_ID_fut) #which interbasins do the species ocfut in
        SR_fut$occ[ib_nodams_fut] <- apply(cbind(SR_fut$occ[ib_nodams_fut],rep(1,length(ib_nodams_fut))),1,function(x) sum(x,na.rm=T))
        
        #which are there in future dams
        t <- t[which(t$hab_futdams,1),] 
        ib_futdams=which(SR_fut$INTER_HYBAS_ID_fut %in% t$INTER_HYBAS_ID_fut)
        SR_fut$SRfutdams[ib_futdams] <- apply(cbind(SR_fut$SRfutdams[ib_futdams],rep(1,length(ib_futdams))),1,function(x) sum(x,na.rm=T))
        
        
      }
    }
  }
  
  # if a fragment has species (not NA occurrence), and the SR (cur/fut) is still NA, 
  # it means that the fragment is not suitable for any of these species so it should be zero
  # however, this is still NA, so should change this to zero
  SR_cur[which(is.na(SR_cur$SRcurdams) & !is.na(SR_cur$occ)),'SRcurdams'] <- 0
  SR_fut[which(is.na(SR_fut$SRfutdams) & !is.na(SR_fut$occ)),'SRfutdams'] <- 0
  
  tab$Aspec_cur=tab$occ-tab$SRcurdams
  tab$Aspec_fut=tab$occ-tab$SRfutdams
  
  saveRDS(SR_cur %>% as.data.frame() %>% dplyr::select(-geom), paste0(dir_out_tabs,'SRinterbasins_cur_fwonly.rds'))
  saveRDS(SR_fut %>% as.data.frame() %>% dplyr::select(-geom), paste0(dir_out_tabs,'SRinterbasins_fut_fwonly.rds'))
  
  SR_cur$PAFcur <- (SR_cur$occ-SR_cur$SRcurdams)/SR_cur$occ
  SR_fut$PAFfut <- (SR_fut$occ-SR_fut$SRfutdams)/SR_fut$occ
  
  st_write(SR_cur,paste0(dir_out_tabs,"PAFinterbasins_cur_fwonly.gpkg"), append=F)
  st_write(SR_fut,paste0(dir_out_tabs,"PAFinterbasins_fut_fwonly.gpkg"), append=F)
  
  cat('SR tab successfully written to ',dir_out_tabs,'\n')
  
  
  cat('Saved SR per fragment.. \n')
  
  
  
  #########################################################################################################################################################################################
  # MAIN BASIN level
  #########################################################################################################################################################################################
  
  
  cat('Per main basin calculations.. \n')
  
  # made the tab with main basins outside linux
  # see prep_DI_create_SR_ESH_tabs
  
  tab_mb <- read_sf(paste0(dir_proc,dams_used,"_mainbasinslayer.gpkg"))
  
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
    diadromous <- traits %>% filter(binomial==name) %>% pull(diad)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
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
  
  
  cat('Write files main basin results.. \n')
  
  
  saveRDS(tab_mb%>% as.data.frame() %>% dplyr::select(-geom),paste0(dir_out_tabs,'mbres_fwonly.rds'))
  st_write(tab_mb,paste0(dir_out_tabs,"mbres_fwonly.gpkg"), append=F)
  cat('successfully written to ',dir_out_tabs,'\n')
}
























if(dams_used == "GDAT" || dams_used == "GDATall"){ # everything the same but no future stuff
  ###########################
  ####### PER SPECIES #######
  ###########################
  
  
  
  damscen <- c("nodams","curdams","futdams")
  esh_calc <- function(i){ #,sn
    sp <- ids$id_no[i]
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <- traits %>% filter(binomial==name) %>% pull(diad)
    
    # only produce result if a species had length data, is not lentic_only, and is not marine or brackish
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
      # read the table
      t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
      t <- t[complete.cases(t),] #remove rows that contain one or more NAs
      
      d <- data.frame(
        id_no = sp,#do all rpc*year combos cur/fut dams, and hist without/cur dams
        no_sbs = nrow(t), # this can be zero when e.g. all dams info is NA due to missing length data
        no_mb = if(nrow(t)!=0){length(unique(t$MAIN_BAS))}else{0},
        area_total_km2 = sum(t$SUB_AREA),
        ESHkm_curdams = NA,
        ESH_curdams = NA,
        fragmented_cur = NA
      )
      
      #filter out rows of ts that are already excluded 
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)==0){ #when areas are all too small without dams
        d$area_adjusted_km2 = 0 # add column in case nrow(t)=0
      }
      
      
      if(nrow(t)!=0){ 
        
        
        
        d$area_adjusted_km2 <- sum(t$SUB_AREA) # total area
        
        # for columns for all combinations dams impact and CC impact
        #sum the areas that are still suitable
        if(nrow(t[which(t$hab_curdams,1),'SUB_AREA'] !=0)){ # on linux sum doesnt work for things with zero rows
          d[,"ESHkm_curdams"] <- d$area_adjusted_km2-sum(t[which(t$hab_curdams,1),'SUB_AREA']) # total minus suitable
          d[,"ESH_curdams"] <- 100-sum(t[which(t$hab_curdams,1),'SUB_AREA'])/d$area_adjusted_km2*100
        }else{ # if all is false
          d[,"ESHkm_curdams"] <- d$area_adjusted_km2
          d[,"ESH_curdams"] <- 100
        }
        
        
        # TRUE/FALSE if the species range is fragmented by current or future dams
        # check for each main basin if subbasins fall into different interbasins (count number of different interbasins)
        # if all values are 1, species ranges are not fragmented within basins, if a value is larger than 1, species range is fragmented by dams
        count_fragments <- t %>% group_by(MAIN_BAS) %>% summarise(fr_cur = as.integer(n_distinct(INTER_ID_cur)) ) 
        d[,"fragmented_cur"] <- !all(count_fragments$fr_cur==1) # if all is 1 (TRUE) the range is not fragmented (so should become FALSE in fragm_cur)
        
      }
      
      return(d)
    }
  }
  
  cat('Produce ESH tab ',dir_out_tabs,'\n')
  # calculate ESH for each scenario and year span
  ESH_tab <- 
    do.call('rbind',
            parallel::mcmapply(esh_calc,1:nrow(ids),SIMPLIFY = F,mc.cores = ncores)
    )
  
  row.names(ESH_tab) <- NULL
  ESH_tab2 <- inner_join(ids,ESH_tab)
  
  out <- paste0(dir_out_tabs,'ESHfragm_tabDI_fwonly.csv')
  write.csv(ESH_tab2,out,row.names = F)
  
  cat(' ESH tab successfully written to ',dir_out_tabs,'\n')
  cat(paste0(rep('-',30)),'\n\n')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  ####### SPATIALLY #######
  #########################
  
  
  #########################################################################################################################################################################################
  # HB subunit level
  #########################################################################################################################################################################################
  
  
  
  # SR per HydroBASINS subbasin unit
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  
  
  # hydrobasins lakes level 12 (for basins ids)
  cat('Reading hb12..\n')
  tab <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% as.data.frame() %>% dplyr::select(HYBAS_ID,MAIN_BAS)
    return(poly)
  }
  
  cat('Write SR tab hb subunits \n')
  
  #add columns for variables
  tab[,'occ'] <- NA
  tab[,'SRcurdams'] <- NA
  
  # <<<<<<<< need to parallelize this
  for(i in 1:nrow(ids)){
    sp <- ids$id_no[i]
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
      #filter out rows of ts that are already excluded in th due to the quantile threshold
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)!=0){
        sb_nodams=which(tab$HYBAS_ID %in% t$HYBAS_ID)
        tab$occ[sb_nodams] <- apply(cbind(tab$occ[sb_nodams],rep(1,length(sb_nodams))),1,function(x) sum(x,na.rm=T))
        
        #which are there in current dams
        t <- t[which(t$hab_curdams,1),] 
        sb_curdams=which(tab$HYBAS_ID %in% t$HYBAS_ID)
        tab$SRcurdams[sb_curdams] <- apply(cbind(tab$SRcurdams[sb_curdams],rep(1,length(sb_curdams))),1,function(x) sum(x,na.rm=T))
        
       
        
        
      }
    }
  }
  # if a fragment has species (not NA occurrence), and the SR (cur/fut) is still NA, 
  # it means that the fragment is not suitable for any of these species so it should be zero
  # however, this is still NA, so should change this to zero
  tab[which(is.na(tab$SRcurdams) & !is.na(tab$occ)),'SRcurdams'] <- 0
  tab$Aspec_cur=tab$occ-tab$SRcurdams
  
  saveRDS(tab,paste0(dir_out_tabs,'SR_tabDI_fwonly.rds'))
  cat('successfully written SR hbunit to ',dir_out_tabs,'\n')
  
  
  
  
  #########################################################################################################################################################################################
  # INTERBASIN level
  #########################################################################################################################################################################################
  
  # SR per interbasin (fragment)
  
  # created the SR_cur and SR_fut outside linux (self intersection error)
  # first create a template (and sf) with interbasins current and future
  continent = c('af','ar','as','au','eu','gr','na','sa','si')
  # see prep_DI_create_SR_ESH_tabs
  
  cat('Read interbasins tabs..\n')
  
  SR_cur <- read_sf(paste0(dir_proc,dams_used,"_interbasins_cur.gpkg"))
  
  # add species richness to these tables
  SR_cur[,'occ'] <- NA
  SR_cur[,'SRcurdams'] <- NA
 
  #add to occ tab when the species lives there, add to SRcurdams if they have TRUE for hab_curdams
  
  for(i in 1:nrow(ids)){
    sp <- ids$id_no[i]
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,INTER_HYBAS_ID_cur,hab_nodams,hab_curdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
      #filter out rows of ts that are already excluded in th due to the quantile threshold
      t <- t[which(t$hab_nodams,1),] # filter out areas already too small
      
      if(nrow(t)!=0){
        #current interbasins
        
        ib_nodams_cur=which(SR_cur$INTER_HYBAS_ID_cur %in% t$INTER_HYBAS_ID_cur) #which interbasins do the species occur in
        SR_cur$occ[ib_nodams_cur] <- apply(cbind(SR_cur$occ[ib_nodams_cur],rep(1,length(ib_nodams_cur))),1,function(x) sum(x,na.rm=T))
        
        #which are there in current dams
        t <- t[which(t$hab_curdams,1),] 
        ib_curdams=which(SR_cur$INTER_HYBAS_ID_cur %in% t$INTER_HYBAS_ID_cur)
        SR_cur$SRcurdams[ib_curdams] <- apply(cbind(SR_cur$SRcurdams[ib_curdams],rep(1,length(ib_curdams))),1,function(x) sum(x,na.rm=T))
        
        
        
      }
    }
  }
  
  # if a fragment has species (not NA occurrence), and the SR (cur/fut) is still NA, 
  # it means that the fragment is not suitable for any of these species so it should be zero
  # however, this is still NA, so should change this to zero
  SR_cur[which(is.na(SR_cur$SRcurdams) & !is.na(SR_cur$occ)),'SRcurdams'] <- 0
  
  tab$Aspec_cur=tab$occ-tab$SRcurdams
  
  saveRDS(SR_cur %>% as.data.frame() %>% dplyr::select(-geom), paste0(dir_out_tabs,'SRinterbasins_cur_fwonly.rds'))
  
  SR_cur$PAFcur <- (SR_cur$occ-SR_cur$SRcurdams)/SR_cur$occ
  
  st_write(SR_cur,paste0(dir_out_tabs,"PAFinterbasins_cur_fwonly.gpkg"), append=F)
  
  
  
  cat('SR tab successfully written to ',dir_out_tabs,'\n')
  
  cat('Saved SR per fragment.. \n')
  
  
  
  #########################################################################################################################################################################################
  # MAIN BASIN level
  #########################################################################################################################################################################################
  
  
  cat('Per main basin calculations.. \n')
  
  # made the tab with main basins outside linux
  # see prep_DI_create_SR_ESH_tabs
  
  tab_mb <- read_sf(paste0(dir_proc,dams_used,"_mainbasinslayer.gpkg"))
  
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
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams)
    
    name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
    # get species traits
    marine <- traits %>% filter(binomial==name) %>% pull(Marine)
    brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
    length <- traits %>% filter(binomial==name) %>% pull(Length)
    lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
    diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
    
    if(!is.na(length) & lentic_only==0 & brackish==0 & marine==0 & diadromous == 'f'){
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
  tab_mb <- tab_mb %>% dplyr::mutate(dplyr::across(.cols=(ncol(tab_mb)-1):ncol(tab_mb),~ifelse(is.nan(.), NA, .)))
  
  tab_mb$Pextinct_spec_cur=tab_mb$occ-tab_mb$occ_cur
  
  
  cat('Write files main basin results.. \n')
  
  
  saveRDS(tab_mb%>% as.data.frame() %>% dplyr::select(-geom),paste0(dir_out_tabs,'mbres_fwonly.rds'))
  st_write(tab_mb,paste0(dir_out_tabs,"mbres_fwonly.gpkg"), append=F)
  cat('successfully written to ',dir_out_tabs,'\n')
}
