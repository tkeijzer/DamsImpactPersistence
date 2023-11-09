# regional datasets get the tables out
# combine scripts DI_interbasins, DI_interb_speciesrange, (prep_)DI_create_SR_ESH_tabs
# Tamara Keijzer
# April 2023


#setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")

g <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) # for different dams dataset

damsdata=c("REG","REG_large","GDATonly","GDATonly_15m","BOTH","BOTH_large")[g]
# output paths

# first do interbasins thing in general folder, then put ESH and SR tabs in specific MVA folders
dir_(paste0(dir_proc,"USA/"))
dir_out_ib <- dir_(paste0(dir_proc,"USA/", damsdata,"/"))
dir_(paste0(dir_proc,"USA/", damsdata,"/model_DI_occurrence/"))
dir_out <- dir_(paste0(dir_proc,"USA/", damsdata,"/model_DI_occurrence/",mva_type,"/"))
dir_out_tabs <- dir_(paste0(dir_proc,"USA/", damsdata,"/model_DI_occurrence/",mva_type,"/tabs/"))


#>> Hydrobasins data

# read hydrobasins data
hb_data <- foreach(i = c('na','ar'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
# extract only US HB units
sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf'),"ESRI:54009"), # outside linux use "ESRI:54009"
                     st_transform(hb_data,"ESRI:54009"), # outside linux use "ESRI:54009"
                     sparse = T)
hb_data <- hb_data[sel[[1]],] #93060 subbasins

valid <- st_is_valid(hb_data) # which are valid geometries
hb_data[valid==F,] <- st_make_valid(hb_data[valid==F,])# make valid so no self intersection errors


cat('\nCompiling main basin area..\n')
# add basin area
main_bas_area <- hb_data %>% as.data.frame %>%
  dplyr::select(HYBAS_ID,MAIN_BAS,SUB_AREA) %>%
  group_by(MAIN_BAS) %>%
  summarize(MAIN_BAS_AREA = sum(SUB_AREA))

hb_data <- left_join(hb_data,main_bas_area,by='MAIN_BAS')

#vector with all unique main basin ids
main_bas=unique(hb_data$MAIN_BAS)



#>> Dams data
cat('Load dams data..\n')
dams_NID <- readRDS(paste0('Dams_impact/proc/dams_NIR_hydrobasins_',damsdata,'.rds'))

dams_cur=dams_NID




### ----------------------- DI_interbasins ------------------------------------------------------------------
cat('DI_interbasins..\n')
# create interbasins with dams dataset
get_ib_ids <- function(d){ 
  # test
  # main_bas_id=1120027410
  main_bas_id <- main_bas[d]
  sbas <- hb_data%>%
    filter(MAIN_BAS == main_bas_id) %>%as.data.frame()%>% dplyr::select(-geometry)
  
  # select dams for the current basin
  dcur <- merge(sbas,dams_cur,by='HYBAS_ID')
  
  dams_no_cur <- 0
  
  if(nrow(dcur) > 0){
    
    # divide the basins in groups
    
    # clean duplicates
    dcur <- dcur[!duplicated(dcur[,1:2]),]
    
    # sort them, higher PFAFSTETTER number = more upstream
    dcur <- dcur[order(dcur$PFAF_ID,decreasing = T),]
    
    # store dams no first for data frame <<<<<<<<<<<
    dams_no_cur <- nrow(dcur)
    
    if(dams_no_cur>0){
      #### CURRENT DAMS ####
      
      # find upstream IDs of each dam
      # st <- Sys.time()
      master_upstream_list <- find_upstream_ids(t=sbas %>% select(HYBAS_ID,NEXT_DOWN), #removed the geom column ion previous
                                                IDs=(dcur$HYBAS_ID))
      
      # Sys.time() - st
      outlet <- main_bas_id
      # include the outlet (which is connected to all hb units) if not yet included (dam in outlet subbasin)
      if(main_bas_id %!in% names(master_upstream_list)){
        master_upstream_list <- append(master_upstream_list,list(outlet = sbas$HYBAS_ID))
        names(master_upstream_list)[length(master_upstream_list)] <- outlet}
      
      # order the list according to length of number of subbasins, otherwise errors with lake having same pfaffsstetter group as other subbasins
      len <- sapply(master_upstream_list, length)
      master_upstream_list <- master_upstream_list[order(len)]
      
      # create INTERBASINS groups
      inter_list <- list()
      for(i in 1:length(master_upstream_list)){
        # store the hybas_id of the upstream basins
        inter_list[[i]] <- master_upstream_list[[i]]
        # need to exclude basins that are in the upstream groups
        if(i > 1) inter_list[[i]] <- inter_list[[i]][!inter_list[[i]] %in% do.call('c',inter_list[1:(i-1)])]
      }
      names(inter_list) <- names(master_upstream_list)
      
      # # check all hb_basins are in there
      # lapply(inter_list,length) %>% unlist %>% sum
      # nrow(hb_data) # yes!
      
      
      # identify next down for each interbasin
      inter_basins <- data.frame(INTER_HYBAS_ID = names(inter_list), INTER_NEXT_DOWN = NA, INTER_ID = 1:length(inter_list), INTER_NEXT = NA)
      for(i in 1:nrow(inter_basins)){
        nid <- sbas$NEXT_DOWN[sbas$HYBAS_ID == inter_basins$INTER_HYBAS_ID[i]]
        if (length(nid)>0){ # sometimes the outlet is not in hydrobasins (because of boundary USA? ) give the most downstream one 0 as next down inter
          if(nid != 0 ){
            # search for group containing next down basin
            nid_group <- foreach(j = 1:length(inter_list),.combine = 'c') %do% (nid %in% inter_list[[j]]) %>% which #corresponds to row number so high number is downstream (except 0)
            if(length(nid_group>0)){inter_basins$INTER_NEXT_DOWN[i] <- inter_basins$INTER_HYBAS_ID[nid_group] # can also be that the next downstream interbasin is not there due to boundaries.
            }else{
              inter_basins$INTER_NEXT_DOWN[i] <-0
              nid_group <- 0}
          }
          if(nid==0){
            inter_basins$INTER_NEXT_DOWN[i] <-0
            nid_group <- 0}
        }
        if (length(nid)==0){
          inter_basins$INTER_NEXT_DOWN[i] <- 0
          nid_group <- 0
        }
        inter_basins$INTER_NEXT[i] <- nid_group
      }
      
      # create correspondence table with INTER-BASINS, hybas ID and up-down basins
      inter_basin_corr <- foreach(i = 1:length(names(inter_list)),.combine = 'rbind') %do% {
        data.frame(INTER_HYBAS_ID = names(inter_list)[i], HYBAS_ID = inter_list[[i]])
      } %>% left_join(.,inter_basins)
      
      
      #table to link to species data
      inter_basin_corr_cur=inter_basin_corr
      names(inter_basin_corr_cur)[names(inter_basin_corr_cur) == 'INTER_ID'] <- 'INTER_ID_cur'
      names(inter_basin_corr_cur)[names(inter_basin_corr_cur) == 'INTER_NEXT'] <- 'INTER_NEXT_cur'
      names(inter_basin_corr_cur)[names(inter_basin_corr_cur) == 'INTER_HYBAS_ID'] <- 'INTER_HYBAS_ID_cur'
    }else{
      inter_basin_corr_cur=data.frame(MAIN_BAS=main_bas_id, HYBAS_ID=sbas$HYBAS_ID,
                                      INTER_ID_cur=0, INTER_NEXT_cur=0, INTER_HYBAS_ID_cur=0)}
    
    
    
    
    
    
    res <- data.frame(MAIN_BAS = main_bas_id, HYBAS_ID=sbas$HYBAS_ID)
    res <- left_join(res,inter_basin_corr_cur %>% select(HYBAS_ID,INTER_ID_cur,INTER_NEXT_cur, INTER_HYBAS_ID_cur))
    
    
    
    
  }else{
    res <- data.frame(MAIN_BAS=main_bas_id, HYBAS_ID=sbas$HYBAS_ID,
                      INTER_ID_cur=0, INTER_NEXT_cur=0, INTER_HYBAS_ID_cur=0)
  }
  print(main_bas_id)
  print(d)
  return(res)
  
  
}
#sometimes gives errors in parallel?
reg_ib_tab <- do.call('rbind',
                    parallel::mclapply(1:length(main_bas),get_ib_ids,mc.silent = TRUE,mc.cores = ncores))
#reg_ib_tab <- do.call('rbind',parallel::mclapply(1:2,get_ib_ids,mc.silent = TRUE,mc.cores = 10))


saveRDS(reg_ib_tab,paste0(dir_out_ib,"NIR_interbasins_",damsdata,'.rds'))
cat('DI_interbasins Done..\n')


### ----------------------- prep_DI_create_SR_ESH_tabs ------------------------------------------------------------------
cat('prep_DI_create_SR_ESH_tabs..\n')

tab_hb <- left_join(reg_ib_tab,hb_data) %>% st_as_sf()

#inter_HYBAS_ID_cur/fut is the hybas_id of the most downstream subbasin in the interbasin/fragment
#create a template with unique interbasin ids (use inter_hybas_id), for current and future seperately


#some basins dont have dams (in current and future), they will have INTER_HYBAS_ID zero, we'd like to remove these from the selection
SR_cur <- tab_hb %>% filter(INTER_HYBAS_ID_cur!=0) %>% group_by(INTER_HYBAS_ID_cur) %>% summarise(INTER_NEXT_cur=min(INTER_NEXT_cur), 
                                                                                                  MAIN_BAS=min(MAIN_BAS),
                                                                                                  sbnr = n(),
                                                                                                  ib_area=sum(SUB_AREA))  
# unique interbasin id and indication whether its the outlet (then next is zero), total area, and geometry - current dams 
SR_cur$outlet=NA
SR_cur$outlet[which(SR_cur$INTER_NEXT_cur !=0)] <- 0 # no outlet
SR_cur$outlet[which(SR_cur$INTER_NEXT_cur ==0)] <- 1 # yes outlet (or boundary of border?)
SR_cur$INTER_NEXT_cur=NULL



#st_write(SR_cur, paste0('Dams_impact/proc/USA/interbasins_NIR_hydrobasins_',damsdata,'.rds'), append=F)


# made the tab with main basins outside linux

#cat('Reading hb12..\n')
cat('tab_mb..\n')

tab_mb <- hb_data  %>% group_by(MAIN_BAS) %>% 
  summarise(MAIN_BAS=min(MAIN_BAS),
            mb_area=sum(SUB_AREA))
# indicate whether the main basins are fragmented (now or future)
ib_cur <- SR_cur %>% as.data.frame()

tab_mb$frag_cur <- ifelse(tab_mb$MAIN_BAS %in% ib_cur$MAIN_BAS,T,F)



### ----------------------- DI_interb_speciesrange ------------------------------------------------------------------


cat('DI_interb_speciesrange..\n')

#tried to do it in parrallel but gave errors with point.in.poly
#testing it in R on linux did not show any problems, so do an array 1:10143 (all species, nrow(ids))
#i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
# Input data -------------------------------------------------------------------

# species data on hydrobasins lakes level 12
cat('Reading fish l12 data..\n')
fish12 <- foreach(cont = c('ar','na'),.combine='rbind') %do% {
  tab <- read.csv(paste0(dir_sp12, '/fish_hybas_hybas12withLakes_',cont,'.csv'))
  return(tab)
}
fish12 <- fish12 %>% dplyr::select(-id_no)
#only select fish in USA
fish12 <- fish12 %>% filter(HYBAS_ID %in% hb_data$HYBAS_ID)



# interbasins due to dams
interbasins <- reg_ib_tab

cat('Load other data..\n')
# get species id_no to link to fishsuit output
ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
#ids <- ids %>% as_tibble() %>% dplyr::select(-geom) 
# in this id list species without fishbase data are already removed
# only keep fish in USA
ids <- ids %>% filter(binomial %in% fish12$binomial)

# species traits info
traits <- read.csv(paste0(dir_fishsuit,"proc/species_traits_extra.csv")) # from fishsuit, species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no)

# to add area to subbasins
hb_area <- hb_data  %>% as_tibble() %>% dplyr::select(-geometry) %>% dplyr::select(HYBAS_ID,SUB_AREA)

# Combine interbasins and species data -----------------------------------------

# For marine & brackish species:
# if subbasin connected to outlet TRUE

# For non-marine/non-brackish species:
# use MVA if length data is available

#for each species in ids
#function to get a table when running in parallel
build_tab <- function(i){
  id_no_sp <- ids$id_no[i]
  #get the species data
  name <- ids %>% filter(id_no==id_no_sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
  sp <- fish12 %>% filter(binomial == name) # only keep this species range
  sp$id_no <- id_no_sp # add id
  #add interbasins info and hydrobasin area to data
  sp_ib <- left_join(sp,interbasins)
  sp_ib <- left_join(sp_ib,hb_area)
  # calculate total area with no dams scenario (in main basin) and current/future dams (in interbasins)
  sp_ib <- sp_ib %>% group_by(MAIN_BAS) %>% mutate(SUM_AREA_nodams = sum(SUB_AREA))
  sp_ib <- sp_ib %>% group_by(MAIN_BAS,INTER_ID_cur) %>% mutate(SUM_AREA_damscur = sum(SUB_AREA))
  
  # get species traits
  marine <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Marine)
  brackish <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Brackish)
  length <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(Length)
  lentic_only <- traits %>% filter(binomial==sp$binomial[1]) %>% pull(lentic_only)
  
  
  if(!is.na(length)){
    MVA <- calculate_MVA(length)
  } 
  
  # if lentic_only, dont produce results. Otherwise check if marine/brackish or not
  if(lentic_only ==1){
    sp_ib$hab_nodams=NA
    sp_ib$hab_curdams=NA
  }else{
    
    # if marine/brackish
    if(marine ==1 | brackish ==1){
      #dont calculate MVA, set subbasins connected to outlet to true, others false
      # INTER_NEXT=0 means that the interbasin/hydrobasin is connected to the outlet
      sp_ib$hab_nodams=TRUE
      sp_ib$hab_curdams=FALSE
      sp_ib$hab_curdams[which(sp_ib$INTER_NEXT_cur==0)] <- TRUE
    }else{
      # if not marine/brackish
      # do MVA
      
      # check if species has length data
      if(is.na(length)){
        sp_ib$hab_nodams=NA
        sp_ib$hab_curdams=NA
      }else{
        # if species has length data, do MVA
        sp_ib$hab_nodams <- sp_ib$SUM_AREA_nodams>=MVA
        sp_ib$hab_curdams <- sp_ib$SUM_AREA_damscur>=MVA
      }
    }
  }
  #save as RDS
  cat('Write polygons file \n')
  saveRDS(sp_ib,paste0(dir_out,id_no_sp,'.rds'))
}

cat('Start interb_species range parrallel running \n')
# do it parrallel
parallel::mcmapply(build_tab,1:nrow(ids),mc.silent = TRUE,mc.cores = ncores)
#parallel::mcmapply(build_tab,1:10,mc.silent = TRUE,mc.cores = ncores)

#gives errors in some cores so use array for now
# could not find the cause of the error. it has something to do with point.in.poly and parallel running...
#array length of nr of unique species
cat('Done! \n')



### ------------------------ DI_create_SR_ESH_tabs --------------------------------------------------------
cat('Start tabs calculations \n')
###########################
####### PER SPECIES #######
###########################



esh_calc <- function(i){ #,sn
  sp <- ids$id_no[i]
  name <- ids %>% filter(id_no==sp) %>% pull(binomial) #get get binomial iucn name according to fishsuit id
  # get species traits
  marine <- traits %>% filter(binomial==name) %>% pull(Marine)
  brackish <- traits %>% filter(binomial==name) %>% pull(Brackish)
  length <- traits %>% filter(binomial==name) %>% pull(Length)
  lentic_only <- traits %>% filter(binomial==name) %>% pull(lentic_only)
  diadromous <-  traits %>% filter(binomial==name) %>% pull(diad)
  
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
tab <- hb_data

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

# see prep_DI_create_SR_ESH_tabs

cat('Read interbasins tabs..\n')


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

saveRDS(SR_cur %>% as.data.frame() %>% dplyr::select(-geometry), paste0(dir_out_tabs,'SRinterbasins_cur_fwonly.rds'))

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
      tab_mb$Rtot <- rowSums(tab_mb[,c("Rtot", "ind_range")]%>% as.data.frame()%>%dplyr::select(-geometry), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
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
        tab_mb$Arange_cur <- rowSums(tab_mb[,c("Arange_cur", "aff_range")]%>% as.data.frame()%>%dplyr::select(-geometry), na.rm=TRUE) #CHECK IF THIS NOW WORKS...
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


saveRDS(tab_mb%>% as.data.frame() %>% dplyr::select(-geometry),paste0(dir_out_tabs,'mbres_fwonly.rds'))
st_write(tab_mb,paste0(dir_out_tabs,"mbres_fwonly.gpkg"), append=F)
cat('successfully written to ',dir_out_tabs,'\n')


