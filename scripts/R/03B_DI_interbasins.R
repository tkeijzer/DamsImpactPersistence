# Dams impact interbasin numbers
# Tamara Keijzer
# October 2022
# updated April 2023



#SBATCH -N 1 -n 10 
#SBATCH --mem=20G
#SBATCH --array 1-9
source("scripts/SETTINGS_Dams_impact.R")

# HydroBASINS data ------------------------------------------------------------------------------------------
# read hydrobasins data
g <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cont <- c('af','ar','as','au','eu','gr','na','sa','si')[[g]]

hb_data <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% as_tibble() %>% select(-geometry)

cat('\nCompiling main basin area..')
# add basin area
main_bas_area <- hb_data %>%
  select(HYBAS_ID,MAIN_BAS,SUB_AREA) %>%
  group_by(MAIN_BAS) %>%
  summarize(MAIN_BAS_AREA = sum(SUB_AREA))

hb_data <- inner_join(hb_data,main_bas_area,by='MAIN_BAS')

#vector with all unique main basin ids
main_bas=unique(hb_data$MAIN_BAS)

# Dams data ---------------------------------------------------------------------------------------------
# filter on dams occurring on this continent
if(dams_used == "GrG2FH"){
dams_cur <- readRDS('Dams_impact/proc/dams_current_hydrobasins.rds') %>%
  filter(HYBAS_ID %in% hb_data$HYBAS_ID)
dams_fut <- readRDS('Dams_impact/proc/dams_future_hydrobasins.rds') %>%
  filter(HYBAS_ID %in% hb_data$HYBAS_ID)
}

if(dams_used == "GDAT"){
dams_cur <- readRDS('Dams_impact/proc/dams_gdat_15m_hydrobasins.rds') %>%
  filter(HYBAS_ID %in% hb_data$HYBAS_ID)
}

if(dams_used == "GDATall"){
  dams_cur <- readRDS('Dams_impact/proc/dams_gdat_all_hydrobasins.rds') %>%
    filter(HYBAS_ID %in% hb_data$HYBAS_ID)
}



# Interbasins ---------------------------------------------------------------------------------------------
# Function to assign interbasin ids (cur/fut) to subbasins, per main basin
cat('\nLoad function..\n\n')

if(dams_used == "GrG2FH"){
get_ib_ids <- function(d){ 
  # test
  # main_bas_id=1120027410
  main_bas_id <- main_bas[d]
  sbas <- hb_data%>%
    filter(MAIN_BAS == main_bas_id)
  
  # select dams for the current basin
  dcur <- merge(sbas,dams_cur,by='HYBAS_ID')
  dfut <- merge(sbas,dams_fut,by='HYBAS_ID')
  
  dams_no_cur <- 0
  dams_no_fut <- 0
  
  if(sum(nrow(dcur),nrow(dfut)) > 0){
    
    # divide the basins in groups
    
    # clean duplicates
    dcur <- dcur[!duplicated(dcur[,1:2]),]
    dfut <- rbind(dcur[,1:9],dfut[,1:9])
    dfut <- dfut[!duplicated(dfut[,1:2]),]
    
    # sort them, higher PFAFSTETTER number = more upstream
    dcur <- dcur[order(dcur$PFAF_ID,decreasing = T),]
    dfut <- dfut[order(dfut$PFAF_ID,decreasing = T),]
    
    # store dams no first for data frame <<<<<<<<<<<
    dams_no_cur <- nrow(dcur)
    dams_no_fut <- nrow(dfut)
    
    if(dams_no_cur>0){
          #### CURRENT DAMS ####
          
          # find upstream IDs of each dam
          # st <- Sys.time()
          master_upstream_list <- find_upstream_ids(t=sbas %>% select(HYBAS_ID,NEXT_DOWN), #removed the geom column
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
            if(nid != 0){
              # search for group containing next down basin
              nid_group <- foreach(j = 1:length(inter_list),.combine = 'c') %do% (nid %in% inter_list[[j]]) %>% which #corresponds to row number so high number is downstream (except 0)
              inter_basins$INTER_NEXT_DOWN[i] <- inter_basins$INTER_HYBAS_ID[nid_group]
            }else{
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
    
    
    
    if(dams_no_fut>0){
          #### FUTURE DAMS ####
          master_upstream_list <- find_upstream_ids(t=sbas %>% select(HYBAS_ID,NEXT_DOWN), #removed the geom column
                                                    IDs=(dfut$HYBAS_ID))
          
          # Sys.time() - st
          outlet <- main_bas_id
          # include the outlet (which is connected to all hb units) if not yet included (dam in outlet subbasin)
          if(main_bas_id %!in% names(master_upstream_list)){
            master_upstream_list <- append(master_upstream_list,list(outlet = sbas$HYBAS_ID))
            names(master_upstream_list)[length(master_upstream_list)] <- outlet}
          
          # order the list according to length of subbasins, otherwise errors with lake having same pfaffsstetter group as other subbasins
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
            if(nid != 0){
              # search for group containing next down basin
              nid_group <- foreach(j = 1:length(inter_list),.combine = 'c') %do% (nid %in% inter_list[[j]]) %>% which #corresponds to row number so high number is downstream (except 0)
              inter_basins$INTER_NEXT_DOWN[i] <- inter_basins$INTER_HYBAS_ID[nid_group]
            }else{
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
          inter_basin_corr_fut=inter_basin_corr
          names(inter_basin_corr_fut)[names(inter_basin_corr_fut) == 'INTER_ID'] <- 'INTER_ID_fut'
          names(inter_basin_corr_fut)[names(inter_basin_corr_fut) == 'INTER_NEXT'] <- 'INTER_NEXT_fut'
          names(inter_basin_corr_fut)[names(inter_basin_corr_fut) == 'INTER_HYBAS_ID'] <- 'INTER_HYBAS_ID_fut'
    }else{
          inter_basin_corr_fut=data.frame(MAIN_BAS=main_bas_id, HYBAS_ID=sbas$HYBAS_ID,
                                           INTER_ID_fut=0, INTER_NEXT_fut=0, INTER_HYBAS_ID_fut=0)}
    
    
    
    
    
    res <- data.frame(MAIN_BAS = main_bas_id, HYBAS_ID=sbas$HYBAS_ID)
    res <- left_join(res,inter_basin_corr_cur %>% select(HYBAS_ID,INTER_ID_cur,INTER_NEXT_cur, INTER_HYBAS_ID_cur))
    res <- left_join(res,inter_basin_corr_fut %>% select(HYBAS_ID,INTER_ID_fut,INTER_NEXT_fut, INTER_HYBAS_ID_fut))
    
    
    # create polygon shapefile of interbasins
    #hb_data_sf <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp'))
    #inter_basin_sf <- merge(hb_data_sf,res) %>% st_as_sf()
    
    #write_sf(inter_basin_sf,"Dams_impact/visual_check/check_inter_basins_HB12lakes_test.gpkg")
    
    
    
  }else{
    res <- data.frame(MAIN_BAS=main_bas_id, HYBAS_ID=sbas$HYBAS_ID,
               INTER_ID_cur=0, INTER_NEXT_cur=0, INTER_HYBAS_ID_cur=0,
               INTER_ID_fut=0, INTER_NEXT_fut=0, INTER_HYBAS_ID_fut=0)
  }
  print(main_bas_id)
  print(d)
  return(res)


}}







if(dams_used == "GDAT"){
get_ib_ids <- function(d){ 
  # test
  # main_bas_id=1120027410
  main_bas_id <- main_bas[d]
  sbas <- hb_data%>%
    filter(MAIN_BAS == main_bas_id)
  
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
      master_upstream_list <- find_upstream_ids(t=sbas %>% select(HYBAS_ID,NEXT_DOWN), #removed the geom column
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
        if(nid != 0){
          # search for group containing next down basin
          nid_group <- foreach(j = 1:length(inter_list),.combine = 'c') %do% (nid %in% inter_list[[j]]) %>% which #corresponds to row number so high number is downstream (except 0)
          inter_basins$INTER_NEXT_DOWN[i] <- inter_basins$INTER_HYBAS_ID[nid_group]
        }else{
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
  
  
}}





if(dams_used == "GDAT" || dams_used == "GDATall"){
  get_ib_ids <- function(d){ 
    # test
    # main_bas_id=1120027410
    main_bas_id <- main_bas[d]
    sbas <- hb_data%>%
      filter(MAIN_BAS == main_bas_id)
    
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
        master_upstream_list <- find_upstream_ids(t=sbas %>% select(HYBAS_ID,NEXT_DOWN), #removed the geom column
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
          if(nid != 0){
            # search for group containing next down basin
            nid_group <- foreach(j = 1:length(inter_list),.combine = 'c') %do% (nid %in% inter_list[[j]]) %>% which #corresponds to row number so high number is downstream (except 0)
            inter_basins$INTER_NEXT_DOWN[i] <- inter_basins$INTER_HYBAS_ID[nid_group]
          }else{
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
    
    
  }}
# Run in parrallel -------------------------------------------------------------------------------------------
library(parallel)
cont_tab <- do.call('rbind',
                      parallel::mclapply(1:length(main_bas),get_ib_ids,mc.silent = TRUE,mc.cores = 10))
length(main_bas)
length(unique(cont_tab$MAIN_BAS))
detectCores()

if(dams_used == "GrG2FH"){
saveRDS(cont_tab,paste0('Dams_impact/proc/InterBasins_tab_GrG2FH_',cont,'.rds'))}


if(dams_used == "GDAT"){
  saveRDS(cont_tab,paste0('Dams_impact/proc/InterBasins_tab_GDAT_',cont,'.rds'))}

if(dams_used == "GDATall"){
  saveRDS(cont_tab,paste0('Dams_impact/proc/InterBasins_tab_GDATall_',cont,'.rds'))}


warnings()

cat('\nSaving table..\n\n')

