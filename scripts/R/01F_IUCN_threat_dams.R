# get info whether dams are considered a threat by IUCN
# do this for the 10143 species

#always connect to local folder first
#local_model_folder="I:/tamara/CC_dams_R"
#setwd(local_model_folder)

library(rredlist)
library(dplyr)
library(sf)

#SBATCH --nodes=10
#SBATCH --mem=5G

# PERHAPS FIRST DO FOR EACH SPECIES SEPERATELY THE THREAT DF SAVE AS RDS
# FOR SPECIES WITHOUTH THREAT DATA SAVE AN EMPTY DF?

#ncores=1 #local
ncores=10

#WHEN USING FISHSUIT
#dir_fishsuit="I:/tamara/FishSuit/" #local
#dir_fishsuit="/vol/milkunI/tamara/FishSuit/" #cluster
#ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)

#WHEN NOT USING FISHSUIT
ids <- read.csv("Fishdata/species_traits_extra.csv") %>% dplyr::select(id_no,binomial)


#token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'

#testing
#hab <- rl_threats(name = 'Channa marulius',key = token)$result
#hab

#hab <- rl_threats(name = 'Labeo potail',key = token)$result
#hab

#hab <- rl_threats(name = 'Aaptosyax grypus',key = token)$result
#hab

#hab <- rl_threats(name = 'Ameca splendens',key = token)$result
#hab

#ids = data.frame(id_no=c(1:4), binomial=c('Channa marulius', 'Labeo potail', 'Aaptosyax grypus', 'Ameca splendens'))

# Retrieved the treat tables separately (see IUCN_threat_list.R)
#dir_threats="/vol/milkunI/tamara/CC_dams_R/Fishdata/iucn_threat_list/"
dir_threats="Fishdata/iucn_threat_list/" #local
# retrieve these and put in function below





#nice function to get a nice table with the desired info, 1 row per species
get_info <- function(i){
  sp=ids$binomial[i]
  
  d= data.frame(
    id_no=ids$id_no[i],
    binomial=sp,
    Ldams=NA, # large dams
    LD_scope=NA,
    LD_sev=NA,
    LD_score=NA,
    Sdams=NA, # small dams
    SD_scope=NA,
    SD_sev=NA,
    SD_score=NA,
    Udams=NA, # dams of unknown size
    UD_scope=NA,
    UD_sev=NA,
    UD_score=NA,
    Ldams_fut=NA, # large dams future
    LD_scope_fut=NA,
    LD_sev_fut=NA,
    LD_score_fut=NA,
    Sdams_fut=NA, # small dams future
    SD_scope_fut=NA,
    SD_sev_fut=NA,
    SD_score_fut=NA,
    Udams_fut=NA, # dams of unknown size future
    UD_scope_fut=NA,
    UD_sev_fut=NA,
    UD_score_fut=NA
  )
  hab <- readRDS(paste0(dir_threats,sp,".rds"))
  
  
  if(nrow(hab)==0){#means there is no info on threats
    return(d) # the info on dams threat will be NA instead of FALSE or TRUE
  }else{
    hab_cur <- hab%>% filter(timing=="Ongoing")
    
    #check if its there
    d[,"Ldams"] <- "7.2.10" %in% hab_cur$code
    d[,"Sdams"] <- "7.2.9" %in% hab_cur$code
    d[,"Udams"] <- "7.2.11" %in% hab_cur$code
    
    # and if so put info in the right column
    if(d[,"Ldams"]==T){
      d[,"LD_scope"] <- hab_cur %>% filter(code == "7.2.10")%>% pull(scope)
      d[,"LD_sev"] <- hab_cur %>% filter(code == "7.2.10")%>% pull(severity)
      d[,"LD_score"] <- hab_cur %>% filter(code == "7.2.10")%>% pull(score)
    }
    
    if(d[,"Sdams"]==T){
      d[,"SD_scope"] <- hab_cur %>% filter(code == "7.2.9")%>% pull(scope)
      d[,"SD_sev"] <- hab_cur %>% filter(code == "7.2.9")%>% pull(severity)
      d[,"SD_score"] <- hab_cur %>% filter(code == "7.2.9")%>% pull(score)
    }
    
    if(d[,"Udams"]==T){
      d[,"UD_scope"] <- hab_cur %>% filter(code == "7.2.11")%>% pull(scope)
      d[,"UD_sev"] <- hab_cur %>% filter(code == "7.2.11")%>% pull(severity)
      d[,"UD_score"] <- hab_cur %>% filter(code == "7.2.11")%>% pull(score)
    }
    
    
    #also check future
    hab_fut <- hab %>% filter(timing=="Future")
    
    d[,"Ldams_fut"] <- "7.2.10" %in% hab_fut$code
    d[,"Sdams_fut"] <- "7.2.9" %in% hab_fut$code
    d[,"Udams_fut"] <- "7.2.11" %in% hab_fut$code
    
    if(d[,"Ldams_fut"]==T){
      d[,"LD_scope_fut"] <- hab_fut %>% filter(code == "7.2.10")%>% pull(scope)
      d[,"LD_sev_fut"] <- hab_fut %>% filter(code == "7.2.10")%>% pull(severity)
      d[,"LD_score_fut"] <- hab_fut %>% filter(code == "7.2.10")%>% pull(score)
    }
    
    if(d[,"Sdams_fut"]==T){
      d[,"SD_scope_fut"] <- hab_fut %>% filter(code == "7.2.9")%>% pull(scope)
      d[,"SD_sev_fut"] <- hab_fut %>% filter(code == "7.2.9")%>% pull(severity)
      d[,"SD_score_fut"] <- hab_fut %>% filter(code == "7.2.9")%>% pull(score)
    }
    
    if(d[,"Udams_fut"]==T){
      d[,"UD_scope_fut"] <- hab_fut %>% filter(code == "7.2.11")%>% pull(scope)
      d[,"UD_sev_fut"] <- hab_fut %>% filter(code == "7.2.11")%>% pull(severity)
      d[,"UD_score_fut"] <- hab_fut %>% filter(code == "7.2.11")%>% pull(score)
    }
    return(d)
  } 
}
start_time <- Sys.time()
tab <- 
  do.call('rbind',
          parallel::mcmapply(get_info,1:nrow(ids),SIMPLIFY = F,mc.cores = ncores)
  )
end_time <- Sys.time()
row.names(tab) <- NULL

write.csv(tab,"Fishdata/IUCN_DamsThreat.csv",row.names = F) # local
#write.csv(tab,"/vol/milkunI/tamara/CC_dams_R/Fishdata/IUCN_DamsThreat.csv",row.names = F) #cluster