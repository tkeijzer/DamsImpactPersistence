# IUCN habitat
# Assign lentic/lotic/seasonal
# Adapted from FishSuit

# Tamara Keijzer
# October 2022

#always connect to local folder first
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")

dir_habitat <- 'Fishdata/iucn_habitat_list/'



# put single habitat lists in one list
list_files <- list.files(dir_habitat)

hab_list <- foreach(i = list_files) %do% return(readRDS(paste0(dir_habitat,i)))
names(hab_list) <- as.character(sapply(list_files,function(x) strsplit(x,'\\.')[[1]][1]))

# retrieve different types
types <- unique(
  do.call('c',hab_list)
)

dat <- data.frame(binomial = names(hab_list))
nrow(dat) # 11726

for(i in 1:length(types)) dat[,paste0('type_',i)] <- 0

for(i in 1:nrow(dat)){
  
  sp_name <- as.character(dat[i,'binomial'])
  
  if(!is.null(hab_list[sp_name][[1]]))  dat[i,(1+which(types %in% hab_list[sp_name][[1]]))] <- 1
  
}

# find out which habitat types contain a word that refers to river
search4proxywords <- function(proxywords,str){
  ans = FALSE
  for(w in proxywords) if(regexec(w,str,ignore.case = T)[[1]][1] > -1) ans <- TRUE
  return(ans)
}

#probably should take out deltas and estuaries as they are brackish/marine environments? <<<<
# made it a separate column
is_lotic <- sapply(types,function(x) search4proxywords(x,
                                                       proxywords = c('river','stream','creek',
                                                                      'canal','channel')))
is_lotic_brackish <- sapply(types,function(x) search4proxywords(x,
                                                                      proxywords = c('delta','estuaries')))
is_lentic <- sapply(types,function(x) search4proxywords(x,
                                                        proxywords = c('lake','pool','bog','swamp','pond')))

is_seasonal <- sapply(types,function(x) search4proxywords(x,
                                                          proxywords = c('seasonal','intermittent','irregular')))


# assign lotic-lentic type to species
dat[,'lotic'] <- 0
dat[,'lotic_brackish'] <- 0
dat[,'lentic'] <- 0
dat[,'seasonal'] <- 0
for(i in 1:nrow(dat)){
  
  dat[i,'lotic'] <- as.numeric(sum(dat[i,(which(unname(is_lotic))+1)]) > 0 )
  dat[i,'lotic_brackish'] <- as.numeric(sum(dat[i,(which(unname(is_lotic_brackish))+1)]) > 0 )
  dat[i,'lentic'] <- as.numeric(sum(dat[i,(which(unname(is_lentic))+1)]) > 0 )
  dat[i,'seasonal'] <- as.numeric(sum(dat[i,(which(unname(is_seasonal))+1)]) > 0 )
  
}

# remove species without info on lentic-lotic habitat
dat[apply(dat[,c('lotic', 'lotic_brackish','lentic')],1,sum) == 0,] <- NA
dat <- dat[!is.na(dat[,1]),]
nrow(dat)

write.csv(dat,'Fishdata/iucn_habitat_type.csv',row.names = F)

# %
sum(dat$lotic)/nrow(dat)*100
sum(dat$lentic)/nrow(dat)*100
sum(dat$seasonal)/nrow(dat)*100
sum(dat$lotic*dat$lentic)/nrow(dat)*100