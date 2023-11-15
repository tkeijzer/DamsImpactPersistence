# get fishbase data for IUCN hydrobasins referenced data
# Tamara Keijzer
# september 2022
#code adapted from Valerio extract_iucn2hybas12.R in connectfish


#always connect to local folder first
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")


# settings
setwd(paste0(local_model_folder,"/Fishdata"))
library(dplyr);library(foreach);library(rfishbase)
options(FISHBASE_VERSION="21.06")




# load species data
sp <- foreach(i = 1:6,.combine='rbind') %do% {
  poly <- read.csv(paste0('Fish_hybas/fish_hybas_table_part',i,'.csv'))
  colnames(poly)[1] = "objectid"
  return(poly)
}
sp <- sp[sp$presence %in% c(1,2),] 
length(unique(sp$binomial)) #11465 species

# validate names
fb_names=array()
your_list=unique(sp$binomial) # this is not an actual R list item, just a vector with each element a species name
for(i in 1:length(your_list)) {
  fb_names[i]=as.character(validate_names(your_list[i])[1])
}
iucn_fbnames=data.frame(binomial=your_list,fb_name=fb_names)
dir_(paste0(local_model_folder,"Fishdata/Fishbase"))
write.csv(iucn_fbnames,'Fishbase/iucn_hybas_fb21names.csv', row.names = F) 

#load and check
iucn_fbnames <- read.csv("Fishbase/iucn_hybas_fb21names.csv")
sum(is.na(iucn_fbnames$fb_name)) #519
sum(!is.na(iucn_fbnames$fb_name)) 
sum(!is.na(unique(iucn_fbnames$fb_name))) #10946, 10888 unique


#get a table with info for each fish species if available
sp <- unique(iucn_fbnames) # make sure no double nams
species_fb <- na.omit(unique(sp$fb_name)) # remove species not known in fishbase

#add traits from fishbase


# see what things mean, but most of time not described...
# may look at https://www.fishbase.de/manual/english/contents.htm , but not always clear
tables <- docs()
# Describe a table
dplyr::filter(tables, table == "species")$description
## See fields in a table
docs("species")
rm(tables)


# from species table
traits.sp <- species(species_fb, # vector of species names
                     fields = c("Species", "Brack","Saltwater","Fresh","AnaCat","Length"))
colnames(traits.sp) = c("fb_name", "Brackish","Saltwater","Freshwater","Migration","Length")
traits.sp$Migration[traits.sp$Migration == " "] <- NA

sp2 <- merge(sp,traits.sp,all.x=T,by ="fb_name",sort=F) # add the trait data to the species names

#for example, commercial imporance:
# traits.sp <- species(species_fb, # vector of species names
#                      fields = c("Species", "Importance"))
# sp2 <- merge(sp,traits.sp,all.x=T,by ="fb_name",sort=F) # add the trait data to the species names


sum(is.na(sp2$Length)) #1273 na's
sum(!is.na(sp2$Length)) #10192

## habitat traits
traits.hab <- ecology(species_fb,
                      fields=c("Species","Estuaries","Mangroves","MarshesSwamps","Stream","Lakes"))
colnames(traits.hab)[1]="fb_name"
# Make data binary (1 = TRUE; 0 = FALSE)
traits.hab$Estuaries <- ifelse(traits.hab$Estuaries == -1, 1, 0)
traits.hab$Mangroves <- ifelse(traits.hab$Mangroves == -1, 1, 0)
traits.hab$MarshesSwamps <- ifelse(traits.hab$MarshesSwamps == -1, 1, 0)
traits.hab$Stream <- ifelse(traits.hab$Stream == -1, 1, 0)
traits.hab$Lakes <- ifelse(traits.hab$Lakes == -1, 1, 0)
traits.hab <- traits.hab[-which(duplicated(traits.hab$fb_name)),]
sp3 <- merge(sp2,traits.hab,by="fb_name",all.x=T,sort=F)


sp4=sp3
sp4$Lentic_only <- NA
sp4$Lentic_only <- ifelse(sp4$Estuaries==0 & sp4$Stream==0 &sp4$Lakes==1,1,0)
sp4$Lentic_only <- ifelse(sp4$Estuaries==0 & sp4$Stream==0 &sp4$MarshesSwamps==1,1,sp4$Lentic_only)
length(which(sp4$Lentic_only==1)) #486 species
sum(!is.na(sp4$Estuaries)) # 3350 species with habitat data

write.csv(sp4, "Fishbase/iucn_hybas_fb.csv")
