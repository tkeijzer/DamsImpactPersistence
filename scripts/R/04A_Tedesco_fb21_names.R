# Tedesco fishbase names
# Tamara Keijzer
# October 2022


local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
library(dplyr);library(foreach);library(rfishbase)
options(FISHBASE_VERSION="21.06")



#load data
dat <- read.csv("Tedesco/Occurrence_table.csv", sep=";")
dat$X6.Fishbase.Valid.Species.Name=gsub(".", " ", dat$X6.Fishbase.Valid.Species.Name, fixed=T)

# validate names
your_list=unique(dat$X6.Fishbase.Valid.Species.Name) # this is not an actual R list item, just a vector with each element a species name

fb_names=array()
for(i in 1:length(your_list)) {
  fb_names[i]=as.character(validate_names(your_list[i])[1])
}
dat_fbnames=data.frame(X6.Fishbase.Valid.Species.Name=your_list,fb_name=fb_names)
df=left_join(dat,dat_fbnames)
write.csv(df,'Tedesco/Occurrence_table_fb21names.csv', row.names = F) 

#tried to do it with speccode but fishbase cannot search on speccodes only on fb names
