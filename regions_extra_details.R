# compare results small and large dams, where is the biggest difference (basins and species)
# including both dams datasets


setwd("I:/tamara/CC_dams_R")
library(dplyr)
####################
#### PERSPECIES ####
####################
region="USA"
mva_type="TSuspsel_meansp_minbin"
damsdata1="BOTH_large"
damsdata2="BOTH"

# ESH (%/km) division
#ESH
out1 <-  paste0('Dams_impact/proc/',region,'/',damsdata1,'/model_DI_occurrence/',mva_type,'/tabs/ESHfragm_tabDI_fwonly.csv')
ESH1 <- read.csv(out1)

out2 <-  paste0('Dams_impact/proc/',region,'/',damsdata2,'/model_DI_occurrence/',mva_type,'/tabs/ESHfragm_tabDI_fwonly.csv')
ESH2 <- read.csv(out2)

diff= data.frame(binomial=ESH1$binomial, diff_PLR=ESH2$ESH_curdams-ESH1$ESH_curdams,
                 PLR_both=ESH2$ESH_curdams, PLR_large=ESH1$ESH_curdams,
                 diff_PLRkm=ESH2$ESHkm_curdams-ESH1$ESHkm_curdams, PLRkm_both=ESH2$ESHkm_curdams, PLRkm_large=ESH1$ESHkm_curdams,
                 perc_increase=ESH2$ESHkm_curdams/ESH1$ESHkm_curdams)

nrow(diff %>% filter(!is.na(diff_PLR))) # nr species
nrow(diff %>%filter(diff_PLR!=0)) # nr of species increase
nrow(diff %>%filter(perc_increase>=2)) # nr of species increase more than twice
nrow(diff %>%filter(diff_PLR>=50)) # nr of species increase more than half of range
diff %>%filter(diff_PLR == max(na.omit(diff$diff_PLR))) # highest increase species in percentage
diff %>%filter(diff_PLRkm == max(na.omit(diff$diff_PLRkm))) # highest increase species in pkm

#check range sizes species
ESH2 %>% filter(binomial %in% (diff %>%filter(diff_PLR==100)%>%pull(binomial)))

# which basins highest increase?
# check if basins level estimate has info

out1 <-  paste0('Dams_impact/proc/',region,'/',damsdata1,'/model_DI_occurrence/',mva_type,'/tabs/mbres_fwonly.rds')
mb1 <- readRDS(out1)

out2 <-  paste0('Dams_impact/proc/',region,'/',damsdata2,'/model_DI_occurrence/',mva_type,'/tabs/mbres_fwonly.rds')
mb2 <- readRDS(out2)

diff_basins= data.frame(mainbasin=mb1$MAIN_BAS, diff_PLR=mb2$PAFrange_cur-mb1$PAFrange_cur,
                 PLR_both=mb2$PAFrange_cur, PLR_large=mb1$PAFrange_cur,
                 diff_PLRkm=mb2$Arange_cur-mb1$Arange_cur, PLRkm_both=mb2$Arange_cur, PLRkm_large=mb1$Arange_cur,
                 perc_increase=mb2$PAFrange_cur/mb1$PAFrange_cur, CRL_both=mb2$Arange_cur/mb2$Rtot, CRL_large=mb1$Arange_cur/mb1$Rtot)


diff_basins %>%filter(diff_PLR == max(na.omit(diff_basins$diff_PLR))) # highest increase basin in percentage
diff_basins %>%filter(diff_PLRkm == max(na.omit(diff_basins$diff_PLRkm))) # highest increase basin in km

