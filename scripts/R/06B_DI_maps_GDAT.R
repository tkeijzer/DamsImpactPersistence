# Make maps for DI paper
# Tamara Keijzer
# april 2023

# SR=species richness / number of species occurring after accounting for the dams
# occ= number of species originally occurring
# Aspec = affected number of species (so somewhere in the basin they are affected. NOT extinct in the basin)
# occ_cur= number of species occurring after accounting for the dams (so occ - occ_cur = nr of species potentially extinct) = SR
# Arange = affected range area (total)
# Rtot = Total amount of freshwater fish range are in this interbasin/basin
# PLR = Potentially Lost Range
# PAF = Potentially Affected Fraction of species (in subbasins: occ-SR/occ, in basins: occ-Aspec/occ) also PAF_range, this is the fraction of range affected (CRL)
# CRL = Cumulative Range Loss (Arange/Rtot) = PAFrange

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")



dir_("Dams_impact/figs/Global/")
dir_figs <- dir_(paste0('Dams_impact/figs/Global/',mvrs_type,'_',dams_used,'/'))
dir_(paste0(dir_figs,"paper/"))
dir_(paste0(dir_figs,"paper/maps/"))
dir_(paste0(dir_figs,"paper/maps/extra"))

#processed data
dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
dir_proc_out <- dir_(paste0('Dams_impact/proc/',mvrs_type,'_',dams_used,'/')) # specific for relation

# inputs
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))



#> FUNCTIONS, BASE LAYERS AND COLORS -------------------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# base layers
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #

world <- rnaturalearth::ne_countries(returnclass = "sf")[,1] %>%
  st_transform(crs_custom)

url="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_wgs84_bounding_box.zip"
temp=tempfile()
temp2=tempfile()
download.file(url,temp)
unzip(zipfile=temp,exdir=temp2)
bb <- read_sf(file.path(temp2,"ne_110m_wgs84_bounding_box.shp")) %>% st_transform(crs_custom)
unlink(c(temp,temp2))


url="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_graticules_30.zip"
temp=tempfile()
temp2=tempfile()
download.file(url,temp)
unzip(zipfile=temp,exdir=temp2)
graticules <- read_sf(file.path(temp2,"ne_110m_graticules_30.shp")) %>% st_transform(crs_custom)
unlink(c(temp,temp2))

#colors
cols=viridis::plasma(100)

#> DATA -------------------------------------------------------------------------------------

tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
tab_sf <- tab_sf %>% st_transform(crs_custom)


#create difference columns
vars=c("Aspec","Arange","PAFspec","PAFrange") # see top of script for explanation

# to check values
#c <- tab_sf %>% as.data.frame() %>% dplyr::select(-geom)



#> MAPS -------------------------------------------------------------------------------------



# for maps regarding other variables see bottom script
# for different scales options see bottom script

# for PAFrange use a log scale bar
# up to 50 percent (so >50% have same color)
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #change all values above 0.5 to 0.5 so we can have a nice scale
  tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.5)] <-0.5
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.5), breaks = seq(0,0.5,0.1),labels = c(0,10,20,30,40,">=50%"), na.value = "transparent", values=scales::rescale((0:50)^1.25))+
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/figure3.jpg'),p,
         width = 170,height = 110,dpi = 1000,units = 'mm')
}


for(v in "Pextinct_spec"){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(Pextinct_spec_cur==0),  fill="black", lwd=NA) +
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(1,10,1),
      limits = c(1,10),
      labels= format(seq(1,10,1), big.mark = ",", scientific = FALSE),
      option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
}
ggsave(paste0(dir_figs,'paper/maps/Pextinct_spec.jpg'),p1,dpi=1000)



#> STATS -------------------------------------------------------------------------------------

# get table with some stats where most stuff happens
PAFrange_top <- tab_sf[order(tab_sf$PAFrange_cur,decreasing=TRUE),]
PAFrange_top <- PAFrange_top %>%as.data.frame()%>%select(-geom)
write.csv(PAFrange_top[1:100,],paste0(dir_figs,"stats_PAFrange_MBtop100.csv"), row.names = F)

PAF_basins_SI <- tab_sf %>%as.data.frame()%>%select(-geom)
CRL_basins_SI <- data.frame(basin=PAF_basins_SI$MAIN_BAS, 
                            Cum_range=PAF_basins_SI$Rtot, 
                            Cum_PLR=PAF_basins_SI$Arange_cur, 
                            CRL=PAF_basins_SI$PAFrange_cur*100, 
                            Species_PRL100=PAF_basins_SI$Pextinct_spec_cur)
write.csv(CRL_basins_SI,paste0(dir_figs,"SI_Results_basin_specific.csv"), row.names = F)



#> EXTRA MAPS -------------------------------------------------------------------------------------


#figure2 different options scales

for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #change all values above 0.5 to 0.5 so we can have a nice scale
  tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.5)] <-0.5
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.5), breaks = seq(0,0.5,0.1),labels = c(0,10,20,30,40,">=50%"), na.value = "transparent")+
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_to50perc.jpg'),p,
         dpi = 1000)
}

# test scale
mybreaks=c(0,0.01,0.1,0.25,0.5,1)

for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_viridis_c(trans = scales::pseudo_log_trans(sigma = 0.001),
                         breaks = mybreaks,
                         labels = mybreaks,
                         limits = c(0,1),
                         option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_logscale.jpg'),p)
}


for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_viridis_c(breaks = seq(0,1,0.1),
                         labels = seq(0,100,10),
                         limits = c(0,1),
                         values=scales::rescale((0:100)^2),
                         option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_diffscale.jpg'),p, dpi=1000)
}



### for PAFrange up to XX percent ####

# for PAFrange up to 5 percent
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #change all values above 0.05 to 0.05 so we can have a nice scale
  tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.05)] <-0.05
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.05), breaks = seq(0,0.05,0.01),labels = c(0:4,">=5%"), na.value = "transparent")+
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_to5perc.jpg'),p,
         dpi = 800)
}

# up to 5 percent but different scale
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #change all values above 0.05 to 0.05 so we can have a nice scale
  tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.05)] <-0.05
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_gradientn(name = "PAF", colours = cols,values=scales::rescale((0:100)^2.5), limits = c(0,0.05), breaks = seq(0,0.05,0.01),labels = c(0:4,">=5%"), na.value = "transparent")+
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_to5perc_otherscale.jpg'),p,
         dpi = 800)
}





#### other variables maps options ####
# vars different maps same scale

# create per variable
# first for the variables with scales 0-1
for(v in vars[3:4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_viridis_c(breaks = seq(0,1,0.1),
                         labels = seq(0,1,0.1),
                         limits = c(0,1),
                         option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'.jpg'),p)
}

# create per variable
# first for the variables with scales 0-1
for(v in vars[1:2]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_viridis_c(
      labels=function(x) format(x, big.mark = ",", scientific = FALSE),
      option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'.jpg'),p)
}

# for PAF spec, try to adjust the color scale (rescale) to make diff more visible
for(v in vars[3]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_fill_gradientn(name = "PAF", colours = cols,values=scales::rescale((0:100)^2.0),na.value = "transparent", labels = seq(0,1,0.1), breaks = seq(0,1,0.1))+
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_differentscale20.jpg'),p)
}

# absolute values Aspec, Arange
# NOTE: THE LIMITS, BREAKS, AND LABELS ARE SET MANUALLY
for(v in vars[1]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig ,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,300,50),
      limits = c(0,300),
      labels= format(seq(0,300,50), big.mark = ",", scientific = FALSE),
      option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          #axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_setscale.jpg'),p1,
         dpi = 800)
  
  
}

for(v in vars[2]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig ,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    labs(y="Present",x=element_blank()) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,600000,100000),
      limits = c(0,600000),
      labels= format(seq(0,600000,100000), big.mark = ",", scientific = FALSE),
      option = 'C',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          panel.grid.major = element_line(color=NA),
          axis.text = element_blank(),
          #axis.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(6,'line'),
          strip.background = element_rect('white'),
          strip.background.x = element_blank(),
          strip.background.y = element_blank(),
          strip.text = element_text(angle = 0, vjust = -1, size = 12),
          legend.title = element_blank()
    )
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_setscale.jpg'),p1,
         dpi = 800)
}



