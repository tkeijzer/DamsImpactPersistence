# make maps for DI paper
# _onlycurrent_GDAT points to taking only current stuff.

# Tamara Keijzer
# april 2023


setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")


### from settings but now manually, remove this later
# Type
# use to create folder structure with different outputs for different relations used
#mva_type="Minspec_Q001bin"
#mva_type="Minspec_Q0025bin"
#mva_type="Minspec_minbin"

#for(mva_type in c("Minspec_Q001bin", "Minspec_Q0025bin", "Minspec_minbin" )){
  
dir_("Dams_impact/figs/Global/")
  dir_figs <- dir_(paste0('Dams_impact/figs/Global/',mva_type,'_',dams_used,'/'))
  dir_(paste0(dir_figs,"paper/"))
  dir_(paste0(dir_figs,"paper/maps/"))
  dir_(paste0(dir_figs,"paper/maps/extra"))
  
  #dir_tabs <- dir_('Dams_impact/tabs/')
  
  #processed data
  dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
  dir_proc_out <- dir_(paste0('Dams_impact/proc/',mva_type,'_',dams_used,'/')) # specific for relation
###
  

dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))

#> FUNCTIONS AND BASE LAYERS -------------------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# base layers
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #

world <- rnaturalearth::ne_countries(returnclass = "sf")[,1] %>%
  st_transform(crs_custom)
# the code below gives errors with download, so I did it another way with the url given in the error message of the code below
#bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",returnclass = "sf") %>%
#  st_transform(crs_custom)
#graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",returnclass = "sf") %>%
#  st_transform(crs_custom)

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

cols=viridis::plasma(100)

#> DATA -------------------------------------------------------------------------------------


tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
tab_sf <- tab_sf %>% st_transform(crs_custom)

# try to make the most simple maps first
# so no aleration of scale bar (colors)

#create difference columns
vars=c("Aspec","Arange","PAFspec","PAFrange")



tab_sf$Aaverage_cur=tab_sf$Arange_cur/tab_sf$Aspec_cur

# check values
#c <- tab_sf %>% as.data.frame() %>% dplyr::select(-geom)



#> MAPS -------------------------------------------------------------------------------------
# 3 different maps same scale

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
    #facet_grid(scenario~.)+
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
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
    #facet_grid(scenario~.)+
    scale_fill_viridis_c(#breaks = seq(0,1,0.1),
      #labels = seq(0,1,0.1),
      #limits = c(0,1),
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'.jpg'),p)
  
}



########### DIFFERENT SCALES #######################################################
#try different scales options (more colors low ranges)


#myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
#plot(scales::rescale((1:100)^1.5), col=myPalette(100))

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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    scale_fill_gradientn(name = "PAF", colours = cols,values=scales::rescale((0:100)^2.0),na.value = "transparent", labels = seq(0,1,0.1), breaks = seq(0,1,0.1))+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
    #scale_fill_viridis_c(trans = scales::pseudo_log_trans(sigma = 0.001),
    #                     breaks = mybreaks,
    #                     labels = mybreaks,
    #                     limits = c(0,1),
    #                     option = 'C',na.value = "transparent") +
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_differentscale20.jpg'),p)
}


# test scale
mybreaks=c(0,0.01,0.1,0.25,0.5,1)

# for PAFrange use a log scale bar
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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    #scale_fill_gradientn(name = "PAF", colours = cols,values=scales::rescale((0:100)^2.5),na.value = "transparent")+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
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
    #facet_grid(scenario~.)+
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_diffscale.jpg'),p, dpi=1000)
}

# up to 50 percent
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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.5), breaks = seq(0,0.5,0.1),labels = c(0,10,20,30,40,">=50%"), na.value = "transparent")+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
    #scale_fill_viridis_c(#trans = scales::pseudo_log_trans(sigma = 0.001),
    #                     breaks = seq(0,0.05,0.01),
    #                     labels = c(0:4,">=5"),
    #                     limits = c(0,0.05),
    #                     option = 'C',na.value = "transparent") +
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_to50perc.jpg'),p,
         dpi = 1000)
}

#rescale a bit
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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.5), breaks = seq(0,0.5,0.1),labels = c(0,10,20,30,40,">=50%"), na.value = "transparent", values=scales::rescale((0:50)^1.25))+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
    #scale_fill_viridis_c(#trans = scales::pseudo_log_trans(sigma = 0.001),
    #                     breaks = seq(0,0.05,0.01),
    #                     labels = c(0:4,">=5"),
    #                     limits = c(0,0.05),
    #                     option = 'C',na.value = "transparent") +
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_to50perc2_mm.jpg'),p,
         width = 170,height = 110,dpi = 1000,units = 'mm')
}



# for PAFrange up to XX percent

# for PAFrange up to XX percent
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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    scale_fill_gradientn(name = "PAF", colours = cols, limits = c(0,0.05), breaks = seq(0,0.05,0.01),labels = c(0:4,">=5%"), na.value = "transparent")+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
    #scale_fill_viridis_c(#trans = scales::pseudo_log_trans(sigma = 0.001),
    #                     breaks = seq(0,0.05,0.01),
    #                     labels = c(0:4,">=5"),
    #                     limits = c(0,0.05),
    #                     option = 'C',na.value = "transparent") +
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_to5perc.jpg'),p,
         dpi = 800)
}

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
    #scale_fill_continuous(type="viridis",trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.1,0.25,0.50,1), breaks=c(0,0.01,0.1,0.25,0.50,1),na.value = "transparent")+
    #scale_fill_manual(values=cols) +
    scale_fill_gradientn(name = "PAF", colours = cols,values=scales::rescale((0:100)^2.5), limits = c(0,0.05), breaks = seq(0,0.05,0.01),labels = c(0:4,">=5%"), na.value = "transparent")+
    #scale_fill_gradientn(name = "PAF", colours = cols,trans=scales::pseudo_log_trans(base = 10),breaks=mybreaks, labels=mybreaks,na.value = "transparent")+
    #scale_fill_viridis_c(#trans = scales::pseudo_log_trans(sigma = 0.001),
    #                     breaks = seq(0,0.05,0.01),
    #                     labels = c(0:4,">=5"),
    #                     limits = c(0,0.05),
    #                     option = 'C',na.value = "transparent") +
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
  #p
  #ggsave(paste0(dir_figs,'paper/maps/test.pdf'),p,
  #       width = 210,height = 229.51,dpi = 600,units = 'mm')
  
  ggsave(paste0(dir_figs,'paper/maps/extra/MB_',v,'_to5perc_otherscale.jpg'),p,
         dpi = 800)
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
    #facet_grid(scenario~.)+
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




for(v in "Pextinct_spec"){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
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
          #axis.title = element_blank(),
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





############################ STATS #####################################
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

continent = c('af','ar','as','au','eu','gr','na','sa','si')

# interbasins due to dams
interbasins <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  tab <- readRDS(paste0('Dams_impact/proc/InterBasins_tab_GDAT_',cont,'.rds')) # shouldnt this then be GDATall? though this was just a check what was happening
  return(tab)
}

nrfrag <- interbasins %>% group_by(MAIN_BAS) %>% summarise(nrib_cur=max(INTER_ID_cur)+1)
# the basins without dams are already filtered out of the interbasins table. 
# However sometimes in a main basin the dam is located in the outlet subbasin. This according to our method does not really fragment, just cut off the access to sea.

# how many basins are fragmented (i.e. between subbasins) by current dams?
nrow(nrfrag%>%filter(nrib_cur==2)) # this is how many basins contain dams, but only in outlet basin (so only cut off from marine)
nrow(nrfrag%>%filter(nrib_cur>2)) # this is how many basins contain dams which seperate subbasins from each other






#}



############################## GARBAGE ###########################################

#cant plot it unfortunately with discrete scale....
#ggplot() +
#  #geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
#  #geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
#  #geom_sf(data = world, fill = "grey90", lwd = NA) +
#  geom_sf(data = nrfrag_sf[,] , aes(fill=cl), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
#  #geom_sf(data = nrfrag_sf %>% filter(!is.na(occ))%>% filter(frag_fut==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
#  #facet_grid(scenario~.)+
#  #labs(y=element_blank(),x=element_blank(), fill= "Total PLR (km²)" ) +
#  #scale_y_continuous(position = "right")+
#  scale_fill_manual(values=c("red","blue","yellow","green")) +
#  scale_fill_viridis_c(
#    #breaks = seq(0,70000,10000),
#    #limits = c(0,70000),
#    #labels= format(seq(0,70000,10000), big.mark = ",", scientific = FALSE),
#    option = 'C',na.value = "transparent") +
#  theme_minimal() +
#  theme(text = element_text(size = 12),
#        panel.grid.major = element_line(color=NA),
#        axis.text = element_blank(),
#        #axis.title = element_blank(),
#        legend.position = 'bottom',
#        legend.key.width = unit(6,'line'),
#        strip.background = element_rect('white'),
#        strip.background.x = element_blank(),
#        strip.background.y = element_blank(),
#        strip.text = element_text(angle = 0, vjust = -1, size = 12),
#        legend.title = element_blank()
#  )




