# make maps for DI paper

# Tamara Keijzer
# feb 2023


setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")


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



#> DATA -------------------------------------------------------------------------------------


tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
tab_sf <- tab_sf %>% st_transform(crs_custom)

# try to make the most simple maps first
# so no aleration of scale bar (colors)

#create difference columns
scen=c("cur","fut")
vars=c("Aspec","Arange","PAFspec","PAFrange")

# does not work so do it by hand
#for(v in vars){
#  tab_sf2 <- tab_sf %>% mutate(get(paste0(v,"_diff")) = get(paste0(v,"_fut")) - get(paste0(v,"_cur")))
#}

tab_sf$Aspec_diff=tab_sf$Aspec_fut - tab_sf$Aspec_cur
tab_sf$Arange_diff=tab_sf$Arange_fut - tab_sf$Arange_cur
tab_sf$PAFspec_diff=tab_sf$PAFspec_fut - tab_sf$PAFspec_cur
tab_sf$PAFrange_diff=tab_sf$PAFrange_fut - tab_sf$PAFrange_cur

tab_sf$Aaverage_cur=tab_sf$Arange_cur/tab_sf$Aspec_cur
tab_sf$Aaverage_fut=tab_sf$Arange_fut/tab_sf$Aspec_fut
tab_sf$Aaverage_diff=tab_sf$Aaverage_fut - tab_sf$Aaverage_cur
#tab_sf$Aaverage_futincrease=tab_sf$Arange_diff/tab_sf$Aspec_diff # this makes no sense as all increased range will be divided by only the new species
#tab_sf$Aaverage_futincrease[which(tab_sf$Aaverage_futincrease==Inf)] <- 0 # replace inf values by zero (Apec_diff=0 hence Inf)

tab_sf$Pextinct_spec_diff=tab_sf$Pextinct_spec_fut - tab_sf$Pextinct_spec_cur

# check values
c <- tab_sf %>% as.data.frame() %>% dplyr::select(-geom)



#> MAPS -------------------------------------------------------------------------------------
# 3 different maps same scale

# create per variable
# first for the variables with scales 0-1
for(v in vars[3:4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", "Future", "Difference"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", "Future", "Difference"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'.jpg'),p)
}





# create per variable
# first for the variables with scales 0-1
for(v in vars[1:2]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", "Future", "Difference"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", "Future", "Difference"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'.jpg'),p)
  
  
  #difference plot separately
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Difference"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
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
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_diff.jpg'),p)
  
}



########### DIFFERENT SCALES #######################################################
#try different scales options (more colors low ranges)


#myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
#plot(scales::rescale((1:100)^1.5), col=myPalette(100))

# for PAF spec, try to adjust the color scale (rescale) to make diff more visible
cols=viridis::plasma(100)
for(v in vars[3]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", "Future", "Difference"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", "Future", "Difference"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_differentscale20.jpg'),p)
}


# test scale
mybreaks=c(0,0.01,0.1,0.25,0.5,1)

# for PAFrange use a log scale bar
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", "Future", "Difference"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", "Future", "Difference"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_logscale.jpg'),p)
}




############### ONLY PRESENT AND FUTURE INCREASE
# do a difference map? or just present, and future increase (diff, with own scale for Arange and Aspec)


#PAF
# for PAF spec,  adjust the color scale (rescale) to make diff more visible
cols=viridis::plasma(100)
for(v in vars[3]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'_differentscale20.jpg'),p,
         dpi = 800)
}


# test scale
mybreaks=c(0,0.01,0.1,0.25,0.5,1)

# for PAFrange use a log scale bar
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'_logscale.jpg'),p,
         dpi = 800)
}

# for PAFrange up to XX percent
for(v in vars[4]){
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #change all values above 0.05 to 0.05 so we can have a nice scale
  tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.05)] <-0.05
  tab_sf_noNA$PAFrange_diff[which(tab_sf_noNA$PAFrange_diff>0.05)] <-0.05
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # and draw
  p <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    facet_grid(scenario~.)+
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
  
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'_to5perc_otherscale.jpg'),p,
         dpi = 800)
}




# absolute values Aspec, Arange
# NOTE: THE LIMITS, BREAKS, AND LABELS ARE SET MANUALLY


for(v in vars[1]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y="Present",x=element_blank()) +
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
  
  
  
  #future increase
  p2 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Future increase"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y="Future increase",x=element_blank()) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,50,10),
      limits = c(0,50),
      labels= format(seq(0,50,10), big.mark = ",", scientific = FALSE),
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
  
  p12=ggarrange(plotlist =  list(p1,p2),nrow=2)
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'.jpg'),p12,
         dpi = 800)
  
  
}


for(v in vars[2]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
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
  
  
  
  #future increase
  p2 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Future increase"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y="Future increase",x=element_blank()) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,70000,10000),
      limits = c(0,70000),
      labels= format(seq(0,70000,10000), big.mark = ",", scientific = FALSE),
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
  
  p12=ggarrange(plotlist =  list(p1,p2),nrow=2)
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'.jpg'),p12,
         dpi = 800)
  
}



#get Arange/Aspec
for(v in c("Aaverage")){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  #negative values in diff turn to zero and change label to <=0
  tab_sf_noNA$Aaverage_diff[which(tab_sf_noNA$Aaverage_diff<0)] <-0
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff"))) # choose diff or something else
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y="Present",x=element_blank()) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,8000,1000),
      limits = c(0,8000),
      labels= format(seq(0,8000,1000), big.mark = ",", scientific = FALSE),
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
  
  
  
  #future increase
  p2 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Future increase"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y="Future increase",x=element_blank()) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,800,100),
      limits = c(0,800),
      labels= c("<0", format(seq(100,800,100), big.mark = ",", scientific = FALSE)),
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
  
  p12=ggarrange(plotlist =  list(p1,p2),nrow=2)
  ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'.jpg'),p12,
         dpi = 800)
  
}




########################## SECOND VERSION #####################################

# absolute values Aspec, Arange
# NOTE: THE LIMITS, BREAKS, AND LABELS ARE SET MANUALLY

#Aspec
for(v in vars[1]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  fAspecPres <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y=element_blank(),x=element_blank(), fill="Number of impacted species") +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,300,50),
      limits = c(0,300),
      labels= format(seq(0,300,50), big.mark = ",", scientific = FALSE),
      option = 'D',na.value = "transparent") +
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
  
  
  
  #future increase
  fAspecFut <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Future increase"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(title="Future increase",y=element_blank(),x=element_blank(), fill="Number of impacted species") +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,50,10),
      limits = c(0,50),
      labels= format(seq(0,50,10), big.mark = ",", scientific = FALSE),
      option = 'D',na.value = "transparent") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          plot.title=element_text(hjust=0.5),
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
  
  #p12=ggarrange(plotlist =  list(p1,p2),nrow=2)
  #ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'.jpg'),p12,
  #       dpi = 800)
  
  
}


for(v in vars[2]){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  fArangePres <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y=element_blank(),x=element_blank(),fill="Total PLR (km²)") +
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
  
  
  
  #future increase
  fArangeFut <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Future increase"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    #facet_grid(scenario~.)+
    labs(y=element_blank(),x=element_blank(), fill= "Total PLR (km²)" ) +
    scale_y_continuous(position = "right")+
    scale_fill_viridis_c(
      breaks = seq(0,70000,10000),
      limits = c(0,70000),
      labels= format(seq(0,70000,10000), big.mark = ",", scientific = FALSE),
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
  
  #p12=ggarrange(plotlist =  list(p1,p2),nrow=2)
  #ggsave(paste0(dir_figs,'paper/maps/2plots_MB_',v,'.jpg'),p12,
  #       dpi = 800)
  
}

f2Pres=ggarrange(plotlist=list(fAspecPres,fArangePres),nrow=2)
ggsave(paste0(dir_figs,'paper/maps/2plots_MB_Present.jpg'),f2Pres,
       dpi = 800)

f2Fut=ggarrange(plotlist=list(fAspecFut,fArangeFut),nrow=2)
ggsave(paste0(dir_figs,'paper/maps/2plots_MB_Future.jpg'),f2Fut,
       dpi = 800)



############################ STATS #####################################
continent = c('af','ar','as','au','eu','gr','na','sa','si')

# interbasins due to dams
interbasins <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  tab <- readRDS(paste0('Dams_impact/proc/InterBasins_tab_GrG2FH_',cont,'.rds'))
  return(tab)
}

nrfrag <- interbasins %>% group_by(MAIN_BAS) %>% summarise(nrib_cur=max(INTER_ID_cur)+1, nrib_fut=max(INTER_ID_fut)+1)
nrfrag$increase <- nrfrag$nrib_fut-nrfrag$nrib_cur
# how many basins are fragmented by current dams?
nrow(nrfrag%>%filter(nrib_cur>1))

#how many basins will be fragmented by current and future dams?
nrow(nrfrag%>%filter(nrib_fut>1))

# in how many basins will fragmentation increase?
nrow(nrfrag %>%filter(increase>0))

nrfrag$cl = NA
nrfrag$cl[which(nrfrag$nrib_fut==1)] <- 1
nrfrag$cl[which(nrfrag$nrib_cur>1 & nrfrag$increase==0)] <- 2
nrfrag$cl[which(nrfrag$nrib_cur>1 & nrfrag$increase>0)] <- 3
nrfrag$cl[which(nrfrag$nrib_cur==1 & nrfrag$nrib_fut>1)] <- 4
#nrfrag$cl <- factor(nrfrag$cl, levels=c("Not fragmented","Fragmented by current dams", "Fragmented by current and future dams", "Fragmented by future dams" ))
#nrfrag$cl <- factor(nrfrag$cl)
#show on a map
nrfrag_sf <- left_join(tab_sf,nrfrag) %>%st_as_sf() %>%select(cl)
#st_write(nrfrag_sf,paste0(dir_figs,'paper/maps/basinsinfo.gpkg'))

#cant plot it unfortunately with discrete scale....
ggplot() +
  #geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  #geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  #geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = nrfrag_sf[,] , aes(fill=cl), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
  #geom_sf(data = nrfrag_sf %>% filter(!is.na(occ))%>% filter(frag_fut==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
  #facet_grid(scenario~.)+
  #labs(y=element_blank(),x=element_blank(), fill= "Total PLR (km²)" ) +
  #scale_y_continuous(position = "right")+
  scale_fill_manual(values=c("red","blue","yellow","green"))
  scale_fill_viridis_c(
    #breaks = seq(0,70000,10000),
    #limits = c(0,70000),
    #labels= format(seq(0,70000,10000), big.mark = ",", scientific = FALSE),
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
############################## GARBAGE ###########################################


# just one map
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = tab_sf %>% filter(!is.na(occ)),  aes(fill=PAFspec_cur), lwd=0) + # filter out basins for which we have no species data (they will be grey)
  geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="white", lwd=0) + # overlay of basins with species data but without dams
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


ggsave(paste0(dir_figs,'paper/maps/test.jpg'),p)



for(v in "Pextinct_spec"){ # do it separately to set limits on scales
  tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
  tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")), 
             #tab_sf_noNA[,paste0(v,"_fut")] %>% rename("value"=paste0(v,"_fut")), 
             tab_sf_noNA[,paste0(v,"_diff")] %>% rename("value"=paste0(v,"_diff")))
  tfig$scenario = rep(c("Present", #"Future", 
                        "Future increase"),each=nrow(tab_sf_noNA))
  tfig$scenario = factor(tfig$scenario, levels= c("Present", #"Future", 
                                                  "Future increase"))
  
  # free scales for facets are not supported using sf, so make two separate plots and arrange
  
  # present
  p1 <- ggplot() +
    geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
    geom_sf(data = world, fill = "grey90", lwd = NA) +
    geom_sf(data = tfig %>% filter(scenario=="Present"),  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
    geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(Pextinct_spec_cur==0),  fill="black", lwd=NA) +
    #facet_grid(scenario~.)+
    labs(y="Present",x=element_blank()) +
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
ggsave(paste0(dir_figs,'paper/maps/test_Pextinct_spec.jpg'),p1,dpi=500)

