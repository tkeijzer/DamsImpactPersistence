#DI maps (paper)
# make maps for DI paper
# _onlycur points to taking only current stuff

# Tamara Keijzer
# april 2023

# ESH (old term) == PLR (new term)


# general
setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")
library(ggplot2)
library(ggpubr)
library(tidyr)
library(cowplot)

colors=c('#f1a340', "#998ec3")
colors2=c('#deebf7','#3182bd')
colors_bar=c('#f46d43','#f46d43', '#d73027','#a50026','#fee090',  '#4575b4')

regs=c("BRA","USA","MEK")
damsdatasets=c("REG","REG_large","GDATonly","GDATonly_15m","BOTH","BOTH_large")

for(region in regs){
  for(damsdata in damsdatasets){
    
    dir_(paste0('Dams_impact/figs/',region,'/'))
    dir_(paste0('Dams_impact/figs/',region,'/',damsdata))
    dir_figs <- dir_(paste0('Dams_impact/figs/',region,'/',damsdata,'/',mva_type,'/'))
    dir_(paste0(dir_figs,"paper/"))
    dir_(paste0(dir_figs,"paper/extra/"))
    dir_(paste0(dir_figs,"paper/maps/"))
    dir_(paste0(dir_figs,"paper/maps/extra/"))
    
    #dir_tabs <- dir_('Dams_impact/tabs/')
    
    #processed data
    dir_proc_out <- paste0('Dams_impact/proc/',region,'/',damsdata,'/model_DI_occurrence/',mva_type,'/') # specific for relation
    ###
    
    #directories
    dir_out_tabs <- (paste0(dir_proc_out,"tabs/"))
    
    
    #> FUNCTIONS AND BASE LAYERS -------------------------------------------------------------------------------------
    
    library(ggplot2)
    library(RColorBrewer)
    library(ggpubr)
    
    # base layers
    
    # bounding box for maps, just do BOI
    if(region =="BRA"){
      BOI_boundary=rnaturalearth::ne_countries(country = 'Brazil',returnclass = 'sf')
    }
    
    if(region =="MEK"){
      BOI_boundary=foreach(i = c('as'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp')) %>% # REGION SPECIFIC
        filter(MAIN_BAS %in% c(4120017020,4120023810,4120023060)) %>% st_union(.)
    }
    
    if(region=="USA"){
      BOI_boundary=rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf')
    }
    
    #> DATA -------------------------------------------------------------------------------------
    
    
    tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
    
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
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1) +
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
      
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'.jpg'),usplot, width = 10, height = 6)
                                    
      }
    }
    
    
    
    
    
    # create per variable
    # first for the variables with scales 0-1
    for(v in vars[1:2]){
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # and draw
      p <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'.jpg'),usplot, width = 10, height = 6)
        
      }
      
    }
    
    
    
    ########### DIFFERENT SCALES #######################################################
    #try different scales options (more colors low ranges)
    
    
    #myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
    #plot(scales::rescale((1:100)^1.5), col=myPalette(100))
    
    # for PAF spec, try to adjust the color scale (rescale) to make diff more visible
    cols=viridis::plasma(100)
    for(v in vars[3]){
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # and draw
      p <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'_differentscale20.jpg'),usplot, width = 10, height = 6)
        
      }
    }
    
    
    # test scale
    mybreaks=c(0,0.01,0.1,0.25,0.5,1)
    
    # for PAFrange use a log scale bar
    for(v in vars[4]){
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # and draw
      p <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'_logscale.jpg'),usplot, width = 10, height = 6)
        
      }
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
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      
      ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_to5perc.jpg'),p,
             dpi = 800)
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/NEW_MB_',v,'_to5perc.jpg'),usplot, width = 10, height = 6)
        
      }
    }
    
    for(v in vars[4]){
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      #change all values above 0.05 to 0.05 so we can have a nice scale
      tab_sf_noNA$PAFrange_cur[which(tab_sf_noNA$PAFrange_cur>0.05)] <-0.05
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # and draw
      p <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      
      ggsave(paste0(dir_figs,'paper/maps/MB_',v,'_to5perc_otherscale.jpg'),p,
             dpi = 800)
      
      if(region=="USA"){
        mainland=p+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/NEW_MB_',v,'_to5perc_otherscale.jpg'),usplot, width = 10, height = 6)
        
      }
    }
    
    
    
    
    # absolute values Aspec, Arange
    # NOTE: THE LIMITS, BREAKS, AND LABELS ARE SET MANUALLY
    
    
    for(v in vars[1]){ # do it separately to set limits on scales
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # free scales for facets are not supported using sf, so make two separate plots and arrange
      
      # present
      p1 <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      
      if(region=="USA"){
        mainland=p1+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p1+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'_setscale.jpg'),usplot, width = 10, height = 6)
        
      }
      
      
    }
    
    
    for(v in vars[2]){ # do it separately to set limits on scales
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # free scales for facets are not supported using sf, so make two separate plots and arrange
      
      # present
      p1 <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
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
      
      if(region=="USA"){
        mainland=p1+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
        alaska=p1+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
        ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
        usplot=ggdraw(mainland) +
          draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                    x = 0.1, y = 0.1)
        ggsave(paste0(dir_figs,'paper/maps/extra/NEW_MB_',v,'_setscale.jpg'),usplot, width = 10, height = 6)
        
      }
      
    }
    
    
    
    
    
    
    
    
    ############################ STATS #####################################
    # get table with some stats where most stuff happens
    PAFrange_top <- tab_sf[order(tab_sf$PAFrange_cur,decreasing=TRUE),]
    PAFrange_top <- PAFrange_top %>%as.data.frame()%>%select(-geom)
    write.csv(PAFrange_top,paste0(dir_figs,"stats_PAFrange_MBtop.csv"), row.names = F)
    
    
    
    
   
    for(v in "Pextinct_spec"){ # do it separately to set limits on scales
      tab_sf_noNA=tab_sf%>% filter(!is.na(occ))
      tfig=rbind(tab_sf_noNA[,paste0(v,"_cur")] %>% rename("value"=paste0(v,"_cur")))
      
      # free scales for facets are not supported using sf, so make two separate plots and arrange
      
      # present
      p1 <- ggplot() +
        geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
        geom_sf(data = tfig,  aes(fill=value), lwd=NA) + # filter out basins for which we have no species data (they will be grey)
        geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay of basins with species data but without dams
        geom_sf(data = tab_sf %>% filter(!is.na(occ))%>% filter(Pextinct_spec_cur==0),  fill="black", lwd=NA) +
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
    ggsave(paste0(dir_figs,'paper/maps/extra/Pextinct_spec.jpg'),p1,dpi=500)
    
    if(region=="USA"){
      mainland=p1+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
      alaska=p1+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
      ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
      usplot=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.1, y = 0.1)
      ggsave(paste0(dir_figs,'paper/maps/extra/Pextinct_spec.jpg'),usplot, width = 10, height = 6)
      
    }
    
    
    
    
    
    
    #################### SUBBASIN MAPS ######################################
    if(region =="BRA"){
      # read hydrobasins data
      hb_data <- foreach(i = c('sa'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
    }
    
    if(region =="MEK"){
      hb_data <- foreach(i = c('as'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
    }
    
    if(region=="USA"){
      hb_data <- foreach(i = c('na','ar'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp'))
    }
    
    
    sb <-readRDS(paste0(dir_out_tabs,'SR_tabDI_fwonly.rds')) %>% as.data.frame() %>% select(-geometry)
    subb <- left_join(sb,hb_data) %>%st_as_sf()
    
    subb$PAF_cur=(subb$Aspec_cur/subb$occ)*100
    
    VOI = "PAF"
    
    # add main basin boundaries
    #remove background?
    # grey out main basins without species data or without dams
    
    mainbasins_noholes=nngeo::st_remove_holes(tab_sf) # remove weird holes in basins
    
    p2= ggplot()+
      #geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
      geom_sf(data= subb, aes(fill=PAF_cur), lwd=NA) +
      
      scale_fill_gradientn(colours =rev(heat.colors(100)),
                           breaks = seq(0,100,10),
                           labels = c(">0",seq(10,100,10)),
                           limits = c(0,100),) +
      geom_sf(data= subb %>% filter(PAF_cur==0), fill="grey10", lwd=NA) +
      geom_sf(data= subb %>% filter(is.na(occ)), fill=" grey90", lwd=NA) + # no species occurrence subbasins
      geom_sf(data = mainbasins_noholes %>% filter(!is.na(occ))%>% filter(frag_cur==F),  fill="grey50", lwd=NA) + # overlay mb without dams
      
      geom_sf(data = mainbasins_noholes ,  fill=NA, lwd=0.25, col="#2166ac") + # overlay main basin boundaries
      #geom_sf(data=BOI_boundary, fill= NA, color = "black", lwd=0.1)+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(4,'line'),
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )
    ggsave(paste0(dir_figs,'paper/maps/NEW_SB_PAF.jpg'),p2,
           dpi = 800)
    
    if(region=="USA"){
      mainland=p2+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
      alaska=p2+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
      ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
      usplot=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.1, y = 0.1)
      ggsave(paste0(dir_figs,'paper/maps/NEW_NEW_SB_PAF.jpg'),usplot, width = 10, height = 6)
      
    }
    
    
    
  }
}