#DI maps (paper)
# make maps for DI paper
# different dams results together for regions (PAF subbasins)

# Tamara Keijzer
# july 2023



# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")

library(ggplot2)
library(ggpubr)
library(tidyr)
library(cowplot)
library(RColorBrewer)


colors=c('#f1a340', "#998ec3")


regs=c("BRA","USA","MEK")
damsdatasets=c("REG","REG_large","GDATonly","GDATonly_15m","BOTH","BOTH_large")
damsdata1="BOTH_large"
damsdata2="BOTH"

for(region in regs){
  

  ##################################
  ### first plot only large dams ###
  ##################################
    dir_(paste0('Dams_impact/figs/',region,'/'))
    dir_(paste0('Dams_impact/figs/',region,'/',damsdata1))
    dir_figs <- dir_(paste0('Dams_impact/figs/',region,'/',damsdata1,'/',mvrs_type,'/'))
    dir_(paste0(dir_figs,"paper/"))
    dir_(paste0(dir_figs,"paper/extra/"))
    dir_(paste0(dir_figs,"paper/maps/"))
    dir_(paste0(dir_figs,"paper/maps/extra/"))

    
    #processed data
    dir_proc_out <- paste0('Dams_impact/proc/',region,'/',damsdata1,'/model_DI_occurrence/',mvrs_type,'/') # specific for relation
    ###
    #directories
    dir_out_tabs <- (paste0(dir_proc_out,"tabs/"))
    
    
    #> FUNCTIONS AND BASE LAYERS -------------------------------------------------------------------------------------
    
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
    
    
    tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") ) # for main basin boundaries


    # hydrobasins data
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
    
    # load subbasin results
    sb <-readRDS(paste0(dir_out_tabs,'RESsubbasin_tabDI_fwonly.rds')) %>% as.data.frame() %>% select(-geometry)
    subb <- left_join(sb,hb_data) %>%st_as_sf()
    
    subb$PAF_cur=(subb$Aspec_cur/subb$occ)*100
    
    VOI = "PAF"
    # for PAF spec, try to adjust the color scale (rescale) to make diff more visible
    cols=viridis::plasma(100)
    
    # add main basin boundaries
    #remove background?
    # grey out main basins without species data or without dams
    
    mainbasins_noholes=nngeo::st_remove_holes(tab_sf) # remove weird holes in basins
    
    p2a= ggplot()+
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
    #ggsave(paste0(dir_figs,'paper/maps/NEW_SB_PAF.jpg'),p2, dpi = 800)
    
    if(region=="USA"){
      mainland=p2a+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))+theme(legend.position = 'none')
      alaska=p2a+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
      ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
      usplot_a=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.1, y = 0.1)
      #ggsave(paste0(dir_figs,'paper/maps/NEW_NEW_SB_PAF.jpg'),usplot, width = 10, height = 6)
    }
    
    
    
    
    
    #############################################
    ### second plot both large and small dams ###
    #############################################
    
    #figure only large dams
    dir_(paste0('Dams_impact/figs/',region,'/'))
    dir_(paste0('Dams_impact/figs/',region,'/',damsdata2))
    dir_figs <- dir_(paste0('Dams_impact/figs/',region,'/',damsdata2,'/',mvrs_type,'/'))
    dir_(paste0(dir_figs,"paper/"))
    dir_(paste0(dir_figs,"paper/extra/"))
    dir_(paste0(dir_figs,"paper/maps/"))
    dir_(paste0(dir_figs,"paper/maps/extra/"))
    
    
    #processed data
    dir_proc_out <- paste0('Dams_impact/proc/',region,'/',damsdata2,'/model_DI_occurrence/',mvrs_type,'/') # specific for relation
    ###
    #directories
    dir_out_tabs <- (paste0(dir_proc_out,"tabs/"))
    
    tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") ) # for main basin boundaries
    
    
    # load subbasin results
    sb <-readRDS(paste0(dir_out_tabs,'RESsubbasin_tabDI_fwonly.rds')) %>% as.data.frame() %>% select(-geometry)
    subb <- left_join(sb,hb_data) %>%st_as_sf()
    
    subb$PAF_cur=(subb$Aspec_cur/subb$occ)*100
    
    VOI = "PAF"
    
    # add main basin boundaries
    #remove background?
    # grey out main basins without species data or without dams
    
    mainbasins_noholes=nngeo::st_remove_holes(tab_sf) # remove weird holes in basins
    
    p2b= ggplot()+
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
    #ggsave(paste0(dir_figs,'paper/maps/NEW_SB_PAF.jpg'),p2, dpi = 800)
    
    if(region=="USA"){
      mainland=p2b+coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))+theme(legend.position = 'none')
      alaska=p2b+coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)+theme(legend.position = 'none')
      ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
      usplot_b=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.1, y = 0.1)
      #ggsave(paste0(dir_figs,'paper/maps/test.jpg'),usplot_b, width = 10, height = 6)
    }
    
    
    
    #############################################
    ###               ALL PLOTS               ###
    #############################################
    # figures are in the BOTH/mvrs_type/paper/maps folder of the region
    #combine plots into 1
    leg=get_legend(p2b)
    
    #next to each other except for USA
    if(region=="MEK"){
      figure2=ggarrange(p2a,p2b, 
                        labels = c("(a) ", "(b)"),
                        #vjust=15,
                        font.label = list(size = 12),
                        ncol = 2, nrow = 1,
                        #widths = c(1, 1.5),
                        common.legend=TRUE,
                        legend="bottom"
      )
      dev.off()
      ggsave(paste0(dir_figs,'paper/maps/Figure_S7.png'),figure2, height = 150, width = 178,
             dpi = 1000,units = 'mm')
    }
    
    if(region=="BRA"){
      figure2=ggarrange(p2a,p2b, 
                        labels = c("(a) ", "(b)"),
                        #vjust=15,
                        font.label = list(size = 12),
                        ncol = 2, nrow = 1,
                        #widths = c(1, 1.5),
                        common.legend=TRUE,
                        legend="bottom"
      )
      dev.off()
      ggsave(paste0(dir_figs,'paper/maps/Figure_S5.png'),figure2, height = 120, width = 178,
             dpi = 1000,units = 'mm')
    }
    
    if(region=="USA"){
      figure2=ggarrange(usplot_a,usplot_b, 
                        labels = c("(a) ", "(b)"),
                        font.label = list(size = 12),
                        ncol = 1, nrow = 2,
                        #widths = c(1, 1.5),
                        legend.grob=leg,
                        legend="bottom"
      )
      dev.off()
      ggsave(paste0(dir_figs,'paper/maps/Figure_S9.png'),figure2, height = 200, width = 178,
             dpi = 1000,units = 'mm')
    }
    
    
}