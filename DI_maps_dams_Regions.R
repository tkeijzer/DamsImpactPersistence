# maps of dams in regions

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

#general data
# load dams datasets (global and regional)
#global
gdat <- st_read(file_gdat_dams) #35,140, 3,360 geometries empty
gdat <- gdat %>% filter(!is.na(Long)) #31,780

# get hydrography lines from hydrorivers
rivers <- read_sf('HydroRivers/HydroRIVERS_v10_shp/HydroRIVERS_v10.shp')

damsdata = "BOTH"
for(region in regs){
  #for(damsdata in damsdatasets){
    
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
      AOI_boundary=rnaturalearth::ne_countries(country = 'Brazil',returnclass = 'sf')
    }
    
    if(region =="MEK"){
      AOI_boundary=foreach(i = c('as'),.combine = 'rbind') %do% read_sf(paste0(dir_hybas12,'/hybas_lake_',i,'_lev12_v1c.shp')) %>% # REGION SPECIFIC
        filter(MAIN_BAS %in% c(4120017020,4120023810,4120023060)) %>% st_union(.)
    }
    
    if(region=="USA"){
      AOI_boundary=rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf')
    }
    
    #> DATA -------------------------------------------------------------------------------------
    
    
    # load dams datasets (global and regional)
    #global
        # select dams in AOI 
    logi_point_in_pol <- st_intersects(AOI_boundary, gdat, sparse = FALSE)
    AOI_dams_global <- gdat[as.vector(logi_point_in_pol), ]
    
    #regional
    regdams=st_read(paste0('Dams_impact/proc/dams_',region,'.gpkg'))
    
    logi_point_in_pol <- st_intersects(AOI_boundary,regdams, sparse = FALSE)
    regdams <- regdams[as.vector(logi_point_in_pol), ]
    
    # divide large and small based on attributes
    if(region =="BRA"){
      regdams_large= regdams %>% filter(Tipo_1=="UHE")
      regdams_small= regdams %>% filter(Tipo_1=="PCH")
    }
    
    if(region =="MEK"){
      regdams_large= regdams %>% filter(Height.m>=15)
      regdams_small= regdams %>% filter(Height.m<15 | is.na(Height.m))
    }
    
    if(region=="USA"){
     regdams_large= regdams %>% filter(height_m>=15)
     regdams_small= regdams %>% filter(height_m<15 | is.na(height_m))
    }
    
    
    # get hb data for only the region (main basin boundaries as well, perhaps use mbres_onlyfw)
    tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
    mainbasins_noholes=nngeo::st_remove_holes(tab_sf)
    
    
    # select hydrobasins (nolakes) in AOI to select rivers
    if(region =="BRA"){
      sb_nolakes <- foreach(i = c('sa'),.combine = 'rbind') %do% read_sf(paste0('Hybas_standardL12/hybas_',i,'_lev12_v1c.shp'))
      sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'Brazil',returnclass = 'sf'),"ESRI:54009"),
                           st_transform(sb_nolakes,"ESRI:54009"),
                           sparse = T)
      sb_nolakes <- sb_nolakes[sel[[1]],]
    }
    
    if(region =="MEK"){
      sb_nolakes <- foreach(i = c('as'),.combine = 'rbind') %do% read_sf(paste0('Hybas_standardL12/hybas_',i,'_lev12_v1c.shp')) %>%
        filter(MAIN_BAS %in% c(4120017020,4120023810,4120023060)) #ID of Mekong, Irrawaddi, Salween
    }
    
    if(region=="USA"){
      sb_nolakes <- foreach(i = c('na','ar'),.combine = 'rbind') %do% read_sf(paste0('Hybas_standardL12/hybas_',i,'_lev12_v1c.shp'))
      # extract only US HB units
      sel <- st_intersects(st_transform(rnaturalearth::ne_countries(country = 'United States of America',returnclass = 'sf'),"ESRI:54009"),
                           st_transform(sb_nolakes,"ESRI:54009"),
                           sparse = T)
      sb_nolakes <- sb_nolakes[sel[[1]],]
    }
    
    AOI_riv <- rivers %>% filter(HYBAS_L12 %in% sb_nolakes$HYBAS_ID)
    
    # make figure with layers
    # subbasins grey fill and main basin boundaries black
    # rivers on top
    # dams global and regional on top
    
    p2= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      geom_sf(data=AOI_dams_global, col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) +
      geom_sf(data=regdams, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) +
      scale_fill_manual(labels = c("GDAT", "Regional data"), values= c(alpha("red",0.5),alpha("white",0.5))) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )
      
    ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'.jpg'),p2,
           dpi = 800)
    
    if(region=="USA"){
    mainland= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      geom_sf(data=AOI_dams_global, col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=0.5) +
      geom_sf(data=regdams, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=0.5) +
      scale_fill_manual(labels = c("GDAT", "Regional data"), values= c(alpha("red",0.5),alpha("white",0.5))) +
      #coord_sf(xlim = c(-124.7,-67), ylim = c(24.5,49.3)) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )+
      coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
    #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_mainland.jpg'),mainland, dpi = 800)
    
    alaska= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      geom_sf(data=AOI_dams_global, col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) +
      geom_sf(data=regdams, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) +
      scale_fill_manual(labels = c("GDAT", "Regional data"), values= c(alpha("red",0.5),alpha("white",0.5))) +
      #coord_sf(xlim = c(-179.43,-124.7), ylim = c(51.229,71.35)) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            #strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )+
      coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)
    #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_alaska.jpg'),alaska,dpi = 800)
    
    
    (ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
    
    test=ggdraw(mainland) +
      draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                x = 0.05, y = 0.05)
    ggsave(paste0(dir_figs,'paper/maps/NEW_Dams_',damsdata,'.jpg'),test, width = 10, height = 6)
    }
    
    
    
    # large vs small
    # TODO  > check if dams are placed correctly, addjust fill colors?, proper legend. if done, also adjust in extra US figures.
    if(region=="MEK"){hfig=150}
    if(region=="BRA"){hfig=120}
    if(region=="USA"){hfig=120}
    
    
    p2= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), aes(fill= alpha("red",0.5)), pch=21, size=1) + # large global
      geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), aes(fill= alpha("white",0.5)), pch=21, size=1) + # small global
      geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
      geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
      geom_sf(data = mainbasins_noholes,  fill=NA, color = "grey20", lwd=0.25) + # main basins as background
      scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            legend.key = element_rect(fill = "grey70",), #otherwise the white dot is not visible
            axis.text = element_blank(),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )
    
    ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_size.jpg'),p2,
           dpi = 1000, width = 178, height=hfig,units = 'mm')
    
    ps= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) + # large global
      #geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=1) + # small global
      geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
      #geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
      #scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            #legend.key = element_rect(fill = "grey90",),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )
    ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_large.jpg'),ps,
           dpi = 800)
    
    ps= ggplot()+
      geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
      geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
      scale_size_identity() +
      #geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), aes(fill= alpha("red",0.5)), pch=21, size=1) + # large global
      geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=1) + # small global
      #geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
      geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
      #scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
      #labs(y=basinname,x=element_blank()) +
      scale_y_continuous(position = "left")+
      theme_minimal() +
      theme(text = element_text(size = 12),
            panel.grid.major = element_line(color=NA),
            axis.text = element_blank(),
            #legend.key = element_rect(fill = "grey90",),
            #axis.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(3,'line'), #adjust legend size
            strip.background = element_rect('white'),
            strip.background.x = element_blank(),
            strip.background.y = element_blank(),
            strip.text = element_text(angle = 0, vjust = -1, size = 12),
            legend.title = element_blank()
      )
    ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_small.jpg'),ps,
           dpi = 800)
    
    
    
    if(region=="USA"){
      mainland= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), aes(fill= alpha("red",0.5)), pch=21, size=0.75) + # large global
        geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), aes(fill= alpha("white",0.5)), pch=21, size=0.75) + # small global
        geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=0.75) + # large regional
        geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=0.75) + # small regional
        geom_sf(data = mainbasins_noholes,  fill=NA, color = "grey20", lwd=0.25) + # main basins as background
        scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-124.7,-67), ylim = c(24.5,49.3)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              legend.key = element_rect(fill = "grey70",),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_mainland.jpg'),mainland, dpi = 800)
      
      alaska= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) + # large global
        geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=1) + # small global
        geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
        geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
        geom_sf(data = mainbasins_noholes,  fill=NA, color = "grey20", lwd=0.25) + # main basins as background
        scale_fill_manual(values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-179.43,-124.7), ylim = c(51.229,71.35)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              #strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_alaska.jpg'),alaska, dpi = 800)
      
      
      (ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
      
      usplot=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.06, y = 0.13)
      ggsave(paste0(dir_figs,'paper/maps/NEW_Dams_',damsdata,'_size.jpg'),usplot,dpi = 1000, width = 178, height=hfig, units = 'mm' )
      
      
      #just large dams
      mainland= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=0.75) + # large global
        #geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), aes(fill= alpha("white",0.5)), pch=21, size=0.75) + # small global
        geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=0.75) + # large regional
        #geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=0.75) + # small regional
        #scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-124.7,-67), ylim = c(24.5,49.3)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              #legend.key = element_rect(fill = "grey90",),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_mainland.jpg'),mainland, dpi = 800)
      
      alaska= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) + # large global
        #geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=1) + # small global
        geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
        #geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
        #scale_fill_manual(values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-179.43,-124.7), ylim = c(51.229,71.35)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              #strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_alaska.jpg'),alaska, dpi = 800)
      
      
      (ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
      
      usplot=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.05, y = 0.05)
      ggsave(paste0(dir_figs,'paper/maps/NEW_Dams_',damsdata,'_large.jpg'),usplot, width = 10, height = 6)
      
      
      
      mainland= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        #geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), aes(fill= alpha("red",0.5)), pch=21, size=0.75) + # large global
        geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=0.75) + # small global
        #geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=0.75) + # large regional
        geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=0.75) + # small regional
        #scale_fill_manual(labels = c("Large", "Small"), values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-124.7,-67), ylim = c(24.5,49.3)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              #legend.key = element_rect(fill = "grey90",),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_mainland.jpg'),mainland, dpi = 800)
      
      alaska= ggplot()+
        geom_sf(data = mainbasins_noholes,  fill="grey70", color = "grey20", lwd=0.25) + # main basins as background
        geom_sf(data = AOI_riv %>% filter(ORD_STRA >= 5), color = "grey40", aes(lwd=ORD_STRA/50) ) +
        scale_size_identity() +
        #geom_sf(data=AOI_dams_global %>% filter(Height>=15), col=alpha("grey10",0.01), fill= alpha("red",0.5), pch=21, size=1) + # large global
        geom_sf(data=AOI_dams_global %>% filter(Height<15|is.na(Height)), col=alpha("grey10",0.01), fill= alpha("white",0.5), pch=21, size=1) + # small global
        #geom_sf(data=regdams_large, col=alpha("grey10",0.01), fill= alpha("red",0.5) , pch=21, size=1) + # large regional
        geom_sf(data=regdams_small, col=alpha("grey10",0.01), fill= alpha("white",0.5) , pch=21, size=1) + # small regional
        #scale_fill_manual(values= c(alpha("red",0.5),alpha("white",0.5),alpha("red",0.5),alpha("white",0.5))) +
        #coord_sf(xlim = c(-179.43,-124.7), ylim = c(51.229,71.35)) +
        #labs(y=basinname,x=element_blank()) +
        scale_y_continuous(position = "left")+
        theme_minimal() +
        theme(text = element_text(size = 12),
              panel.grid.major = element_line(color=NA),
              axis.text = element_blank(),
              #axis.title = element_blank(),
              legend.position = 'bottom',
              legend.key.width = unit(3,'line'), #adjust legend size
              #strip.background = element_rect('white'),
              strip.background.x = element_blank(),
              strip.background.y = element_blank(),
              strip.text = element_text(angle = 0, vjust = -1, size = 12),
              legend.title = element_blank()
        )+
        coord_sf(crs = st_crs(3467), xlim = c(-1600000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA)
      #ggsave(paste0(dir_figs,'paper/maps/Dams_',damsdata,'_alaska.jpg'),alaska, dpi = 800)
      
      
      (ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
      
      usplot=ggdraw(mainland) +
        draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
                  x = 0.05, y = 0.05)
      ggsave(paste0(dir_figs,'paper/maps/NEW_Dams_',damsdata,'_small.jpg'),usplot, width = 10, height = 6)
      
      
    }
    
    
    
  #}
}