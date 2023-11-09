# zoom in on selected main basins

# make maps for DI paper

# Tamara Keijzer
# feb 2023


setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")


colors=c('#f1a340', "#998ec3")
colors2=c('#deebf7','#3182bd')
colors_bar=c('#f46d43','#f46d43', '#d73027','#a50026','#fee090',  '#4575b4')
colors3=c('#f1a340',"red")

#directories
dir_out <- dir_(paste0(dir_proc,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc,"tabs/"))

#> FUNCTIONS AND BASE LAYERS -------------------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(spatialEco)



 #plot 1 data
# main basins (for boundary)
mb <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )

#dams this is the original location, so not to the downstream border of the subbasin in which they occur
dams_cur <- read_sf('Dams_impact/proc/dams_current.gpkg')
dams_cur$scenario <- "Present"
dams_fut <- read_sf('Dams_impact/proc/dams_future.gpkg') 
dams_fut$scenario <- "Future"
alldams <- rbind(dams_cur %>% select(scenario), dams_fut %>% select(scenario))
alldams$scenario <- factor(alldams$scenario, levels=c("Present", "Future"))

# hydrorivers
rivers <- read_sf('HydroRivers/HydroRIVERS_v10_shp/HydroRIVERS_v10.shp') 
# need hydrobasins without lakes to get rivers within main basin
sb_nolakes <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0('Hybas_standardL12/hybas_',cont,'_lev12_v1c.shp')) %>% as.data.frame() %>% dplyr::select(HYBAS_ID,MAIN_BAS)
  return(poly)
}

#plot 2 data subbasins or interbasins
sb_sf <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp')) %>% dplyr::select(HYBAS_ID,MAIN_BAS, SUB_AREA)
  return(poly)
}
sb <-readRDS(paste0(dir_out_tabs,'SR_tabDI_fwonly.rds'))
subb <- left_join(sb,sb_sf)

subb$PAF_cur=(subb$Aspec_cur/subb$occ)*100
subb$PAF_fut=(subb$Aspec_fut/subb$occ)*100
subb$PAF_diff=subb$PAF_fut-subb$PAF_cur

#ib_cur=read_sf(paste0(dir_out_tabs,"PAFinterbasins_cur_fwonly.gpkg"))
#ib_fut=read_sf(paste0(dir_out_tabs,"PAFinterbasins_fut_fwonly.gpkg"))

#simple plots
#ib_curplot= ib_cur %>%filter(occ!=0)
# PAF vs. fragment size
#plot(log10(ib_curplot$ib_area ),ib_curplot$PAFcur, xlab="Log10(Fragment size) sqkm", ylab="PAF species")


#> CHOOSE A BASIN -------------------------------------------------------------------------------------

#BOI <- 4120027941  #main basin number MAIN_BAS in hydrobasins (with lakes)
#basinname <- "Krishna" # CHANGE THIS MANUALLY Basin name


# 4120009880 #Yangtze
# 4120025450 #Ganges
# 2120008491 #Danube # 2120008490 (no lakes)
# 7120047061 #Mississipi
# 6120007000 #Amazon
# 4120027941 #Krishna

BOIs=c(4120027941,4120009880,2120008491,4120025450,7120047061,6120007000, 4120017020)
basinnames=c("Krishna","Yangtze","Danube","Ganges","Mississipi","Amazon","Mekong")
# CHOOSE VARIABLE OF INTEREST
VOI = "PAF"



for(i in 1:length(BOIs)){

  BOI=BOIs[i]
  basinname=basinnames[i]
  
  #> Get data for BOI -------------------------------------------------------------------------------------
  
  # get boundary
  BOI_boundary <- mb %>%filter(MAIN_BAS==BOI)
  
  #get vector with subbasin numbers
  BOI_sb <- sb_sf%>% as.data.frame() %>% filter(MAIN_BAS == BOI) %>% pull(HYBAS_ID)
  
  #get vector with subbasin numbers from the hydrobasins standard without lakes l12
  BOI_sb_nolakes <- sb_nolakes %>% filter(MAIN_BAS == BOI) %>% pull(HYBAS_ID)
  if(length(BOI_sb_nolakes)==0){ # for the Hydrobasins without lakes the main basin numbers are sometimes different
    BOI_sb_nolakes <- sb_nolakes %>% filter(MAIN_BAS == plyr::round_any(BOI,10)) %>% pull(HYBAS_ID)}
  
  #get river outline
  BOI_riv <- rivers %>% filter(HYBAS_L12 %in% BOI_sb_nolakes)
  
  # sf of dams current and future
  logi_point_in_pol <- st_intersects(BOI_boundary, alldams, sparse = FALSE)
  BOI_dams<- alldams[as.vector(logi_point_in_pol), ]
  
  #data to display (subbasin level)
  subres_BOI <- subb %>% filter(HYBAS_ID %in% BOI_sb) %>% st_as_sf()
  
  
  
  subres_BOI_fig <- rbind(subres_BOI %>% select(paste0(VOI,"_cur")) %>% rename(value=1),
                          subres_BOI %>% select(paste0(VOI,"_diff")) %>% rename(value=1)
  )
  subres_BOI_fig$scenario <- rep(c("Present","Future increase"), each= nrow(subres_BOI))
  subres_BOI_fig$scenario <- factor(subres_BOI_fig$scenario , levels=c("Present","Future increase"))
  
  #> Make figures for BOI -------------------------------------------------------------------------------------
  
  
  p1= ggplot()+
    geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
    geom_sf(data=BOI_riv %>% filter(ORD_STRA >= 5), color = "grey50", aes(lwd=ORD_STRA/10) )+
    scale_size_identity() +
    geom_sf(data=BOI_dams, aes(fill=scenario), col=alpha("grey20",0.5), pch=21, size=1)+
    scale_fill_manual(values= c(alpha(colors3[1],0.5),alpha(colors3[2],0.75))) +
    labs(y=basinname,x=element_blank()) +
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
  
  p2= ggplot()+
    geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
    geom_sf(data= subres_BOI_fig, aes(fill=value), lwd=NA) +
    scale_fill_gradientn(colours =rev(heat.colors(100)),
                         breaks = seq(0,100,10),
                         labels = c(">0",seq(10,100,10)),
                         limits = c(0,100),) +
    facet_grid(~scenario)+
    geom_sf(data= subres_BOI %>% filter(is.na(occ)), fill=" grey90", lwd=NA) +
    geom_sf(data= subres_BOI_fig %>% filter(value==0), fill="grey10", lwd=NA) +
    geom_sf(data=BOI_boundary, fill= NA, color = "black", lwd=0.1)+
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
  
  
  
  p12=ggarrange(plotlist =  list(p1,p2),ncol=2,widths=c(1,2))
  ggsave(paste0(dir_figs,'paper/maps/basins/',basinname,'_PAF.jpg'),p12,
         dpi = 800)


}
















































#garbage

p3=ggplot()+
  geom_sf(data=BOI_boundary, fill= "grey90", color = "grey20", lwd=0.1)+
  geom_sf(data= subres_BOI, aes(fill=PAF_diff), lwd=NA) +
  scale_fill_gradientn(colours =rev(heat.colors(100)),
                       breaks = seq(0,100,10),
                       labels = c(">0",seq(10,100,10)),
                       limits = c(0,100),) +
  geom_sf(data= subres_BOI %>% filter(is.na(occ)), fill=" grey90", lwd=NA) +
  geom_sf(data= subres_BOI %>% filter(PAF_diff==0), fill="grey10", lwd=NA) +
  geom_sf(data=BOI_boundary, fill= NA, color = "black", lwd=0.1)+
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

  
  
  
  
  
  
  
