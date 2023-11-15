#DI figures (paper)
# make figures for DI paper
# REGIONAL

# Tamara Keijzer
# april 2023

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")


library(ggplot2)
library(ggpubr)
library(tidyr)
library(cowplot)

colors=c('#f1a340', "#998ec3")

# species number, name, traits data
# species traits info
traits <- read.csv('Fishdata/species_traits_extra.csv')
ids <- traits %>% dplyr::select(id_no,binomial)




# function to get the true mean when the y scale is log transformed (otherwise mean of transformed values is shown)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Summarise y values at unique/binned x
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stat_summary_two <- function(mapping = NULL, data = NULL,
                             geom = "pointrange", position = "identity",
                             ...,
                             fun.data = NULL,
                             fun.y = NULL,
                             fun.ymax = NULL,
                             fun.ymin = NULL,
                             fun.args = list(),
                             na.rm = FALSE,
                             transform.after.summary = TRUE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryTwo,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      fun.y = fun.y,
      fun.ymax = fun.ymax,
      fun.ymin = fun.ymin,
      fun.args = fun.args,
      na.rm = na.rm,
      transform.after.summary = transform.after.summary,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ggproto Stat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

StatSummaryTwo <- ggproto(
  "StatSummaryTwo", Stat,
  required_aes = c("x", "y"),
  
  compute_panel = function(data, scales, fun.data = NULL, fun.y = NULL,
                           fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                           na.rm = FALSE, transform.after.summary = TRUE) {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The `data` we have in this function has already been transformed, so
    # let's untransform it
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (transform.after.summary) {
      data$y <- scales$y$trans$inverse(data$y)
    }
    
    fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
    res <- ggplot2:::summarise_by_x(data, fun)
    
    if (transform.after.summary) {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Transform the summary of the raw data into the final scale
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      res$y    <- scales$y$trans$transform(res$y)
      res$ymin <- scales$y$trans$transform(res$ymin)
      res$ymax <- scales$y$trans$transform(res$ymax)
    }
    
    res
  }
)

# loops over region and dams datasets
regs=c("BRA","USA","MEK")
damsdatasets=c("REG","REG_large","GDATonly","GDATonly_15m","BOTH","BOTH_large")

for(region in regs){
  for(damsdata in damsdatasets){
    
    
    dir_(paste0('Dams_impact/figs/',region,'/'))
    dir_(paste0('Dams_impact/figs/',region,'/',damsdata))
    dir_figs <- dir_(paste0('Dams_impact/figs/',region,'/',damsdata,'/',mvrs_type,'/'))
    dir_(paste0(dir_figs,"paper/"))
    dir_(paste0(dir_figs,"paper/extra/"))
    dir_(paste0(dir_figs,"paper/maps/"))
    dir_(paste0(dir_figs,"paper/maps/extra/"))
    
    #dir_tabs <- dir_('Dams_impact/tabs/')
    
    #processed data
    dir_proc_out <- paste0('Dams_impact/proc/',region,'/',damsdata,'/model_DI_occurrence/',mvrs_type,'/') # specific for relation
    ###
    
    #directories
    dir_out_tabs <- (paste0(dir_proc_out,"tabs/"))
    
    
    ####################
    #### PERSPECIES ####
    ####################
    
    # PLR (%/km) division
    #PLR
    out <- paste0(dir_out_tabs,'PLRfragm_tabDI_fwonly.csv')
    PLR <- read.csv(out)
    
    #remove species (n=9) that already are fully endangered without dams
    PLR <- PLR %>% filter(area_adjusted_km2!=0)
    
    dp=ggplot(PLR %>%filter(PLRkm_curdams>0), aes(x=log10(PLRkm_curdams), y=log10(PLR_curdams)) ) + 
      geom_bin2d(bins = 50) + #TODO
      scale_fill_continuous(type = "viridis",trans="log10") +
      #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
      xlab(expression(' PLR (km²)')) + ylab(expression(' PLR (%)')) +
      scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2),labels = c("0.0001","0.001","0.01","0.1","1","10","100"), limits=c(-4.5,2)) +
      scale_x_continuous(breaks=c(0,1,2,3,4,5),labels=c("1","10","100","1.000","10.000","100.000"), limits=c(0,5)) +
      theme_bw() +
      theme(text = element_text(size = 30))
    
    ggsave(paste0(dir_figs,'paper/PLRkm_vs_PLRperc_current.jpg'),dp, width = 12, height = 7)
    
    
    
    #get values for df
    impPLRperc=PLR%>%
      #filter(PLR_curdams>=1) %>% 
      pull(PLR_curdams)
    
    impPLRkm=PLR%>%
      #filter(PLR_curdams>=1) %>% 
      pull(PLRkm_curdams)
    
    # prepare data
    figPLR=data.frame(PLR=c(impPLRperc, 
                            impPLRkm),
                      scenario=c(rep("Present",length(impPLRperc)), rep("Present",length(impPLRkm))),
                      var=c(rep("PLR (%)",length(impPLRperc)),rep("PLR (km²)",length(impPLRkm)))
    )
    
    # PLR percentage
    # with zeros
    f1a1= ggplot(figPLR %>% filter(var=="PLR (%)"), aes(y=PLR, x=scenario)) + #TODO
      geom_violin(aes(fill = scenario),lwd = .5,color='transparent')+
      geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.9)+
      scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
      theme_bw() +
      coord_cartesian(ylim = c(0,99)) +
      labs(y="PLR (%)",x=element_blank()) +
      scale_fill_manual(values=colors) +
      stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
      theme(
        legend.position="none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed',color='grey'),
        axis.ticks.x = element_blank(),
        text = element_text(size=10),
        axis.text.x = element_text(color='black',vjust = 0),
        axis.text.y = element_text(color='black',hjust = 0.5),
        axis.line.y = element_line(color='black'),
        # axis.line.y.right = element_line(),
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        # , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        # , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        , strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(angle = 0, size = 16)
      )
    
    ggsave(paste0(dir_figs,'paper/PLR_Pres_incl0.jpg'),f1a1,
           width = 90,height = 60,dpi = 600,units = 'mm')
    
 
    ############################### STATS ##########################################
    
    # stats in txt file
    
    # save stats in txt file in dir_proc_out
    write.csv(PLR[order(PLR$PLR_curdams,decreasing=TRUE),], paste0(dir_figs,"PLR_perc_top.csv"), row.names = F)
    write.csv(PLR[order(PLR$PLRkm_curdams,decreasing=TRUE),], paste0(dir_figs,"PLR_km_top.csv"), row.names = F)
    fileConn<-file(paste0(dir_figs,"stats.txt"))
    
    writeLines(c(paste0("Relation type: ", mvrs_type,", Dams data: ", dams_used),
                 paste0(sum(PLR %>% pull(fragmented_cur))," species experience fragmentation, of all species (n= ",nrow(PLR),"): ",sum(PLR %>% pull(fragmented_cur))/nrow(PLR)*100,"%"),
                 paste0("Of all main basins, ", length(morethanonefragment), " exist of multiple interbasins (so are fragmented)"),
                 paste0(nrow(PLR %>% filter(PLRkm_curdams!=0))," species are affected by isolation (PLR>0), ",
                        nrow(PLR %>% filter(PLR_curdams>10))," species have PLR>10%, ", nrow(PLR %>% filter(PLR_curdams>5)), " species have PLR>5%, ",
                        nrow(PLR %>% filter(PLRkm_curdams>10000))," species have PLR>10.000km2, ", nrow(PLR %>% filter(PLRkm_curdams>1000)), " species have PLR>1.000km2" ),
                 paste0("Average PLR: ",mean(na.omit(impPLRkm))," km2 +- ",sd(na.omit(impPLRkm)), " SD or ", mean(na.omit(impPLRperc)), " % +- ", sd(na.omit(impPLRperc)), "SD") 
                 ), fileConn)
    close(fileConn)
  }
}