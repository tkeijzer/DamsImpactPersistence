#DI figures (paper)
# make figures for DI paper

# Tamara Keijzer
# feb 2023

# ESH (old term) == PLR (new term)

# general
setwd("I:/tamara/CC_dams_R")
source("SETTINGS_Dams_impact.R")
library(ggplot2)
library(ggpubr)
library(tidyr)

colors=c('#f1a340', "#998ec3")
colors2=c('#deebf7','#3182bd')
colors_bar=c('#f46d43','#f46d43', '#d73027','#a50026','#fee090',  '#4575b4')

#directories
dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))
dir_fishsuit="I:/tamara/FishSuit/"
dir_threattab <- "I:/tamara/CC_dams_R/Fishdata/IUCN_DamsThreat.csv"

# species number, name, traits data
ids <- read_sf(paste0(dir_fishsuit,'proc/species_ranges_iucn_hybaslakes8.gpkg')) %>% as_tibble() %>% dplyr::select(id_no,binomial)
traits <- read.csv(paste0(dir_fishsuit,"proc/species_traits_extra.csv")) # from fishsuit, species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no) # may be iucn id so remove.

threattab <- read.csv(dir_threattab) # whether dams are indicated as a threat




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













####################
#### PERSPECIES ####
####################





# ESH (%/km) division
#ESH
out <- paste0(dir_out_tabs,'ESH_tabDI_fwonly2.csv')
ESH <- read.csv(out)

#remove species (n=9) that already are fully endangered wihtout dams
ESH <- ESH %>% filter(area_adjusted_km2!=0)

ESH$diffperc=ESH$ESH_futdams-ESH$ESH_curdams
ESH$diffkm=ESH$ESHkm_futdams-ESH$ESHkm_curdams


dp=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('Log'[10]*' PLR (km²)')) + ylab(expression('Log'[10]*' PLR (%)')) +
  scale_y_continuous(labels = c("0","0.01","1","100")) +
  scale_x_continuous(labels=c("0","1","100","10.000","1.000.000")) +
  theme_bw() +
  theme(text = element_text(size = 30))

dp=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('PLR (km²)'~'Log'[10]*" scale")) + ylab(expression('PLR (%)'~'Log'[10]*" scale")) +
  scale_y_continuous(labels = c("0","0.01","1","100")) +
  scale_x_continuous(labels=c("0","1","100","10.000","1.000.000")) +
  theme_bw() +
  theme(text = element_text(size = 30))




ggsave(paste0(dir_figs,'paper/PLRkm_vs_PLRperc_current.jpg'),dp, width = 12, height = 7)

#########################################################################################################################################################################################
# bar plots which species affected/fragmented or not
#########################################################################################################################################################################################



# see the division of species over affected or not or how.

pie=data.frame(
  scenario=c(rep("Present",3), rep("Future",5)),
  Category=c("No fragmentation","Fragmentation: not threatened by isolation", "Threatened by isolation", 
             "No fragmentation","Fragmentation: not threatened by isolation", "Threatened by isolation: no increase", "Threatened by isolation: increase","Threatened by isolation: new"),
  value=c(nrow(ESH %>% filter(fragmented_cur==F)), nrow(ESH %>% filter(ESHkm_curdams==0&fragmented_cur==T)), nrow(ESH %>% filter(ESHkm_curdams!=0)), #current
          nrow(ESH %>% filter(fragmented_fut==F)), nrow(ESH %>% filter(ESHkm_futdams==0&fragmented_fut==T)),
          nrow(ESH %>% filter(ESHkm_curdams!=0 & diffkm==0)), nrow(ESH %>% filter(ESHkm_curdams!=0 & diffkm>0)), nrow(ESH %>% filter(ESHkm_curdams==0 & diffkm>0))   )
)

pie$scenario <- factor(pie$scenario,levels=c("Present","Future"))
pie$Category <- factor(pie$Category,levels=c("Threatened by isolation","Threatened by isolation: no increase", "Threatened by isolation: increase",
                                             "Threatened by isolation: new","Fragmentation: not threatened by isolation", "No fragmentation" ))


ggplot(pie, aes(x="", y=value, fill=Category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  facet_wrap(~scenario)+
  scale_fill_brewer(palette="Set1")

barp=ggplot(pie[order(pie$Category),], aes(fill=Category, y=value, x=scenario)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+ 
  #scale_fill_discrete(guide=guide_legend(reverse=T))+
  scale_fill_manual(values=colors_bar, guide=guide_legend(reverse=T))+
  theme_bw()+
  labs(y="Number of species", x=element_blank())

ggsave(paste0(dir_figs,'paper/Barplot_species.jpg'),barp,
       dpi = 600)


#get stats
# how many species experience fragmentation
sum(ESH %>% pull(fragmented_cur))/nrow(ESH)*100
sum(ESH %>% pull(fragmented_cur))
sum(ESH  %>% pull(fragmented_fut))/nrow(ESH)*100
sum(ESH  %>% pull(fragmented_fut))

# how many species affected
nrow(ESH %>% filter(ESHkm_curdams!=0))
nrow(ESH %>% filter(ESHkm_futdams!=0))

nrow(ESH %>% filter(ESH_curdams>10))
nrow(ESH %>% filter(ESH_futdams>10))

nrow(ESH %>% filter(ESHkm_curdams>10000))
nrow(ESH %>% filter(ESHkm_futdams>10000))







#########################################################################################################################################################################################
# ESH box/violin plots
#########################################################################################################################################################################################


#get values for df
impESHperc=ESH%>%
  #filter(ESH_curdams>=1) %>% 
  pull(ESH_curdams)
impESHfutperc=ESH%>% 
  #filter(ESH_futdams>=1) %>% 
  pull(ESH_futdams)

impESHkm=ESH%>%
  #filter(ESH_curdams>=1) %>% 
  pull(ESHkm_curdams)
impESHfutkm=ESH%>% 
  #filter(ESH_futdams>=1) %>% 
  pull(ESHkm_futdams)

# prepare data
figesh=data.frame(ESH=c(impESHperc,impESHfutperc, 
                        impESHkm,impESHfutkm),
                  scenario=c(rep("Present",length(impESHperc)),rep("Future",length(impESHfutperc))
                             , rep("Present",length(impESHkm)),rep("Future",length(impESHfutkm))),
                  var=c(rep("PLR (%)",length(impESHperc)+length(impESHfutperc)),rep("PLR (km²)",length(impESHkm)+length(impESHfutkm)))
                  )

figesh$scenario <- factor(figesh$scenario,levels=c("Present","Future"))


#---------------------- PRESENT & FUTURE TOGETHER ---------------------------------

# ESH percentage
  
# with zeros
f1a1= ggplot(figesh %>% filter(var=="PLR (%)"), aes(x=scenario,y=ESH, fill=scenario)) +
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

#without zeros
nrow(figesh %>% filter(var=="PLR (%)") %>%filter(ESH!=0) %>% filter(scenario=="Present")) #2840 of 7495-18 (NA)
f1a2= ggplot(figesh %>% filter(var=="PLR (%)") %>%filter(ESH!=0), aes(x=scenario,y=ESH, fill=scenario)) +
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


# km2 ESH
# including zeros

f1b1= ggplot(figesh %>% filter(var=="PLR (km²)"), aes(x=scenario,y=ESH, fill=scenario)) +
  geom_violin(aes(fill = scenario),lwd = .5,color='transparent')+
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.75)+
  scale_y_continuous(trans="pseudo_log", labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))+ # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y="PLR (km²)",x=element_blank()) +
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


# without zeros
nrow(figesh %>% filter(var=="PLR (km²)") %>%filter(ESH!=0) %>% filter(scenario=="Present")) #2840 of 7495-9 (NA)
nrow(figesh %>% filter(var=="PLR (km²)") %>%filter(ESH!=0) %>% filter(scenario=="Future")) #2942
f1b2=  ggplot(figesh %>% filter(var=="PLR (km²)")%>%filter(ESH!=0), aes(x=scenario,y=ESH, fill=scenario)) +
  geom_violin(aes(fill = scenario),lwd = .5,color='transparent') +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.75)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000)) + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y="PLR (km²)",x=element_blank()) +
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


#figure if zeros included
f1inclzero=ggarrange(f1a1,f1b1, ncol=2,nrow=1)
#figure if zeros excluded
f1exclzero=ggarrange(f1a2,f1b2, ncol=2,nrow=1)

ggsave(paste0(dir_figs,'paper/PLR_PresFut_incl0.jpg'),f1inclzero,
      width = 183,height = 60,dpi = 600,units = 'mm')
ggsave(paste0(dir_figs,'paper/PLR_PresFut_excl0.jpg'),f1exclzero,
       width = 183,height = 60,dpi = 600,units = 'mm')



#stats
mean(na.omit(impESHkm))
sd(na.omit(impESHkm))
mean(na.omit(impESHfutkm))
sd(na.omit(impESHfutkm))
mean(na.omit(impESHperc))
sd(na.omit(impESHperc))
mean(na.omit(impESHfutperc))
sd(na.omit(impESHfutperc))

sum(na.omit(impESHkm)!=0) #2840 species threatened
sum(na.omit(impESHfutkm)!=0) #2942 species threatened
#stats zero excluded
mean(na.omit(impESHkm[impESHkm!=0]))
sd(na.omit(impESHkm[impESHkm!=0]))
mean(na.omit(impESHfutkm[impESHfutkm!=0]))
sd(na.omit(impESHfutkm[impESHfutkm!=0]))
mean(na.omit(impESHperc[impESHperc!=0]))
sd(na.omit(impESHperc[impESHperc!=0]))
mean(na.omit(impESHfutperc[impESHfutperc!=0]))
sd(na.omit(impESHfutperc[impESHfutperc!=0]))

# are the increases large?
test=ESH%>%filter(ESHkm_futdams!=0)
test$diff=test$ESHkm_futdams-test$ESHkm_curdams

summary(test$diff)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     0.0     0.0    64.9     0.0 37648.9 
nrow(test) #2942
test2=test%>%filter(diff !=0)
summary(test2$diff)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.3    18.3    42.0   445.1   161.5 37648.9 
nrow(test2) #429 species experience increase

test=ESH%>%filter(ESH_futdams!=0)
test$diffperc=test$ESH_futdams-test$ESH_curdams

summary(test$diffperc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000  0.00000  0.00000  0.06485  0.00000 15.34325 
nrow(test) #2942
test2=test%>%filter(diffperc !=0)
summary(test2$diffperc)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000040  0.003154  0.033828  0.444712  0.199035 15.343246
nrow(test2)



# figure of future species separately, only including where increase in affected area

f1afut=  ggplot(ESH %>%filter(diffkm!=0), aes(x=factor(0),y=diffperc) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,99)) +
  labs(y=expression(Delta~"PLR (%)"),x=element_blank()) +
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


mean(ESH %>%filter(diffkm!=0) %>% pull(diffkm))

f1bfut=  ggplot(ESH %>%filter(diffkm!=0), aes(x=factor(0),y=diffkm) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000)) + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y=expression(Delta~"PLR (km²)"),x=element_blank()) +
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


f1onlyfutchange=ggarrange(f1afut,f1bfut, ncol=2,nrow=1)
ggsave(paste0(dir_figs,'paper/PLR_onlyfutchange.jpg'),f1onlyfutchange,
       width = 183,height = 60,dpi = 600,units = 'mm')


#incl.0

f1afut=  ggplot(ESH , aes(x=factor(0),y=diffperc) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,99)) +
  labs(y=expression(Delta~"PLR (%)"),x=element_blank()) +
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




f1bfut=  ggplot(ESH, aes(x=factor(0),y=diffkm) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000)) + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y=expression(Delta~"PLR (km²)"),x=element_blank()) +
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


f1onlyfutchange=ggarrange(f1afut,f1bfut, ncol=2,nrow=1)
ggsave(paste0(dir_figs,'paper/PLR_onlyfutchange_incl0.jpg'),f1onlyfutchange,
       width = 183,height = 60,dpi = 600,units = 'mm')


















#---------------------- ONLY PRESENT  ---------------------------------

# ESH percentage

# with zeros
f1a1= ggplot(figesh %>% filter(var=="PLR (%)", scenario=="Present"), aes(x=scenario,y=ESH, fill=scenario)) +
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
    axis.text.x = element_blank(),
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

#without zeros
nrow(figesh %>% filter(var=="PLR (%)") %>%filter(ESH!=0) %>% filter(scenario=="Present")) #2840 of 7495-18 (NA)
f1a2= ggplot(figesh %>% filter(var=="PLR (%)") %>%filter(ESH!=0, scenario=="Present"), aes(x=scenario,y=ESH, fill=scenario)) +
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
    axis.text.x = element_blank(),
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


# km2 ESH
# including zeros

f1b1= ggplot(figesh %>% filter(var=="PLR (km²)", scenario=="Present"), aes(x=scenario,y=ESH, fill=scenario)) +
  geom_violin(aes(fill = scenario),lwd = .5,color='transparent')+
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.75)+
  scale_y_continuous(trans="pseudo_log", labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))+ # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y="PLR (km²)",x=element_blank()) +
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
    axis.text.x = element_blank(),
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


# without zeros
nrow(figesh %>% filter(var=="PLR (km²)") %>%filter(ESH!=0) %>% filter(scenario=="Present")) #2840 of 7495-9 (NA)
nrow(figesh %>% filter(var=="PLR (km²)") %>%filter(ESH!=0) %>% filter(scenario=="Future")) #2942
f1b2=  ggplot(figesh %>% filter(var=="PLR (km²)")%>%filter(ESH!=0, scenario=="Present"), aes(x=scenario,y=ESH, fill=scenario)) +
  geom_violin(aes(fill = scenario),lwd = .5,color='transparent') +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.75)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000)) + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  labs(y="PLR (km²)",x=element_blank()) +
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
    axis.text.x =element_blank(),
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


#figure if zeros included
f1inclzero=ggarrange(f1a1,f1b1, ncol=2,nrow=1)
#figure if zeros excluded
f1exclzero=ggarrange(f1a2,f1b2, ncol=2,nrow=1)

ggsave(paste0(dir_figs,'paper/PLR_present_incl0.jpg'),f1inclzero,
       width = 183,height = 60,dpi = 600,units = 'mm')
ggsave(paste0(dir_figs,'paper/PLR_present_excl0.jpg'),f1exclzero,
       width = 183,height = 60,dpi = 600,units = 'mm')



#########################################################################################################################################################################################
# CI Valerio
#########################################################################################################################################################################################


# CI values Valerio
CI=read.csv("I:/tamara/CC_dams_R/Fishdata/CI_Valerio/pnasCI.csv")
colnames(CI)[1]="binomial" # these binomials should be fishbase names, so couple it to fishbase column in traits

#get fishbase names (my fishbase version)
#library(rfishbase)
#options(FISHBASE_VERSION="21.06")
#fb_names=array()
#your_list=unique(CI$binomial) # this is not an actual R list item, just a vector with each element a species name
#for(i in 1:length(your_list)) {
#  fb_names[i]=as.character(validate_names(your_list[i])[1])
#}
#CI_fbnames=data.frame(binomial=your_list,fb_name=fb_names) # 16 NAs
#write.csv(CI_fbnames,"I:/tamara/CC_dams_R/Fishdata/CI_Valerio/CImatch_fb21names.csv", row.names = F) 

CI_fbnames=read.csv("I:/tamara/CC_dams_R/Fishdata/CI_Valerio/CImatch_fb21names.csv")

#how many matches (the traits$fb_name column gives fishbase referenced names for all IUCN species)
#test=traits$binomial %in% CI$binomial #5869
#test2=traits$binomial %in% CI_fbnames$fb_name #5755
#test3=traits$fb_name %in% CI$binomial #5746
#test4= traits$fb_name %in% CI_fbnames$fb_name #5989

CI_names=left_join(CI,CI_fbnames) %>% select(-binomial) # want to match the CI values by fb_name, so remove the binomial column
# df with binomial IUCN, fishbase name and CI values
CIspec=left_join(traits %>% select(binomial,fb_name),CI_names %>% select(fb_name, CI_current, CI_future)) 

#so now we can compare CI value and our MVA results
ESH_CI=left_join(ESH,CIspec) # they will match on binomial, NA for species we dont have values for
sum(is.na(ESH_CI$CI_current)) #2648 not there
sum(is.na(ESH_CI$CI_future)) #2648

# results vs CI
plot(log10(ESH_CI$ESH_curdams),ESH_CI$CI_current)
plot(log10(ESH_CI$ESHkm_curdams),ESH_CI$CI_current)
summary(lm(formula = CI_current ~ ESH_curdams  , data=ESH_CI))
summary(lm(formula = CI_current ~ ESHkm_curdams  , data=ESH_CI))


summary(lm(formula = CI_current ~ log10(ESH_curdams+1)  , data=ESH_CI)) # does not work log10(0)=-inf, so added 1
summary(lm(formula = CI_current ~ log10(ESHkm_curdams+1)  , data=ESH_CI)) # does not work, so added 1
# removed zero values though this does not make sense as we want to take
#into account the species that have value zero as well to check whether CI says their connectivity is damaged.
summary(lm(formula = CI_current ~ log10(ESH_curdams)  , data=ESH_CI %>%filter(ESH_curdams!=0))) 
summary(lm(formula = CI_current ~ log10(ESHkm_curdams)  , data=ESH_CI %>%filter(ESH_curdams!=0))) # 


library(lubridate)
library(ggpmisc)

#compare CI and my results (including zero values)
# no log transformation as this cannot handle values of zero
CIcomp_incl0log=ggplot(ESH_CI, aes(x= log10(ESH_curdams+1), y=CI_current))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, label.y = (0)) +
  theme_bw()

#excluding zeros and log transforming (like simple plot function)
CIcomp_excl0log= ggplot(ESH_CI %>% filter(ESH_curdams!=0), aes(x= log10(ESH_curdams), y=CI_current))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, label.y = (0)) +
  theme_bw()

#ggsave(paste0(dir_figs,'paper/CIcomp_perc_incl0.jpg'),CIcomp_incl0log)
#ggsave(paste0(dir_figs,'paper/CIcomp_perc_excl0.jpg'),CIcomp_excl0log)

CIcompkm_incl0log=ggplot(ESH_CI, aes(x= log10(ESHkm_curdams+1), y=CI_current))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, label.y = (0)) +
  theme_bw()

#excluding zeros and log transforming (like simple plot function)
CIcompkm_excl0log= ggplot(ESH_CI %>% filter(ESHkm_curdams!=0), aes(x= log10(ESH_curdams), y=CI_current))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, label.y = (0)) +
  theme_bw()


CIcompinclzero=ggarrange(CIcomp_incl0log, CIcompkm_incl0log, ncol=2,nrow=1)
ggsave(paste0(dir_figs,'paper/CIcomp_incl0.jpg'),CIcompinclzero)

CIcompexclzero=ggarrange(CIcomp_excl0log, CIcompkm_excl0log, ncol=2,nrow=1)
ggsave(paste0(dir_figs,'paper/CIcomp_excl0.jpg'),CIcompexclzero)






#########################################################################################################################################################################################
# Species characteristics
#########################################################################################################################################################################################

############################### BOXPLOTS ##########################################

##########
## prep ##
##########

# add IUCN info on dams threat
compIUCN <- left_join(ESH,threattab) #2774 no data
table(compIUCN$Ldams,useNA = 'always') #T=300
table(compIUCN$Sdams,useNA = 'always') #T=230
table(compIUCN$Udams,useNA = 'always') #T=914

table(compIUCN$Ldams_fut,useNA = 'always') #T=117
table(compIUCN$Sdams_fut,useNA = 'always') #T=14
table(compIUCN$Udams_fut,useNA = 'always') #T=188

table(compIUCN%>%filter(Ldams==T)%>%pull(Udams),useNA = 'always') # only 20 species that have dams as large threat also have dams of unknown size as threat

# per category of dams make a figure. (rows) per column a variable (severity or scope or score) (include NA as "no threat/none" category?), put boxplots in with classes (future seperately?)
include_NA=TRUE # if false need to adapt some code adding factors traits

traits2=traits

# if we want to exclude NAs:
if(include_NA==FALSE){
  # remove the species we have no threat info for, although this is not handy for combining with trait table
  #compIUCN <- compIUCN %>% filter(!is.na(Ldams)) #4721
    
    # order levels
    
    compIUCN$LD_scope <- factor(compIUCN$LD_scope,levels=c("Minority (<50%)","Majority (50-90%)","Whole (>90%)", "Unknown"))
    compIUCN$LD_scope_fut <- factor(compIUCN$LD_scope_fut,levels=c("Minority (<50%)","Majority (50-90%)","Whole (>90%)", "Unknown"))
    compIUCN$LD_sev <- factor(compIUCN$LD_sev,levels=c("No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations", "Unknown" ))
    compIUCN$LD_sev_fut <- factor(compIUCN$LD_sev_fut,levels=c("No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations", "Unknown" ))
    compIUCN$LD_score <- factor(compIUCN$LD_score,levels=c("Low Impact: 3","Low Impact: 4","Low Impact: 5","Medium Impact: 6", "Medium Impact: 7", "High Impact: 8","High Impact: 9", "Unknown"))
    compIUCN$LD_score_fut <- factor(compIUCN$LD_score_fut,levels=c("No/Negligible Impact: 1","Low Impact: 3","Low Impact: 4", "Low Impact: 5","Medium Impact: 6","Unknown" ))
  
  # fix flaw
  traits2$importance[which(traits2$importance=="")] <- NA
  traits2$importance[which(traits2$importance==" ")] <- NA
}

# if we want to include NAs as a group:
if(include_NA==TRUE){
  # Replace NAs; fill in Unknown when dams is TRUE, if Dams is FALSE fill in no threat
  
  # large dams
  compIUCN[which(compIUCN$Ldams==T),'LD_sev'] <- compIUCN[which(compIUCN$Ldams==T),'LD_sev'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Ldams==T),'LD_scope'] <- compIUCN[which(compIUCN$Ldams==T),'LD_scope'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Ldams==T),'LD_score'] <- compIUCN[which(compIUCN$Ldams==T),'LD_score'] %>% replace_na("Unknown")
  
  compIUCN[which(compIUCN$Ldams==F),'LD_sev'] <- compIUCN[which(compIUCN$Ldams==F),'LD_sev'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Ldams==F),'LD_scope'] <- compIUCN[which(compIUCN$Ldams==F),'LD_scope'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Ldams==F),'LD_score'] <- compIUCN[which(compIUCN$Ldams==F),'LD_score'] %>% replace_na("No threat")
  
  
  # small dams
  compIUCN[which(compIUCN$Sdams==T),'SD_sev'] <- compIUCN[which(compIUCN$Sdams==T),'SD_sev'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Sdams==T),'SD_scope'] <- compIUCN[which(compIUCN$Sdams==T),'SD_scope'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Ldams==T),'SD_score'] <- compIUCN[which(compIUCN$Ldams==T),'SD_score'] %>% replace_na("Unknown")
  
  compIUCN[which(compIUCN$Sdams==F),'SD_sev'] <- compIUCN[which(compIUCN$Sdams==F),'SD_sev'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Sdams==F),'SD_scope'] <- compIUCN[which(compIUCN$Sdams==F),'SD_scope'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Sdams==F),'SD_score'] <- compIUCN[which(compIUCN$Sdams==F),'SD_score'] %>% replace_na("No threat")
  
  # unknown dams
  compIUCN[which(compIUCN$Udams==T),'UD_sev'] <- compIUCN[which(compIUCN$Udams==T),'UD_sev'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Udams==T),'UD_scope'] <- compIUCN[which(compIUCN$Udams==T),'UD_scope'] %>% replace_na("Unknown")
  compIUCN[which(compIUCN$Udams==T),'UD_score'] <- compIUCN[which(compIUCN$Udams==T),'UD_score'] %>% replace_na("Unknown")
  
  compIUCN[which(compIUCN$Udams==F),'UD_sev'] <- compIUCN[which(compIUCN$Udams==F),'UD_sev'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Udams==F),'UD_scope'] <- compIUCN[which(compIUCN$Udams==F),'UD_scope'] %>% replace_na("No threat")
  compIUCN[which(compIUCN$Udams==F),'UD_score'] <- compIUCN[which(compIUCN$Udams==F),'UD_score'] %>% replace_na("No threat")
  
  #4,717 species for which dams (regardless of size) are stated as a threat.
  # for 2769 species no data on threats, so make it a category no data?
  compIUCN[is.na(compIUCN)] <- "No data"
  
  #compIUCN$LD_scope <- factor(compIUCN$LD_scope,levels=c("Minority (<50%)","Majority (50-90%)","Whole (>90%)", "Unknown", "No threat", "No data"))
  #compIUCN$LD_scope_fut <- factor(compIUCN$LD_scope_fut,levels=c("Minority (<50%)","Majority (50-90%)","Whole (>90%)",  "Unknown","No threat"))
  #compIUCN$LD_sev <- factor(compIUCN$LD_sev,levels=c("No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations",  "Unknown","No threat", "No data" ))
  #compIUCN$LD_sev_fut <- factor(compIUCN$LD_sev_fut,levels=c("No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations", "Unknown", "No threat" ,"No data" ))
  #compIUCN$LD_score <- factor(compIUCN$LD_score,levels=c("Low Impact: 3","Low Impact: 4","Low Impact: 5","Medium Impact: 6", "Medium Impact: 7", "High Impact: 8","High Impact: 9","Unknown","No threat", "No data" ))
  #compIUCN$LD_score_fut <- factor(compIUCN$LD_score_fut,levels=c("No/Negligible Impact: 1","Low Impact: 3","Low Impact: 4", "Low Impact: 5","Medium Impact: 6", "Unknown","No threat", "No data" ))
  
  #nodata first for convenience plots
  compIUCN$LD_scope <- factor(compIUCN$LD_scope,levels=c("No data", "No threat","Unknown","Minority (<50%)","Majority (50-90%)","Whole (>90%)"  ))
  compIUCN$LD_scope_fut <- factor(compIUCN$LD_scope_fut,levels=c("No data", "No threat","Unknown","Minority (<50%)","Majority (50-90%)","Whole (>90%)"))
  compIUCN$LD_sev <- factor(compIUCN$LD_sev,levels=c("No data", "No threat","Unknown","No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations"))
  compIUCN$LD_sev_fut <- factor(compIUCN$LD_sev_fut,levels=c("No data", "No threat","Unknown","No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations" ))
  compIUCN$LD_score <- factor(compIUCN$LD_score,levels=c("No data", "No threat","Unknown","Low Impact: 3","Low Impact: 4","Low Impact: 5","Medium Impact: 6", "Medium Impact: 7", "High Impact: 8","High Impact: 9" ))
  compIUCN$LD_score_fut <- factor(compIUCN$LD_score_fut,levels=c("No data", "No threat","Unknown","No/Negligible Impact: 1","Low Impact: 3","Low Impact: 4", "Low Impact: 5","Medium Impact: 6"))
  
  
  
  # fix flaw
  traits2$importance[which(traits2$importance=="")] <- NA
  traits2$importance[which(traits2$importance==" ")] <- NA
  traits2$importance=as.character(traits2$importance)
  traits2[is.na(traits2)] <- "No data"
  traits2$code[which(traits2$code=="LRlc")] <- "LC"
  # set levels
  traits2$code <- factor(traits2$code, levels = c("DD","LC","NT","VU","EN","CR","EW","EX") )
  traits2$importance <- factor(traits2$importance, levels = c("No data","of no interest","of potential intrest","subsistence fisheries","minor commercial","highly commercial") )
  traits2$foodtrophcat <- factor(traits2$foodtrophcat, levels = c("No data","Herbi.","Carni.","Omni.") )
  traits2$climate_zone <- factor(traits2$climate_zone, levels = c("No data","1","2","3", "4", "5") ) 
  levels(traits2$climate_zone) <- c("No data", "A", "B", "C", "D", "E")
  
}
#prepare data for ggplot
#create a dataframe with scenarios (fut/cur), ESH_type (ESHperc/ESHkm) (so basically 4x7496 rows)
# this can then be used to melt a dataframe filtered on a selected ESH_type, and filtered on a specific column (variable of interest) (e.g. severity) 
# as input and scenario as id, so we get all values per IUCN category (groups within variable of interest, wil be x axis, named variable)
# we can rbind indiated what kind of variable of interest it is (e.g. scope, severity), so this can be facetted. (to get 1 figure for each variable_type)


tab_char <- left_join(compIUCN,traits2 %>%select(binomial,importance,foodtrophcat,climate_zone,code))



###########
## prep2 ##
###########


#only looking at ongoing threats and current dams results

# combine info large dams and unknown dams to have more data
table(compIUCN$Ldams)
table(compIUCN$Udams)
# if Large dams = F and dams of unknown size = T, use these scope, severity and score values.

compIUCN$LorUdams <- (compIUCN$Ldams==T | compIUCN$Udams==T)
compIUCN$LorUdams[which(compIUCN$Ldams=="No data")] <- "No data" 
compIUCN$LorUdams[which(compIUCN$LorUdams==F)] <- "Dams not indicated as threat" 
compIUCN$LorUdams[which(compIUCN$LorUdams==T)] <- "Dams indicated as threat" 
compIUCN$LorUdams <- factor(compIUCN$LorUdams, levels=c("No data","Dams not indicated as threat", "Dams indicated as threat"  ))
table(compIUCN$LorUdams) #1194 species threatened by either large dams or dams of unknown size

compIUCN$LU_scope <- compIUCN$LD_scope
compIUCN$LU_scope[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_scope[which(compIUCN$Ldams==F & compIUCN$Udams==T)]

compIUCN$LU_sev <- compIUCN$LD_sev
compIUCN$LU_sev[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_sev[which(compIUCN$Ldams==F & compIUCN$Udams==T)]

compIUCN$LU_score <- compIUCN$LD_score
compIUCN$LU_score[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_score[which(compIUCN$Ldams==F & compIUCN$Udams==T)]


# make a column with timing of the threat
table(compIUCN$Ldams==F & compIUCN$Udams==F & (compIUCN$Ldams_fut==T | compIUCN$Udams==T))
table(compIUCN$Ldams==T &compIUCN$Udams_fut==T) #overlap
table(compIUCN$Udams==T &compIUCN$Ldams_fut==T) # overlap


tab_char <- left_join(compIUCN,traits2 %>%select(binomial,importance,foodtrophcat,climate_zone,code))




###########
## prep3 ##
###########

# make a long table to facat things (nope does not work) and get boxplots per group
# rename variables of interest to logical names
tab_char <- tab_char %>% rename("IUCN threat scope"="LU_scope", "IUCN threat severity"="LU_sev", "IUCN threat score"="LU_score",
                                "IUCN threat status"="code", "Commercial importance" = "importance",
                                "Trophic level"="foodtrophcat", "Climate zone"= "climate_zone")

vars=c("IUCN threat scope","IUCN threat severity","IUCN threat score","IUCN threat status","Commercial importance","Trophic level","Climate zone")
n=length(vars)




datalist = vector("list", length = n)
for (i in 1:n) {
  # ... make some data
  d <- tab_char %>% select(ESHkm_curdams,vars[i]) %>% reshape2::melt(.,id=vars[i])
  #if(include_NA==FALSE){d=d[complete.cases(d),]}
  colnames(d)[1]= "group"
  colnames(d)[2]= "ESH_type"
  d$var=vars[i]
  datalist[[i]] <- d # add it to your list
}
big_datakm = do.call(rbind, datalist)

datalist = vector("list", length = n)
for (i in 1:n) {
  # ... make some data
  d <- tab_char %>% select(ESH_curdams,vars[i]) %>% reshape2::melt(.,id=vars[i])
  #if(include_NA==FALSE){d=d[complete.cases(d),]}
  colnames(d)[1]= "group"
  colnames(d)[2]= "ESH_type"
  d$var=vars[i]
  datalist[[i]] <- d # add it to your list
}
big_dataperc = do.call(rbind, datalist)

#combine
big_data=rbind(big_dataperc,big_datakm)
#cannot use facet grid because manual log scales perc or km results




############
## ggplot ##
############


#nr observations per boxplot
give.n <- function(x){
  return(c(y = -0.25, label = length(x))) 
  # experiment with the number to find the perfect position
}

#wrap the x axis labels
str_wrap_factor <- function(x, ...) {
  levels(x) <- stringr::str_wrap(levels(x), ...)
  x
}

#make a list of plot because facetting does not work as there are different x and y axises
ESHtypes=c("ESH_curdams","ESHkm_curdams")
lops=expand.grid(ESHtypes,vars)
colnames(lops)=c("ESHtypes","vars")



###################### including non affected species (ESH=0) ###############################

iucnfigperc=ggplot(tab_char, aes(x=LorUdams,y=ESH_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(-2,99)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")coord_cartesian(ylim = c(-1,700000)) } +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), ESH_curdams) +
  labs(y="PLR (%)",x="IUCN threat data") +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")labs(y="ESH (km²)",x=lops$vars[i]) } +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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


#first 2 figures showing no data, no threat, dams as threat
iucnfigkm=ggplot(tab_char, aes(x=LorUdams,y=ESHkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))  + # special trans to keep zeros
  theme_bw() +
  #coord_cartesian(ylim = c(-2,99)) +
  coord_cartesian(ylim = c(-5,700000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), ESHkm_curdams) +
  #labs(y="ESH (%)",x="IUCN threat data") +
  labs(y="PLR (km²)",x="IUCN threat data")  +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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


# per variable without no data or no threat species

plotList <- lapply(1:nrow(lops), function(i){
  
  ggplot(big_data %>%filter(var==lops$vars[i]) %>% filter(ESH_type==lops$ESHtypes[i]) %>% filter(group %!in% c("No data","No threat"))%>%droplevels(.), aes(x=group,y=value) ) +
    geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
    geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
    #if(ESHtypes[j]=="ESH_curdams"){}
    {if(lops$ESHtypes[i]=="ESH_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) }+
    {if(lops$ESHtypes[i]=="ESHkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
    theme_bw() +
    {if(lops$ESHtypes[i]=="ESH_curdams")coord_cartesian(ylim = c(-2,99))} +
    {if(lops$ESHtypes[i]=="ESHkm_curdams")coord_cartesian(ylim = c(-5,700000)) } +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                 position = position_dodge(width = 0.75)) +
    aes(str_wrap_factor(group, 15), value) +
    {if(lops$ESHtypes[i]=="ESH_curdams")labs(y="ESH (%)",x=lops$vars[i])} +
    {if(lops$ESHtypes[i]=="ESHkm_curdams")labs(y="ESH (km²)",x=lops$vars[i]) } +
    #facet_grid(var~., scales="free_x") +
    #scale_fill_manual(values=colors) +
    #scale_x_discrete(guide = guide_axis(n.dodge=3))+
    stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
    #coord_flip()+
    
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
      axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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
  
  
  
})

# combine the plots to one list
plots_extra=list(iucnfigperc,iucnfigkm)
plotList2=append(plots_extra,plotList)
# arrange plots in 1
allplots <- ggarrange(plotlist=plotList2,
                      #labels = c("A", "B", "C", "D"),
                      ncol = length(ESHtypes), nrow = length(vars)+1)
allplots
# save
ggsave(paste0(dir_figs,'paper/BP_AllSpecChar_incl0_sepNDNT.jpg'),allplots) #cannot be seen....

#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=plotList2[c(1:8)],
                      #labels = c("A", "B", "C", "D"),
                      ncol = length(ESHtypes), nrow = 4)
threatplots
ggsave(paste0(dir_figs,'paper/BP_SpecDamsThreat_incl0_sepNDNT.jpg'),threatplots) #better visible except score labels


#plots IUCN dams threat only
traitplots <- ggarrange(plotlist=plotList2[c(9:length(plotList2))],
                         #labels = c("A", "B", "C", "D"),
                         ncol = length(ESHtypes), nrow = 4)
traitplots
ggsave(paste0(dir_figs,'paper/BP_SpecTrait_incl0_exclNA.jpg'),traitplots) #ok




# SECOND TIME
##################### including non affected species (ESH=0) ###############################

iucnfigperc=ggplot(tab_char, aes(x=LorUdams,y=ESH_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(-2,99)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")coord_cartesian(ylim = c(-1,700000)) } +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size=6) +
  aes(str_wrap_factor(LorUdams, 15), ESH_curdams) +
  labs(y="PLR (%)",x=element_blank()) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")labs(y="ESH (km²)",x=lops$vars[i]) } +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    text = element_text(size=20),
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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
    axis.title.y = element_text(size = 20),
    strip.text = element_text(angle = 0, size = 16)
  )


#first 2 figures showing no data, no threat, dams as threat
iucnfigkm=ggplot(tab_char, aes(x=LorUdams,y=ESHkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))  + # special trans to keep zeros
  theme_bw() +
  #coord_cartesian(ylim = c(-2,99)) +
  coord_cartesian(ylim = c(-5,700000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size=6) +
  aes(str_wrap_factor(LorUdams, 15), ESHkm_curdams) +
  #labs(y="ESH (%)",x="IUCN threat data") +
  labs(y="PLR (km²)",x="IUCN threat data")  +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    text = element_text(size=20),
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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
    axis.title.y = element_text(size = 20),
    strip.text = element_text(angle = 0, size = 16)
  )


#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=list(iucnfigperc,iucnfigkm),
                         #labels = c("A", "B", "C", "D"),
                         ncol = 1, nrow = 2)
threatplots
ggsave(paste0(dir_figs,'paper/2_SpecDamsThreat_incl0_sepND.jpg'),threatplots) #better visible except score labels




###################### excluding non affected species ###############################

# do the same but exclude zeros (not impacted species, focus on the ~2500 affected)
# then only do this for the characteristics....

iucnfigperc=ggplot(tab_char %>%filter(ESHkm_curdams!=0), aes(x=LorUdams,y=ESH_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(-2,99)) +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")coord_cartesian(ylim = c(-1,700000)) } +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), ESH_curdams) +
  labs(y="ESH (%)",x="IUCN threat data") +
  #{if(lops$ESHtypes[i]=="ESHkm_curdams")labs(y="ESH (km²)",x=lops$vars[i]) } +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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


#first 2 figures showing no data, no threat, dams as threat
iucnfigkm=ggplot(tab_char %>%filter(ESHkm_curdams!=0), aes(x=LorUdams,y=ESHkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))  + # special trans to keep zeros
  theme_bw() +
  #coord_cartesian(ylim = c(-2,99)) +
  coord_cartesian(ylim = c(-5,700000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), ESHkm_curdams) +
  #labs(y="ESH (%)",x="IUCN threat data") +
  labs(y="ESH (km²)",x="IUCN threat data")  +
  #facet_grid(var~., scales="free_x") +
  #scale_fill_manual(values=colors) +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
  #coord_flip()+
  
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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


# per variable without no data or no threat species

plotList <- lapply(1:nrow(lops), function(i){
  
  ggplot(big_data %>%filter(var==lops$vars[i]) %>% filter(ESH_type==lops$ESHtypes[i]) %>% 
           filter(group %!in% c("No data","No threat")) %>% 
           filter(value!=0) %>% droplevels(.), aes(x=group,y=value) ) +
    geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
    geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
    #if(ESHtypes[j]=="ESH_curdams"){}
    {if(lops$ESHtypes[i]=="ESH_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) }+
    {if(lops$ESHtypes[i]=="ESHkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
    theme_bw() +
    {if(lops$ESHtypes[i]=="ESH_curdams")coord_cartesian(ylim = c(-2,99))} +
    {if(lops$ESHtypes[i]=="ESHkm_curdams")coord_cartesian(ylim = c(-5,700000)) } +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                 position = position_dodge(width = 0.75)) +
    aes(str_wrap_factor(group, 15), value) +
    {if(lops$ESHtypes[i]=="ESH_curdams")labs(y="ESH (%)",x=lops$vars[i])} +
    {if(lops$ESHtypes[i]=="ESHkm_curdams")labs(y="ESH (km²)",x=lops$vars[i]) } +
    #facet_grid(var~., scales="free_x") +
    #scale_fill_manual(values=colors) +
    #scale_x_discrete(guide = guide_axis(n.dodge=3))+
    stat_summary_two(fun.y=mean, geom="point", shape=18, size=2, color="red", fill="red") +
    #coord_flip()+
    
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
      axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
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
  
  
  
})

# combine the plots to one list
plots_extra=list(iucnfigperc,iucnfigkm)
plotList2=append(plots_extra,plotList)
# arrange plots in 1
allplots <- ggarrange(plotlist=plotList2,
                      #labels = c("A", "B", "C", "D"),
                      ncol = length(ESHtypes), nrow = length(vars)+1)
allplots
# save
ggsave(paste0(dir_figs,'paper/BP_AllSpecChar_excl0_sepNDNT.jpg'),allplots) #cannot be seen....

#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=plotList2[c(1:8)],
                         #labels = c("A", "B", "C", "D"),
                         ncol = length(ESHtypes), nrow = 4)
threatplots
ggsave(paste0(dir_figs,'paper/BP_SpecDamsThreat_excl0_sepNDNT.jpg'),threatplots) #better visible except score labels


#plots IUCN dams threat only
traitplots <- ggarrange(plotlist=plotList2[c(9:length(plotList2))],
                        #labels = c("A", "B", "C", "D"),
                        ncol = length(ESHtypes), nrow = 4)
traitplots
ggsave(paste0(dir_figs,'paper/BP_SpecTrait_excl0_exclNA.jpg'),traitplots) #ok












############################### BARPLOTS ##########################################

# barplot


bars=data.frame(
  myres=c(rep(c("No fragmentation","Fragmentation: not affected", "Affected"),each=3)),
  IUCNgroup=c(rep(c("No data","Dams not indicated as threat", "Dams indicated as threat"  ))),
  value=c(nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="Dams indicated as threat") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams indicated as threat") %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams indicated as threat") %>% filter(ESHkm_curdams!=0)))
)

bars$myres <- factor(bars$myres,levels=c("No fragmentation","Fragmentation: not affected", "Affected"))
bars$IUCNgroup <- factor(bars$IUCNgroup,levels=(c("Dams indicated as threat","Dams not indicated as threat",  "No data")))


ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+
  scale_fill_manual(values=c("red","blue", "grey"))+
  theme_bw()+
  aes(str_wrap_factor(myres, 15), value)+
  guides(fill=guide_legend(title="IUCN threat data")) +
  labs(y="Number of species", x=element_blank())


# large dams and dams of unknown size separated
bars=data.frame(
  myres=c(rep(c("No fragmentation","Fragmentation: not affected", "Affected"),each=5)),
  IUCNgroup=c(rep(c("No data","Dams not indicated as threat", "Threat: Large dams", "Threat: Dams of unknown size ", "Threat: Large dams and dams of unknown size"))),
  value=c(nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(ESHkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(ESHkm_curdams!=0)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(ESHkm_curdams!=0))
          )
  
)


bars$myres <- factor(bars$myres,levels=c("No fragmentation","Fragmentation: not affected", "Affected"))
bars$IUCNgroup <- factor(bars$IUCNgroup,levels=(c("Threat: Large dams", "Threat: Dams of unknown size ", "Threat: Large dams and dams of unknown size","Dams not indicated as threat",  "No data")))


ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+
  scale_fill_manual(values=c("red","yellow","orange","blue", "grey"))+
  theme_bw()+
  aes(str_wrap_factor(myres, 15), value)+
  labs(y="Number of species", x=element_blank())+
  guides(fill=guide_legend(title="IUCN threat data"))
  


###################################################################################
# PER SPATIAL UNIT
###################################################################################
# See script DI_maps

# create violin plots per basin size category

tab_sf <- read_sf(paste0(dir_out_tabs,"mbres_fwonly.gpkg") )
tab_sf$Aspec_diff=tab_sf$Aspec_fut - tab_sf$Aspec_cur
tab_sf$Arange_diff=tab_sf$Arange_fut - tab_sf$Arange_cur
tab_sf$PAFspec_diff=tab_sf$PAFspec_fut - tab_sf$PAFspec_cur
tab_sf$PAFrange_diff=tab_sf$PAFrange_fut - tab_sf$PAFrange_cur
c <- tab_sf %>% as.data.frame() %>% dplyr::select(-geom)

c$size_category <- NA
c$size_category[which(c$mb_area<100)] <- "<2"
c$size_category[which(c$mb_area>=100)] <- "2-3"
c$size_category[which(c$mb_area>=1000)] <- "3-4"
c$size_category[which(c$mb_area>=10000)] <- "4-5"
c$size_category[which(c$mb_area>=100000)] <- ">=5"

c$size_category <- factor(c$size_category, levels = c("<2","2-3", "3-4", "4-5", ">=5" ))


#species fraction PAFspec
ggplot(c %>% filter(frag_cur==T), aes(x=size_category,y=PAFspec_cur) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.10,0.25,0.50,1), breaks=c(0,0.01,0.10,0.25,0.50,1)) + # special trans to keep zeros
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,700000)) +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #             position = position_dodge(width = 0.75)) +
  labs(y="PAF of species",x="Hydrological basin size (log10-km²)") +
  #scale_fill_manual(values=colors) +
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

#future increase
ggplot(c %>% filter(frag_fut==T), aes(x=size_category,y=PAFspec_diff) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.10,0.25,0.50,1), breaks=c(0,0.01,0.10,0.25,0.50,1)) + # special trans to keep zeros
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,700000)) +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #             position = position_dodge(width = 0.75)) +
  labs(y="Increase in PAF of species",x="Hydrological basin size (log10-km²)") +
  #scale_fill_manual(values=colors) +
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


#range area PAFrange
ggplot(c %>% filter(frag_cur==T), aes(x=size_category,y=PAFrange_cur*100) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.10,0.25,0.50,1), breaks=c(0,0.01,0.10,0.25,0.50,1)) + # special trans to keep zeros
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,700000)) +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #             position = position_dodge(width = 0.75)) +
  labs(y="PAF of range",x="Hydrological basin size (log10-km²)") +
  #scale_fill_manual(values=colors) +
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

#future increase
ggplot(c %>% filter(frag_fut==T), aes(x=size_category,y=PAFrange_diff*100) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[2]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,0.01,0.10,0.25,0.50,1), breaks=c(0,0.01,0.10,0.25,0.50,1)) + # special trans to keep zeros
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,700000)) +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #             position = position_dodge(width = 0.75)) +
  labs(y="Increase in PAF of range",x="Hydrological basin size (log10-km²)") +
  #scale_fill_manual(values=colors) +
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


























############################### PAF vs Fragment size ##########################################



# simple figures with this
SR_cur=read_sf(paste0(dir_out_tabs,"PAFinterbasins_cur_fwonly.gpkg"))
SR_fut=read_sf(paste0(dir_out_tabs,"PAFinterbasins_fut_fwonly.gpkg"))

# PAF vs. fragment size
plot(log10(SR_cur$ib_area),SR_cur$PAFcur, col=SR_cur$occ)
plot(log10(SR_fut$ib_area),SR_fut$PAFfut)


#density plot

nribs <- SR_cur %>% as.data.frame()%>% group_by(MAIN_BAS) %>% summarise(nrfrags=n())
morethanonefragment=nribs%>%filter(nrfrags>1)%>% pull(MAIN_BAS)
length(morethanonefragment)
SR_cur2 <- SR_cur %>% filter(MAIN_BAS %in% morethanonefragment)

dp=ggplot(SR_cur2, aes(x=log10(ib_area), y=PAFcur) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('Fragment size (km²)'~'Log'[10]*" scale")) + ylab(expression('PAF'["species"])) +
  scale_x_continuous(labels=c("1","100","10.000","1.000.000", "No")) +
  theme_bw() +
  theme(text = element_text(size = 30))



ggsave(paste0(dir_figs,'paper/PAF_vs_FragmentSize.jpg'),dp, width=12, height=7)










###################################################################################
# GARBAGE
###################################################################################



# first tried something only using large dams IUCN threat data
# ok so get a grouped variable
#lets first try it with one for only large dams.

d <- tab_char %>% select(ESHkm_curdams,LD_scope) %>% reshape2::melt(.,id="LD_scope")

#if(include_NA==FALSE){d=d[complete.cases(d),]}

give.n <- function(x){
  return(c(y = -0.1, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

ggplot(d, aes(x=LD_scope,y=value) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000)) + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,700000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  labs(y="ESH (km²)",x=element_blank()) +
  #scale_fill_manual(values=colors) +
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

