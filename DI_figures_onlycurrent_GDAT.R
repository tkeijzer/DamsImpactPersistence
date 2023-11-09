#DI figures (paper)
# make figures for DI paper
# _new points to taking only current stuff

# Tamara Keijzer
# april 2023

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
colors= 'Grey70' # just grey for the violin plot


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
dir_(paste0(dir_figs,"paper/extra/"))
dir_(paste0(dir_figs,"paper/maps/"))
dir_(paste0(dir_figs,"paper/maps/extra/"))

#dir_tabs <- dir_('Dams_impact/tabs/')

#processed data
dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
dir_proc_out <- dir_(paste0('Dams_impact/proc/',mva_type,'_',dams_used,'/')) # specific for relation
###

#directories
dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))
dir_fishsuit="I:/tamara/FishSuit/"
dir_threattab <- "I:/tamara/CC_dams_R/Fishdata/IUCN_DamsThreat.csv"


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
out <- paste0(dir_out_tabs,'ESHfragm_tabDI_fwonly.csv')
ESH <- read.csv(out)

species_out= left_join(data.frame(ESH %>% filter(area_adjusted_km2==0) %>% select(binomial,area_total_km2)),traits)
write.csv(species_out,paste0(dir_figs,"species_fully_out.csv"), row.names = F)

#remove species (n=9) that already are fully endangered without dams
ESH <- ESH %>% filter(area_adjusted_km2!=0)

# get a nice table for SI material
PLR_table=data.frame(binomial=ESH %>% select(binomial), range_total_km= ESH %>% pull(area_adjusted_km2) ,PLR_km= ESH %>% pull(ESHkm_curdams), PLR_perc = ESH %>% pull(ESH_curdams))
write.csv(PLR_table,paste0(dir_figs,"SI_Results_species_specific.csv"), row.names = F)

dp=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression(' PLR (km²)')) + ylab(expression(' PLR (%)')) +
  scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2),labels = c("0.0001","0.001","0.01","0.1","1","10","100"), limits=c(-4.5,2)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5),labels=c("1","10","100","1,000","10,000","100,000"), limits=c(0,5)) +
  theme_bw() +
  theme(text = element_text(size = 30))



ggsave(paste0(dir_figs,'paper/PLRkm_vs_PLRperc_current.jpg'),dp, width = 12, height = 7)


#get stats
# how many species experience fragmentation
sum(ESH %>% pull(fragmented_cur))/nrow(ESH)*100
sum(ESH %>% pull(fragmented_cur))


# how many species affected
nrow(ESH %>% filter(ESHkm_curdams!=0))
nrow(ESH %>% filter(ESH_curdams>10))
nrow(ESH %>% filter(ESH_curdams>5))
nrow(ESH %>% filter(ESHkm_curdams>10000))









#get values for df
impESHperc=ESH%>%
  #filter(ESH_curdams>=1) %>% 
  pull(ESH_curdams)

impESHkm=ESH%>%
  #filter(ESH_curdams>=1) %>% 
  pull(ESHkm_curdams)


# prepare data
figesh=data.frame(ESH=c(impESHperc, 
                        impESHkm),
                  scenario=c(rep("Present",length(impESHperc)), rep("Present",length(impESHkm))),
                  var=c(rep("PLR (%)",length(impESHperc)),rep("PLR (km²)",length(impESHkm)))
)


#stats
mean(na.omit(impESHkm))
sd(na.omit(impESHkm))
mean(na.omit(impESHperc))
sd(na.omit(impESHperc))

sum(na.omit(impESHkm)!=0) #2840 species threatened
#stats zero excluded
mean(na.omit(impESHkm[impESHkm!=0]))
sd(na.omit(impESHkm[impESHkm!=0]))
mean(na.omit(impESHperc[impESHperc!=0]))
sd(na.omit(impESHperc[impESHperc!=0]))



# ESH percentage

# with zeros
f1a1= ggplot(figesh %>% filter(var=="PLR (%)"), aes(y=ESH, x=scenario)) + #TODO
  geom_violin(aes(fill = scenario),lwd = .5,color='transparent')+
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white", outlier.alpha = 0.5, alpha=0.9)+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(0,99)) +
  labs(y="PLR (%)",x=element_blank()) +
  scale_x_discrete(labels = c("")) + # get rid of the label below violin plot (we only have 1 now)
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

#ggsave(paste0(dir_figs,'paper/PLR_PresFut_incl0.jpg'),f1a1,
#       width = 90,height = 60,dpi = 1000,units = 'mm')


dp=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression(' PLR (km²)')) + ylab(expression(' PLR (%)')) +
  scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2),labels = c("0.0001","0.001","0.01","0.1","1","10","100"), limits=c(-4.5,2)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5),labels=c("1","10","100","1,000","10,000","100,000"), limits=c(0,5)) +
  theme_bw() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(color='black',vjust = 0),
        axis.text.y = element_text(color='black',hjust = 0.5),
        axis.line.y = element_line(color='black'),
        legend.key.size = unit(5, 'mm'), #change legend key size
        legend.key.height = unit(4, 'mm'), #change legend key height
        legend.key.width = unit(3, 'mm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)

# combine these things

figure2=ggarrange(f1a1,dp, 
          labels = c("A ", "B"),
          font.label = list(size = 12),
          ncol = 2, nrow = 1,
          widths = c(1, 1.5))
ggsave(paste0(dir_figs,'paper/Fig2.jpg'),figure2,
       width = 150,height = 50,dpi = 1000,units = 'mm')

#extend x axis
dp2=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression(' PLR (km²)')) + ylab(expression(' PLR (%)')) +
  scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2),labels = c("0.0001","0.001","0.01","0.1","1","10","100"), limits=c(-4.5,2)) +
  scale_x_continuous(breaks=c(0,2,4,6),labels=c("1","100","10,000","1,000,000"), limits=c(0,7)) +
  theme_bw() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(color='black',vjust = 0),
        axis.text.y = element_text(color='black',hjust = 0.5),
        axis.line.y = element_line(color='black'),
        legend.key.size = unit(5, 'mm'), #change legend key size
        legend.key.height = unit(4, 'mm'), #change legend key height
        legend.key.width = unit(3, 'mm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)

# combine these things

figure2=ggarrange(f1a1,dp2, 
                  labels = c("A ", "B"),
                  font.label = list(size = 12),
                  ncol = 2, nrow = 1,
                  widths = c(1, 1.5))
ggsave(paste0(dir_figs,'paper/Fig2more.jpg'),figure2,
       width = 150,height = 50,dpi = 1000,units = 'mm')


#extend x axis
dp3=ggplot(ESH %>%filter(ESHkm_curdams>0), aes(x=log10(ESHkm_curdams), y=log10(ESH_curdams)) ) + 
  geom_point(alpha=0.3, size=0.75) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression(' PLR (km²)')) + ylab(expression(' PLR (%)')) +
  scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2),labels = c("0.0001","0.001","0.01","0.1","1","10","100"), limits=c(-4.5,2)) +
  scale_x_continuous(breaks=c(0,2,4,6),labels=c("1","100","10,000","1,000,000"), limits=c(0,7)) +
  theme_bw() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(color='black',vjust = 0),
        axis.text.y = element_text(color='black',hjust = 0.5),
        axis.line.y = element_line(color='black'),
        legend.key.size = unit(5, 'mm'), #change legend key size
        legend.key.height = unit(4, 'mm'), #change legend key height
        legend.key.width = unit(3, 'mm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)

# combine these things

figure2=ggarrange(f1a1,dp3, 
                  labels = c("A ", "B"),
                  font.label = list(size = 12),
                  ncol = 2, nrow = 1,
                  widths = c(1, 1.5))
ggsave(paste0(dir_figs,'paper/Fig2more_scatterright.jpg'),figure2,
       width = 150,height = 50,dpi = 1000,units = 'mm')


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

# results vs CI
#stats
cor(ESH_CI$CI_current, log10(ESH_CI$ESH_curdams+1),  method = "pearson", use = "complete.obs")
cor(ESH_CI$CI_current, ESH_CI$ESH_curdams,  method = "spearman", use = "complete.obs")
cor.test(ESH_CI$CI_current, ESH_CI$ESH_curdams,  method = "spearman", use = "complete.obs")


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


CIcompkm_incl0log=ggplot(ESH_CI, aes(x= log10(ESHkm_curdams+1), y=CI_current))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, label.y = (0)) +
  theme_bw()

CIcompinclzero=ggarrange(CIcomp_incl0log, CIcompkm_incl0log, ncol=2,nrow=1)
ggsave(paste0(dir_figs,'paper/extra/CIcomp_incl0.jpg'),CIcompinclzero)

CIcomp2_incl0log=ggplot(ESH_CI, aes(x= ESH_curdams, y=CI_current))+
  geom_point() +
  xlab(expression('PLR (%)'))+
  ylab(expression('CI (%)'))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  #geom_smooth(method = "lm", formula = y ~ log10(x+0.000001), se = FALSE) +
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), parse = TRUE, label.y = (0)) +
  theme_bw()+
  theme(
    legend.position="none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-20,0,0,0),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color='grey'),
    panel.grid.major.x = element_line(linetype = 'dashed',color='grey'),
    text = element_text(size=10),
    axis.text.x = element_text(color='black',vjust = 0),
    axis.text.y = element_text(color='black',hjust = 0.5),
    axis.line.y = element_line(color='black'),
    axis.line.x = element_line(color='black'),
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
#CIcomp2_incl0log

ggsave(paste0(dir_figs,'paper/extra/CIcomp_incl0_new.jpg'), CIcomp2_incl0log,
       width = 150,height = 150,dpi = 1000,units = 'mm')


CIcomp3_incl0log=ggplot(ESH_CI, aes(x= ESH_curdams, y=CI_current))+
  geom_point(alpha=0.3, size=0.75) +
  xlab(expression('PLR (%)'))+
  ylab(expression('CI (%)'))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+ # special trans to keep zeros
  #geom_smooth(method = "lm", formula = y ~ log10(x+0.000001), se = FALSE) +
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), parse = TRUE, label.y = (0)) +
  theme_bw() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(color='black',vjust = 0),
        axis.text.y = element_text(color='black',hjust = 0.5),
        axis.line.y = element_line(color='black'),
        legend.key.size = unit(5, 'mm'), #change legend key size
        legend.key.height = unit(4, 'mm'), #change legend key height
        legend.key.width = unit(3, 'mm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6),#change legend text font size)
        panel.grid.minor = element_blank()
        ) 

ggsave(paste0(dir_figs,'paper/extra/CIcomp_incl0_newlayout.jpg'), CIcomp3_incl0log,
       width = 89,height = 89,dpi = 1000,units = 'mm')


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
  traits2$importance <- factor(traits2$importance, levels = c("No data","of no interest","of potential intrest","subsistence fisheries","minor commercial","commercial","highly commercial") )
  traits2$foodtrophcat <- factor(traits2$foodtrophcat, levels = c("No data","Herbi.","Carni.","Omni.") )
  traits2$climate_zone <- factor(traits2$climate_zone, levels = c("No data","1","2","3", "4", "5") ) 
  levels(traits2$climate_zone) <- c("No data", "A", "B", "C", "D", "E")
  # add length classes? to compare the impact on species of different lengths?
  
}
traits2[is.na(traits2)] <- "No data"
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
#allplots
# save
ggsave(paste0(dir_figs,'paper/extra/BP_AllSpecChar_incl0_sepNDNT.jpg'),allplots) #cannot be seen....

#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=plotList2[c(1:8)],
                         #labels = c("A", "B", "C", "D"),
                         ncol = length(ESHtypes), nrow = 4)
#threatplots
ggsave(paste0(dir_figs,'paper/extra/BP_SpecDamsThreat_incl0_sepNDNT.jpg'),threatplots) #better visible except score labels


#plots IUCN dams threat only
traitplots <- ggarrange(plotlist=plotList2[c(9:length(plotList2))],
                        #labels = c("A", "B", "C", "D"),
                        ncol = length(ESHtypes), nrow = 4)
#traitplots
ggsave(paste0(dir_figs,'paper/extra/BP_SpecTrait_incl0_exclNA.jpg'),traitplots) #ok



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
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000))  + # special trans to keep zeros
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
#threatplots
ggsave(paste0(dir_figs,'paper/2_SpecDamsThreat_incl0_sepND.jpg'),threatplots) #better visible except score labels




###################### excluding non affected species ###############################
# now not output figures

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
#allplots
# save
#ggsave(paste0(dir_figs,'paper/BP_AllSpecChar_excl0_sepNDNT.jpg'),allplots) #cannot be seen....

#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=plotList2[c(1:8)],
                         #labels = c("A", "B", "C", "D"),
                         ncol = length(ESHtypes), nrow = 4)
#threatplots
#ggsave(paste0(dir_figs,'paper/BP_SpecDamsThreat_excl0_sepNDNT.jpg'),threatplots) #better visible except score labels


#plots IUCN dams threat only
traitplots <- ggarrange(plotlist=plotList2[c(9:length(plotList2))],
                        #labels = c("A", "B", "C", "D"),
                        ncol = length(ESHtypes), nrow = 4)
#traitplots
#ggsave(paste0(dir_figs,'paper/BP_SpecTrait_excl0_exclNA.jpg'),traitplots) #ok












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


barplot1=ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+
  scale_fill_manual(values=c("red","blue", "grey"))+
  theme_bw()+
  aes(str_wrap_factor(myres, 15), value)+
  guides(fill=guide_legend(title="IUCN threat data")) +
  labs(y="Number of species", x=element_blank())

ggsave(paste0(dir_figs,'paper/IUCN_barplot1.jpg'),barplot1, width=12, height=7)


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


barplot2= ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+
  scale_fill_manual(values=c("red","yellow","orange","blue", "grey"))+
  theme_bw()+
  aes(str_wrap_factor(myres, 15), value)+
  labs(y="Number of species", x=element_blank())+
  guides(fill=guide_legend(title="IUCN threat data"))

ggsave(paste0(dir_figs,'paper/extra/IUCN_barplot2.jpg'),barplot2, width=12, height=7)

# IUCN comparison fig


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
               position = position_dodge(width = 0.75), size=4) +
  aes(str_wrap_factor(LorUdams, 10), ESH_curdams) +
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
    strip.text = element_text(angle = 0, size = 10)
  )


#first 2 figures showing no data, no threat, dams as threat
#adjust number position
give.n <- function(x){
  return(c(y = -1, label = length(x))) 
  # experiment with the number to find the perfect position
}
iucnfigkm=ggplot(tab_char, aes(x=LorUdams,y=ESHkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(ESHtypes[j]=="ESH_curdams"){}
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,100,10000,1000000))  + # special trans to keep zeros
  theme_bw() +
  #coord_cartesian(ylim = c(-2,99)) +
  coord_cartesian(ylim = c(-30,7000000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size=4) +
  aes(str_wrap_factor(LorUdams, 10), ESHkm_curdams) +
  #labs(y="ESH (%)",x="IUCN threat data") +
  labs(y="PLR (km²)",x=element_blank())  +
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
    strip.text = element_text(angle = 0, size = 10)
  )
#old one
give.n <- function(x){
  return(c(y = -0.25, label = length(x))) 
  # experiment with the number to find the perfect position
}

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


barplot3= ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
  geom_bar( stat="identity", position=position_stack(reverse=T))+
  scale_fill_manual(values=c("red","blue", "grey"))+
  theme_bw()+
  aes(str_wrap_factor(myres, 15), value)+
  labs(y="Number of species", x=element_blank())+
  guides(fill=guide_legend(title="IUCN threat data"))+
  theme(
    #legend.position="none",
    #legend.direction = "horizontal",
    #legend.title = element_blank(),
    #legend.margin=margin(0,0,0,0),
    #legend.box.margin=margin(-20,0,0,0),
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
    strip.text = element_text(angle = 0, size = 10)
  )


#plots IUCN dams threat only
tp=ggarrange(barplot3, labels = c("A"), 
          ggarrange(iucnfigperc,iucnfigkm, labels = c("B","C"),ncol=2), 
          nrow = 2)
threatplots <- ggarrange(plotlist=list(barplot3,iucnfigperc,iucnfigkm),
                         labels = c("A", "B", "C"),
                         ncol = 1, nrow = 3)
#threatplots
ggsave(paste0(dir_figs,'paper/IUCNcomparison_incl0_sepND.jpg'),tp,
       width = 150,height = 100,dpi = 1000,units = 'mm') #better visible except score labels


############################### PAF vs Fragment size ##########################################



# simple figures with this
SR_cur=read_sf(paste0(dir_out_tabs,"PAFinterbasins_cur_fwonly.gpkg"))

# PAF vs. fragment size
#plot(log10(SR_cur$ib_area),SR_cur$PAFcur, col=SR_cur$occ)


#density plot

nribs <- SR_cur %>% as.data.frame()%>% group_by(MAIN_BAS) %>% summarise(nrfrags=n()) 
morethanonefragment=nribs%>%filter(nrfrags>1)%>% pull(MAIN_BAS) # this is how many basins contain dams which separate subbasins from each other (creating multiple interbasins)
length(morethanonefragment)
SR_cur2 <- SR_cur %>% filter(MAIN_BAS %in% morethanonefragment)

dp=ggplot(SR_cur2, aes(x=log10(ib_area), y=PAFcur) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('Fragment size (km²)')) + ylab(expression('PAF'["species"])) +
  scale_x_continuous(labels=c("1","100","10.000","1.000.000"), breaks=c(0,2,4,6)) +
  theme_bw() +
  theme(text = element_text(size = 30))
#dp

ggsave(paste0(dir_figs,'paper/extra/PAF_vs_FragmentSize.jpg'),dp, width=12, height=7)


dp=ggplot(SR_cur2, aes(x=log10(ib_area), y=PAFcur) ) + 
  geom_point(alpha=0.2, size=0.75) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('Fragment size (km²)')) + ylab(expression('PAF'["species"])) +
  scale_x_continuous(labels=c("1","100","10.000","1.000.000"), breaks=c(0,2,4,6)) +
  theme_bw() +
  theme(text = element_text(size = 12))
#dp

ggsave(paste0(dir_figs,'paper/extra/PAF_vs_FragmentSize_dots.jpg'),dp, width=89,height=70,dpi = 1000,units = 'mm')



############################### STATS ##########################################

# stats in txt file

#get stats
# how many species experience fragmentation
sum(ESH %>% pull(fragmented_cur))/nrow(ESH)*100
sum(ESH %>% pull(fragmented_cur))


# how many species affected
nrow(ESH %>% filter(ESHkm_curdams!=0))
nrow(ESH %>% filter(ESH_curdams>10))
nrow(ESH %>% filter(ESH_curdams>5))
nrow(ESH %>% filter(ESHkm_curdams>10000))

#stats
mean(na.omit(impESHkm))
sd(na.omit(impESHkm))
mean(na.omit(impESHperc))
sd(na.omit(impESHperc))

sum(na.omit(impESHkm)!=0) #2840 species threatened
#stats zero excluded
#mean(na.omit(impESHkm[impESHkm!=0]))
#sd(na.omit(impESHkm[impESHkm!=0]))
#mean(na.omit(impESHperc[impESHperc!=0]))
#sd(na.omit(impESHperc[impESHperc!=0]))

# CI stats
sum(is.na(ESH_CI$CI_current)) #2648 species not there
sum(!is.na(ESH_CI$CI_current))
cor(ESH_CI$CI_current, log10(ESH_CI$ESH_curdams+1),  method = "pearson", use = "complete.obs")
cor(ESH_CI$CI_current, ESH_CI$ESH_curdams,  method = "spearman", use = "complete.obs")

length(morethanonefragment) # this is how many basins contain dams which separate subbasins from each other

# save stats in txt file in dir_proc_out
write.csv(left_join(ESH[order(ESH$ESH_curdams,decreasing=TRUE),],traits), paste0(dir_figs,"PLR_perc_top100.csv"), row.names = F)
write.csv(left_join(ESH[order(ESH$ESHkm_curdams,decreasing=TRUE),],traits), paste0(dir_figs,"PLR_km_top100.csv"), row.names = F)
write.table(bars,paste0(dir_figs,"stats_IUCN_comp.txt"), row.names = F)
fileConn<-file(paste0(dir_figs,"stats.txt"))

writeLines(c(paste0("Relation type: ", mva_type,", Dams data: ", dams_used),
             paste0(sum(ESH %>% pull(fragmented_cur))," species experience fragmentation, of all species (n= ",nrow(ESH),"): ",sum(ESH %>% pull(fragmented_cur))/nrow(ESH)*100,"%"),
             paste0("Of all main basins, ", length(morethanonefragment), " exist of multiple interbasins (so are fragmented)"),
             paste0(nrow(ESH %>% filter(ESHkm_curdams!=0))," species are affected by isolation (PLR>0), ",
                    nrow(ESH %>% filter(ESH_curdams==100))," species have PLR=100%, ",nrow(ESH %>% filter(ESH_curdams>75))," species have PLR>75%,",
                    nrow(ESH %>% filter(ESH_curdams>50))," species have PLR>50%, ",nrow(ESH %>% filter(ESH_curdams>10))," species have PLR>10%, ", nrow(ESH %>% filter(ESH_curdams>5)), " species have PLR>5%, ",
                    nrow(ESH %>% filter(ESHkm_curdams>100000))," species have PLR>100.000km2, ",
                    nrow(ESH %>% filter(ESHkm_curdams>10000))," species have PLR>10.000km2, ", nrow(ESH %>% filter(ESHkm_curdams>1000)), " species have PLR>1.000km2" ),
             paste0("Average PLR: ",mean(na.omit(impESHkm))," km2 +- ",sd(na.omit(impESHkm)), " SD or 95% range:",quantile(na.omit(impESHkm),probs=c(0.025,0.975))," or ", mean(na.omit(impESHperc)), " % +- ", sd(na.omit(impESHperc)), "SD and 95% range",quantile(na.omit(impESHperc),probs=c(0.025,0.975))),
             paste0("Comparing with Valerio's CI values; matches for ",sum(!is.na(ESH_CI$CI_current))," species. Spearmans rank: ", cor(ESH_CI$CI_current, ESH_CI$ESH_curdams,  method = "spearman", use = "complete.obs"),
                    ",  Pearsons r (log10(PLR+1):", cor(ESH_CI$CI_current, log10(ESH_CI$ESH_curdams+1),  method = "pearson", use = "complete.obs") ) ), fileConn)
close(fileConn)

quantile(impESHperc,probs=c(0.025,0.975))
quantile(impESHkm,probs=c(0.025,0.975))




### extra ##### 
#creates gpkgs
cat('Reading hb12..\n')
hblakes12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp'))
  return(poly)
}


# where do the species live with high impact stuff



# choose here what you want to know because it takes much time to write many features

#10 percent
check_species=ESH[order(ESH$ESH_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(ESH_curdams>10)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
cat('Reading fish  data..\n')
# get the location of the species
dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
#st_write(fish_sf, paste0(dir_figs,"PLR_perc_largerthan10.gpkg"))

#50 percent
check_species=ESH[order(ESH$ESH_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(ESH_curdams>50)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
cat('Reading fish  data..\n')
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
st_write(fish_sf, paste0(dir_figs,"PLR_perc_largerthan50.gpkg"))

#75 percent
check_species=ESH[order(ESH$ESH_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(ESH_curdams>75)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
cat('Reading fish  data..\n')
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
#st_write(fish_sf, paste0(dir_figs,"PLR_perc_largerthan75.gpkg"))

#100 percent
if(nrow(ESH %>% filter(ESH_curdams==100)) !=0){
  check_species=ESH[order(ESH$ESH_curdams,decreasing=TRUE),]
  check_species=check_species %>% filter(ESH_curdams==100)
  ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
  # species data on hydrobasins lakes level 12
  cat('Reading fish  data..\n')
  fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
    return(t)
  }
  fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
  st_write(fish_sf, paste0(dir_figs,"PLR_perc_100perc.gpkg"))
}


# km
check_species=ESH[order(ESH$ESHkm_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(ESHkm_curdams>10000)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)

# get the location of the fish
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
#st_write(fish_sf, paste0(dir_figs,"PLR_km_largerthan10000.gpkg"))

#1,000,000 km
if(nrow(ESH %>% filter(ESHkm_curdams>1000000)) !=0){
  check_species=ESH[order(ESH$ESHkm_curdams,decreasing=TRUE),]
  check_species=check_species %>% filter(ESHkm_curdams>1000000)
  ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
  
  # get the location of the fish
  fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
    return(t)
  }
  fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
  #st_write(fish_sf, paste0(dir_figs,"PLR_km_largerthan10000.gpkg"))
}




# species endangered without dams
ESH2 <- read.csv(out)
ESH2 <- ESH2 %>% filter(area_adjusted_km2==0)
ids_oi <- ids %>% filter(binomial %in% ESH2$binomial)
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
st_write(fish_sf, paste0(dir_figs,"sp_out_nodams.gpkg"))

#}