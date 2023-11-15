# Make figures for DI paper
# Tamara Keijzer
# april 2023

# PLR = potentially lost range

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")
library(ggplot2)
library(ggpubr)
library(tidyr)

colors=c('#f1a340', "#998ec3")
colors2=c('#deebf7','#3182bd')
colors_bar=c('#f46d43','#f46d43', '#d73027','#a50026','#fee090',  '#4575b4')
colors= 'Grey70' # just grey for the violin plot


dir_("Dams_impact/figs/Global/")
dir_figs <- dir_(paste0('Dams_impact/figs/Global/',mvrs_type,'_',dams_used,'/'))
dir_(paste0(dir_figs,"paper/"))
dir_(paste0(dir_figs,"paper/extra/"))
dir_(paste0(dir_figs,"paper/maps/"))
dir_(paste0(dir_figs,"paper/maps/extra/"))

#processed data
dir_proc <- dir_('Dams_impact/proc/') #general (interbasins)
dir_proc_out <- dir_(paste0('Dams_impact/proc/',mvrs_type,'_',dams_used,'/')) # specific for relation

# inputs
dir_out <- dir_(paste0(dir_proc_out,"model_DI_occurrence/"))
dir_out_tabs <- dir_(paste0(dir_proc_out,"tabs/"))
dir_threattab <- "Fishdata/IUCN_DamsThreat.csv"

#load inputs
# species number, name, traits data, iucn threat data
traits <- read.csv('Fishdata/species_traits_extra.csv')
ids <- traits %>% dplyr::select(id_no,binomial)
threattab <- read.csv(dir_threattab) # whether dams are indicated as a threat




# functions to get the true mean when the y scale is log transformed (otherwise mean of transformed values is shown)

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


# PLR (%/km) division
#PLR
out <- paste0(dir_out_tabs,'PLRfragm_tabDI_fwonly.csv')
PLR <- read.csv(out)

species_out= left_join(data.frame(PLR %>% filter(area_adjusted_km2==0) %>% select(binomial,area_total_km2)),traits)
write.csv(species_out,paste0(dir_figs,"species_fully_out.csv"), row.names = F) # the species that are removed entirely from the analysis due to their range area already being below MVRS
#remove species that already are fully endangered without dams
PLR <- PLR %>% filter(area_adjusted_km2!=0)

# get a nice table for SI material
PLR_table=data.frame(binomial=PLR %>% select(binomial), range_total_km= PLR %>% pull(area_adjusted_km2) ,PLR_km= PLR %>% pull(PLRkm_curdams), PLR_perc = PLR %>% pull(PLR_curdams))
write.csv(PLR_table,paste0(dir_figs,"SI_Results_species_specific.csv"), row.names = F)




#get values for df
impPLRperc=PLR%>%
  pull(PLR_curdams)

impPLRkm=PLR%>%
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
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(angle = 0, size = 16)
  )



#extend x axis
dp=ggplot(PLR %>%filter(PLRkm_curdams>0), aes(x=log10(PLRkm_curdams), y=log10(PLR_curdams)) ) + 
  geom_bin2d(bins = 50) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
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

figure2=ggarrange(f1a1,dp, 
                  labels = c("A ", "B"),
                  font.label = list(size = 12),
                  ncol = 2, nrow = 1,
                  widths = c(1, 1.5))
ggsave(paste0(dir_figs,'paper/figure2_densityplotright.jpg'),figure2,
       width = 150,height = 50,dpi = 1000,units = 'mm')


#extend x axis
f1b=ggplot(PLR %>%filter(PLRkm_curdams>0), aes(x=log10(PLRkm_curdams), y=log10(PLR_curdams)) ) + 
  geom_point(alpha=0.3, size=0.75) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
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
figure2=ggarrange(f1a1,f1b, 
                  labels = c("A ", "B"),
                  font.label = list(size = 12),
                  ncol = 2, nrow = 1,
                  widths = c(1, 1.5))
ggsave(paste0(dir_figs,'paper/figure2.jpg'),figure2,
       width = 150,height = 50,dpi = 1000,units = 'mm')


#########################################################################################################################################################################################
# CI Valerio
#########################################################################################################################################################################################


# CI values Valerio
CI=read.csv("Fishdata/CI_Valerio/pnasCI.csv")
colnames(CI)[1]="binomial" # these binomials should be fishbase names, so couple it to fishbase column in traits

#get fishbase names 
# if it doesnt exist yet, do this:
if(file.exists("Fishdata/CI_Valerio/CImatch_fb21names.csv")==FALSE){
  library(rfishbase)
  options(FISHBASE_VERSION="21.06")
  fb_names=array()
  your_list=unique(CI$binomial) # this is not an actual R list item, just a vector with each element a species name
  for(i in 1:length(your_list)) {
    fb_names[i]=as.character(validate_names(your_list[i])[1])
  }
  CI_fbnames=data.frame(binomial=your_list,fb_name=fb_names) # 16 NAs
  write.csv(CI_fbnames,"Fishdata/CI_Valerio/CImatch_fb21names.csv", row.names = F) 
}
CI_fbnames=read.csv("Fishdata/CI_Valerio/CImatch_fb21names.csv")
CI_names=left_join(CI,CI_fbnames) %>% select(-binomial) # want to match the CI values by fb_name, so remove the binomial column



# df with binomial IUCN, fishbase name and CI values
CIspec=left_join(traits %>% select(binomial,fb_name),CI_names %>% select(fb_name, CI_current, CI_future)) 
#so now we can compare CI value and our results
PLR_CI=left_join(PLR,CIspec) # they will match on binomal name of our dataset, NA for species we dont have values for
sum(is.na(PLR_CI$CI_current)) #missing nr of species


library(lubridate)
library(ggpmisc)
#compare CI and my results (including zero values)
CIcomp_incl0log=ggplot(PLR_CI, aes(x= PLR_curdams, y=CI_current))+
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

ggsave(paste0(dir_figs,'paper/extra/CIcomparison.jpg'), CIcomp_incl0log,
       width = 89,height = 89,dpi = 1000,units = 'mm')


#########################################################################################################################################################################################
# IUCN dams threat comparison
#########################################################################################################################################################################################

#################
## prep tables ##
#################

# add IUCN info on dams threat
compIUCN <- left_join(PLR,threattab) #2774 no data
traits2=traits

# complement the existing tables

# if we want to include NAs as a group:

  # Replace NAs in seveity, scope and score; fill in Unknown when dams is TRUE, if Dams is FALSE fill in no threat
  
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
  
  # rest of NAs "No data"
  compIUCN[is.na(compIUCN)] <- "No data"
  
  #nodata first for convenience plots
  compIUCN$LD_scope <- factor(compIUCN$LD_scope,levels=c("No data", "No threat","Unknown","Minority (<50%)","Majority (50-90%)","Whole (>90%)"  ))
  compIUCN$LD_scope_fut <- factor(compIUCN$LD_scope_fut,levels=c("No data", "No threat","Unknown","Minority (<50%)","Majority (50-90%)","Whole (>90%)"))
  compIUCN$LD_sev <- factor(compIUCN$LD_sev,levels=c("No data", "No threat","Unknown","No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations"))
  compIUCN$LD_sev_fut <- factor(compIUCN$LD_sev_fut,levels=c("No data", "No threat","Unknown","No decline","Negligible declines","Slow, Significant Declines", "Rapid Declines", "Very Rapid Declines", "Causing/Could cause fluctuations" ))
  compIUCN$LD_score <- factor(compIUCN$LD_score,levels=c("No data", "No threat","Unknown","Low Impact: 3","Low Impact: 4","Low Impact: 5","Medium Impact: 6", "Medium Impact: 7", "High Impact: 8","High Impact: 9" ))
  compIUCN$LD_score_fut <- factor(compIUCN$LD_score_fut,levels=c("No data", "No threat","Unknown","No/Negligible Impact: 1","Low Impact: 3","Low Impact: 4", "Low Impact: 5","Medium Impact: 6"))
 
  # fix flaws in traits table
  traits2$importance[which(traits2$importance=="")] <- NA
  traits2$importance[which(traits2$importance==" ")] <- NA
  traits2$importance=as.character(traits2$importance)
  traits2[is.na(traits2)] <- "No data"
  traits2$code[which(traits2$code=="LRlc")] <- "LC"
  # set levels
  traits2$code <- factor(traits2$code, levels = c("DD","LC","NT","VU","EN","CR","EW","EX") )
  traits2$importance <- factor(traits2$importance, levels = c("No data","of no interest","of potential intrest","subsistence fisheries","minor commercial","commercial","highly commercial") )
  traits2$foodtrophcat <- factor(traits2$foodtrophcat, levels = c("No data","Herbi.","Carni.","Omni.") )
  #traits2$climate_zone <- factor(traits2$climate_zone, levels = c("No data","1","2","3", "4", "5") ) 
  #levels(traits2$climate_zone) <- c("No data", "A", "B", "C", "D", "E")
  # NA as "No data"
  traits2[is.na(traits2)] <- "No data"


# combine info large dams and unknown dams to have more data
#only looking at ongoing threats and current dams results

# if Large dams = F and dams of unknown size = T, use these scope, severity and score values.
compIUCN$LorUdams <- (compIUCN$Ldams==T | compIUCN$Udams==T)
compIUCN$LorUdams[which(compIUCN$Ldams=="No data")] <- "No data" 
compIUCN$LorUdams[which(compIUCN$LorUdams==F)] <- "Dams not indicated as threat" 
compIUCN$LorUdams[which(compIUCN$LorUdams==T)] <- "Dams indicated as threat" 
compIUCN$LorUdams <- factor(compIUCN$LorUdams, levels=c("No data","Dams not indicated as threat", "Dams indicated as threat"))

# also combine info on scope severity and score
compIUCN$LU_scope <- compIUCN$LD_scope
compIUCN$LU_scope[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_scope[which(compIUCN$Ldams==F & compIUCN$Udams==T)]
compIUCN$LU_sev <- compIUCN$LD_sev
compIUCN$LU_sev[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_sev[which(compIUCN$Ldams==F & compIUCN$Udams==T)]
compIUCN$LU_score <- compIUCN$LD_score
compIUCN$LU_score[which(compIUCN$Ldams==F & compIUCN$Udams==T)] <- compIUCN$UD_score[which(compIUCN$Ldams==F & compIUCN$Udams==T)]

# result PLR values and species characteristics
tab_char <- left_join(compIUCN,traits2 %>%select(binomial,importance,foodtrophcat,
                                                 #climate_zone,
                                                 code))




################################
## prepare tables for figures ##
################################

# make a long table to facet things (nope does not work) and get boxplots per group
# rename variables of interest to logical names
tab_char <- tab_char %>% rename("IUCN threat scope"="LU_scope", "IUCN threat severity"="LU_sev", "IUCN threat score"="LU_score",
                                "IUCN threat status"="code", "Commercial importance" = "importance",
                                "Trophic level"="foodtrophcat"
                                #, "Climate zone"= "climate_zone"
                                )

vars=c("IUCN threat scope","IUCN threat severity","IUCN threat score","IUCN threat status","Commercial importance","Trophic level"
       #,"Climate zone"
       )
n=length(vars)

# formulas
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


############################### IUCN SI FIGURE ##########################################

# IUCN comparison fig
iucnfigperc=ggplot(tab_char, aes(x=LorUdams,y=PLR_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(PLRtypes[j]=="PLR_curdams"){}
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  #{if(lops$PLRtypes[i]=="PLRkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(-2,99)) +
  #{if(lops$PLRtypes[i]=="PLRkm_curdams")coord_cartesian(ylim = c(-1,700000)) } +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size=4) +
  aes(str_wrap_factor(LorUdams, 10), PLR_curdams) +
  labs(y="PLR (%)",x=element_blank()) +
  #{if(lops$PLRtypes[i]=="PLRkm_curdams")labs(y="PLR (km²)",x=lops$vars[i]) } +
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
iucnfigkm=ggplot(tab_char, aes(x=LorUdams,y=PLRkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  #if(PLRtypes[j]=="PLR_curdams"){}
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,100,10000,1000000))  + # special trans to keep zeros
  theme_bw() +
  #coord_cartesian(ylim = c(-2,99)) +
  coord_cartesian(ylim = c(-30,7000000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size=4) +
  aes(str_wrap_factor(LorUdams, 10), PLRkm_curdams) +
  #labs(y="PLR (%)",x="IUCN threat data") +
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

#adjust number position
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
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams indicated as threat") %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams indicated as threat") %>% filter(PLRkm_curdams!=0)))
)

bars$myres <- factor(bars$myres,levels=c("No fragmentation","Fragmentation: not affected", "Affected"))
bars$IUCNgroup <- factor(bars$IUCNgroup,levels=(c("Dams indicated as threat","Dams not indicated as threat",  "No data")))

barplot= ggplot(bars, aes(fill=IUCNgroup, y=value, x=myres)) + 
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
tp=ggarrange(barplot, labels = c("A"), 
          ggarrange(iucnfigperc,iucnfigkm, labels = c("B","C"),ncol=2), 
          nrow = 2)

#threatplots
ggsave(paste0(dir_figs,'paper/IUCNcomparison_incl0.jpg'),tp,
       width = 150,height = 100,dpi = 1000,units = 'mm') #better visible except score labels






############################### SI PAF vs Fragment size ##########################################

# simple figures with this
SR_cur=readRDS(paste0(dir_out_tabs,"RESinterbasins_cur_fwonly.rds"))

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

ggsave(paste0(dir_figs,'paper/extra/PAF_vs_FragmentSize.jpg'),dp, width=12, height=7)

#scatter plot
dp=ggplot(SR_cur2, aes(x=log10(ib_area), y=PAFcur) ) + 
  geom_point(alpha=0.2, size=0.75) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  #ggtitle(paste(spec_value,if(spec_value =="quantile"){quant_value}, "per species")) +
  xlab(expression('Fragment size (km²)')) + ylab(expression('PAF'["species"])) +
  scale_x_continuous(labels=c("1","100","10.000","1.000.000"), breaks=c(0,2,4,6)) +
  theme_bw() +
  theme(text = element_text(size = 12))

ggsave(paste0(dir_figs,'paper/extra/PAF_vs_FragmentSize_dots.jpg'),dp, width=89,height=70,dpi = 1000,units = 'mm')



############################### STATS ##########################################

# stats in txt file

#get stats

# save stats in txt file in dir_proc_out
write.csv(left_join(PLR[order(PLR$PLR_curdams,decreasing=TRUE),],traits), paste0(dir_figs,"PLR_perc_top.csv"), row.names = F)
write.csv(left_join(PLR[order(PLR$PLRkm_curdams,decreasing=TRUE),],traits), paste0(dir_figs,"PLR_km_top.csv"), row.names = F)
write.table(bars,paste0(dir_figs,"stats_IUCN_comp.txt"), row.names = F)

fileConn<-file(paste0(dir_figs,"stats.txt"))

writeLines(c(paste0("Relation type: ", mvrs_type,", Dams data: ", dams_used),
             paste0(sum(PLR %>% pull(fragmented_cur))," species experience fragmentation, of all species (n= ",nrow(PLR),"): ",sum(PLR %>% pull(fragmented_cur))/nrow(PLR)*100,"%"),
             paste0("Of all main basins, ", length(morethanonefragment), " exist of multiple interbasins (so are fragmented)"),
             paste0(nrow(PLR %>% filter(PLRkm_curdams!=0))," species are affected by isolation (PLR>0), ",
                    nrow(PLR %>% filter(PLR_curdams==100))," species have PLR=100%, ",nrow(PLR %>% filter(PLR_curdams>75))," species have PLR>75%,",
                    nrow(PLR %>% filter(PLR_curdams>50))," species have PLR>50%, ",nrow(PLR %>% filter(PLR_curdams>10))," species have PLR>10%, ", nrow(PLR %>% filter(PLR_curdams>5)), " species have PLR>5%, ",
                    nrow(PLR %>% filter(PLRkm_curdams>100000))," species have PLR>100.000km2, ",
                    nrow(PLR %>% filter(PLRkm_curdams>10000))," species have PLR>10.000km2, ", nrow(PLR %>% filter(PLRkm_curdams>1000)), " species have PLR>1.000km2" ),
             paste0("Average PLR: ",mean(na.omit(impPLRkm))," km2 +- ",sd(na.omit(impPLRkm)), " SD or 95% range:",as.vector(quantile(na.omit(impPLRkm),probs=c(0.025,0.975)))," or ", mean(na.omit(impPLRperc)), " % +- ", sd(na.omit(impPLRperc)), "SD and 95% range",as.vector(quantile(na.omit(impPLRperc),probs=c(0.025,0.975)))),
             paste0("Comparing with Valerio's CI values; matches for ",sum(!is.na(PLR_CI$CI_current))," species. Spearmans rank: ", cor(PLR_CI$CI_current, PLR_CI$PLR_curdams,  method = "spearman", use = "complete.obs"),
                    ",  Pearsons r (log10(PLR+1):", cor(PLR_CI$CI_current, log10(PLR_CI$PLR_curdams+1),  method = "pearson", use = "complete.obs") ) ), fileConn)
close(fileConn)



# to check significance
cor(PLR_CI$CI_current, PLR_CI$PLR_curdams,  method = "spearman", use = "complete.obs")
cor(PLR_CI$CI_current, log10(PLR_CI$PLR_curdams+1),  method = "pearson", use = "complete.obs")












### extra ##### 
#creates gpkgs
cat('Reading hb12..\n')
hblakes12 <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas12,'/hybas_lake_',cont,'_lev12_v1c.shp'))
  return(poly)
}

# choose here what you want to know because it takes much time to write many features



# where do the species live with high impact stuff

#more than 50 percent PLR
check_species=PLR[order(PLR$PLR_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(PLR_curdams>50)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
cat('Reading fish  data..\n')
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
st_write(fish_sf, paste0(dir_figs,"PLR_perc_largerthan50.gpkg"))

#100 percent PLR
if(nrow(PLR %>% filter(PLR_curdams==100)) !=0){
  check_species=PLR[order(PLR$PLR_curdams,decreasing=TRUE),]
  check_species=check_species %>% filter(PLR_curdams==100)
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


# fish species with more than 10,000 km2 PLR
check_species=PLR[order(PLR$PLRkm_curdams,decreasing=TRUE),]
check_species=check_species %>% filter(PLRkm_curdams>10000)
ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
#st_write(fish_sf, paste0(dir_figs,"PLR_km_largerthan10000.gpkg"))

# more than 1,000,000 km PLR
if(nrow(PLR %>% filter(PLRkm_curdams>1000000)) !=0){
  check_species=PLR[order(PLR$PLRkm_curdams,decreasing=TRUE),]
  check_species=check_species %>% filter(PLRkm_curdams>1000000)
  ids_oi <- ids %>% filter(binomial %in% check_species$binomial)
  
  # get the location of the fish
  fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
    t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
    return(t)
  }
  fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
  #st_write(fish_sf, paste0(dir_figs,"PLR_km_largerthan100000.gpkg"))
}




# species endangered without dams (range area < MVRS)
PLR2 <- read.csv(out)
PLR2 <- PLR2 %>% filter(area_adjusted_km2==0)
ids_oi <- ids %>% filter(binomial %in% PLR2$binomial)
fish <- foreach(sp = ids_oi$id_no,.combine='rbind') %do% {
  t <- readRDS(paste0(dir_out,sp,'.rds')) %>% ungroup() %>% dplyr::select(binomial,MAIN_BAS,HYBAS_ID,SUB_AREA,hab_nodams,hab_curdams, INTER_ID_cur)
  return(t)
}
fish_sf <- left_join(fish,hblakes12)%>% st_as_sf()
st_write(fish_sf, paste0(dir_figs,"sp_out_nodams.gpkg"))

#}




# extra figs
# IUCN threat large dams and dams of unknown size separated
bars=data.frame(
  myres=c(rep(c("No fragmentation","Fragmentation: not affected", "Affected"),each=5)),
  IUCNgroup=c(rep(c("No data","Dams not indicated as threat", "Threat: Large dams", "Threat: Dams of unknown size ", "Threat: Large dams and dams of unknown size"))),
  value=c(nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(fragmented_cur==F)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(PLRkm_curdams==0&fragmented_cur==T)),
          nrow(tab_char %>% filter(LorUdams=="No data") %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(LorUdams=="Dams not indicated as threat") %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(Ldams==T & Udams==F) %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(Udams==T & Ldams==F) %>% filter(PLRkm_curdams!=0)),
          nrow(tab_char %>% filter(Ldams==T & Udams==T) %>% filter(PLRkm_curdams!=0))
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
ggsave(paste0(dir_figs,'paper/extra/IUCN_barplot_damgroupsseparated.jpg'),barplot2, width=12, height=7)







#species characteristics and IUCN dams threat scope severity and score
######################### extra figure #################################
# 
# formulas
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


#PLR km
datalist = vector("list", length = n)
for (i in 1:n) {
  # ... make some data
  d <- tab_char %>% select(PLRkm_curdams,vars[i]) %>% reshape2::melt(.,id=vars[i])
  colnames(d)[1]= "group"
  colnames(d)[2]= "PLR_type"
  d$var=vars[i]
  datalist[[i]] <- d # add it to your list
}
big_datakm = do.call(rbind, datalist)

# PLR %
datalist = vector("list", length = n)
for (i in 1:n) {
  # ... make some data
  d <- tab_char %>% select(PLR_curdams,vars[i]) %>% reshape2::melt(.,id=vars[i])
  colnames(d)[1]= "group"
  colnames(d)[2]= "PLR_type"
  d$var=vars[i]
  datalist[[i]] <- d # add it to your list
}
big_dataperc = do.call(rbind, datalist)

#combine
big_data=rbind(big_dataperc,big_datakm)
#cannot use facet grid because manual log scales perc or km results



#make a list of plot because facetting does not work as there are different x and y axises
PLRtypes=c("PLR_curdams","PLRkm_curdams")
lops=expand.grid(PLRtypes,vars)
colnames(lops)=c("PLRtypes","vars")


#first 2 figures showing no data, no threat, dams as threat
iucnfigperc=ggplot(tab_char, aes(x=LorUdams,y=PLR_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) +
  theme_bw() +
  coord_cartesian(ylim = c(-2,99)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), PLR_curdams) +
  labs(y="PLR (%)",x="IUCN threat data") +
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
    axis.text.y = element_text(color='black',hjust = 0.5),
    axis.line.y = element_line(color='black'),
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(angle = 0, size = 16)
  )

iucnfigkm=ggplot(tab_char, aes(x=LorUdams,y=PLRkm_curdams) ) +
  geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
  geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))  + # special trans to keep zeros
  theme_bw() +
  coord_cartesian(ylim = c(-5,700000)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75)) +
  aes(str_wrap_factor(LorUdams, 15), PLRkm_curdams) +
  labs(y="PLR (km²)",x="IUCN threat data")  +
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
    axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
    axis.text.y = element_text(color='black',hjust = 0.5),
    axis.line.y = element_line(color='black'),
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(angle = 0, size = 16)
  )


# per variable, for the variables scope, severity, score filter out no data or no threat species
plotList <- lapply(1:nrow(lops), function(i){
  ggplot(big_data %>%filter(var==lops$vars[i]) %>% filter(PLR_type==lops$PLRtypes[i]) %>% filter(group %!in% c("No data","No threat"))%>%droplevels(.), aes(x=group,y=value) ) +
    geom_violin(lwd = .5,color='transparent', fill=colors[1]) +
    geom_boxplot(outlier.size = 0.5, outlier.shape = 1, width = 0.08, fill="white")+
    {if(lops$PLRtypes[i]=="PLR_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100)) }+
    {if(lops$PLRtypes[i]=="PLRkm_curdams")scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,10,100,1000,10000,100000,1000000))}  + # special trans to keep zeros
    theme_bw() +
    {if(lops$PLRtypes[i]=="PLR_curdams")coord_cartesian(ylim = c(-2,99))} +
    {if(lops$PLRtypes[i]=="PLRkm_curdams")coord_cartesian(ylim = c(-5,700000)) } +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                 position = position_dodge(width = 0.75)) +
    aes(str_wrap_factor(group, 15), value) +
    {if(lops$PLRtypes[i]=="PLR_curdams")labs(y="PLR (%)",x=lops$vars[i])} +
    {if(lops$PLRtypes[i]=="PLRkm_curdams")labs(y="PLR (km²)",x=lops$vars[i]) } +
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
      axis.text.x = element_text(color='black',vjust = 0), #, angle = 90
      axis.text.y = element_text(color='black',hjust = 0.5),
      axis.line.y = element_line(color='black'),
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
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

#plots IUCN dams threat only
threatplots <- ggarrange(plotlist=plotList2[c(1:8)],
                         ncol = length(PLRtypes), nrow = 4)
#threatplots
ggsave(paste0(dir_figs,'paper/extra/PLR_SpecDamsThreat_inclPLR0.jpg'),threatplots) #ok visible except score labels


#plots PLR values devided over categories of species characteristics
traitplots <- ggarrange(plotlist=plotList2[c(9:length(plotList2))],
                        #labels = c("A", "B", "C", "D"),
                        ncol = length(PLRtypes), nrow = 4)
#traitplots
ggsave(paste0(dir_figs,'paper/extra/PLR_SpecTraits_inclPLR0.jpg'),traitplots) #ok

