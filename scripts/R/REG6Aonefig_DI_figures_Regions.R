

# general
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")


library(ggplot2)
library(tidyr)


# functions
# for log scale 
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


# prep tables and make figures



### KM ###
# create for each region a table
region="USA"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="United States", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="United States", variable="large")%>%filter(!is.na(value))

sp_usa=rbind(sp_largesmall,sp_large)


region="MEK"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="greater Mekong", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="greater Mekong", variable="large") %>%filter(!is.na(value))

sp_mek=rbind(sp_largesmall,sp_large)


region="BRA"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="Brazil", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLRkm_curdams) %>% mutate(region="Brazil", variable="large")%>%filter(!is.na(value))

sp_bra=rbind(sp_largesmall,sp_large)


tab <- bind_rows(sp_usa,sp_bra,sp_mek) %>%
  mutate(variable = factor(variable))
levels(tab$variable) <- c('large','large+small')


# stats
stats <- tab %>%
  spread(variable,value) %>%
  mutate(diff = large - `large+small`) %>%
  group_by(region) %>%
  summarize(rsq = paste0('R2 = ',round(valerioUtils::r.squared(`large+small`,large),2)),
            rmse = paste0('RMSE = ',round(valerioUtils::rmse(`large+small`,large),0)),
            n = paste0('n = ',length(unique(binomial))),
            mean_diff = round(mean(diff),2)
  )


#figure
p1 <- ggplot(data = tab, aes(x = variable, y = value)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1, outlier.size = 0.5, outlier.shape = 1, outlier.alpha = 0.5, alpha=0.9) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=scales::comma_format(accuracy=1), breaks=c(0,100,1000,10000,100000))+ # special trans to keep zeros
  facet_wrap('region') +
  xlab(' ') +
  ylab('PLR (km²)') +
  stat_summary_two(fun.y=mean, geom="point", aes(fill = variable),
                   shape=18, size=2,show.legend = FALSE, color = 'red',fill='red') +
  theme_bw() +
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
    strip.text = element_text(angle = 0, size = 10)
  )
p1





### PERCENTAGE ###
# create for each region a table
region="USA"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="United States", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="United States", variable="large")%>%filter(!is.na(value))

sp_usa=rbind(sp_largesmall,sp_large)


region="MEK"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="greater Mekong", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="greater Mekong", variable="large") %>%filter(!is.na(value))

sp_mek=rbind(sp_largesmall,sp_large)


region="BRA"
out <- paste0('Dams_impact/proc/',region,'/BOTH/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')
out2 <- paste0('Dams_impact/proc/',region,'/BOTH_large/model_DI_occurrence/',mvrs_type,'/tabs/PLRfragm_tabDI_fwonly.csv')

sp_largesmall=read.csv(out) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="Brazil", variable="large+small")%>%filter(!is.na(value))
sp_large=read.csv(out2) %>%
  select(binomial, value=PLR_curdams) %>% mutate(region="Brazil", variable="large")%>%filter(!is.na(value))

sp_bra=rbind(sp_largesmall,sp_large)


tab <- bind_rows(sp_usa,sp_bra,sp_mek) %>%
  mutate(variable = factor(variable))
levels(tab$variable) <- c('large','large+small')


# stats
stats <- tab %>%
  spread(variable,value) %>%
  mutate(diff = large - `large+small`) %>%
  group_by(region) %>%
  summarize(rsq = paste0('R2 = ',round(valerioUtils::r.squared(`large+small`,large),2)),
            rmse = paste0('RMSE = ',round(valerioUtils::rmse(`large+small`,large),0)),
            n = paste0('n = ',length(unique(binomial))),
            mean_diff = round(mean(diff),2)
  )


# figure
p2 <- ggplot(data = tab, aes(x = variable, y = value)) +
  geom_violin(scale='width',color = 'transparent',fill = 'Grey70',alpha = 0.8) +
  geom_boxplot(fill = 'white',width = 0.1, outlier.size = 0.2, outlier.shape = 1, outlier.alpha = 0.5, alpha=0.9) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels=c(0,1,10,25,50,100), breaks=c(0,1,10,25,50,100))+
  facet_wrap('region') +
  xlab(' ') +
  ylab('PLR (%)') +
  stat_summary_two(fun.y=mean, geom="point", aes(fill = variable),
                   shape=18, size=2,show.legend = FALSE, color = 'red',fill='red') +
  theme_bw() +
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
    strip.text = element_text(angle = 0, size = 10)
  )
p2

library(ggpubr)
allplots <- ggarrange(p2,p1,
                      labels = c("A ", "B"),
                      font.label = list(size = 12),
                      ncol = 1, nrow = 2)

ggsave(filename = "Dams_impact/figs/REGIONAL_compare_largesmalldams_perckm2.jpg",allplots,
       width = 114,height = 116,units = 'mm',dpi = 1000)






