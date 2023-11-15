# Get the MVA relationship 
# Approach:
# Sum ranges within basins
# Filter on species (lakes/marine/brackish)
# Filter on basins (Tedesco)
# Tamara Keijzer
# October 2022


#set-up
local_model_folder="I:/tamara/CC_dams_R"
setwd(local_model_folder)
source("scripts/SETTINGS_Dams_impact.R")

library(dplyr);library(sf);library(foreach);library(sp);library(spatialEco)
library(ggplot2)
sf::sf_use_s2(FALSE)


#-------------------------------------------------------------------------------
# Choose
#-------------------------------------------------------------------------------

# FALSE = exclude, TRUE = include
lentic_only = FALSE
marine = FALSE
brackish = FALSE
diadromous = FALSE
vulnerable = FALSE
iucncode_included = c("LC","NT") # only works when vulnerable = FALSE
# DD = data deficient , LC = least concern , NT = near threatened ,  
# VU = vulnerable, EN = endangered , CR = critically endangered ,
# EW = Extinct in the wild , EX = extinct
if(vulnerable = TRUE){iucncode_included = c("LC","NT","VU","EN","CR")}

# filter on basins? so exclude = TRUE
coastal_basins_filtered_out = TRUE

#filter on species in basins? so exclude = TRUE
TedescoSu_filter_species= TRUE #both tedesco and su (also non native and extinct)


#breakpoint manually, check in line 190
bp=6




# output files
folder="TSuspsel_meansp_minbin/" # change the folder name with chosen settings

dir_("Dams_impact/figs")
figdir=dir_("Dams_impact/figs/MVRSbodysize/")
dir_(paste0(figdir,folder))

bothplots = paste0(figdir,folder,"figure1.jpg")



#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

#Hydrobasins lakes l8
# per continent
continent = c('af','ar','as','au','eu','gr','na','sa','si')

# link to hydrobasins level 8 lakes
# hydrobasins of the continent
cat('Reading hb data..\n')

# hydrobasins of the continent
hblakes <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
  poly <- read_sf(paste0(dir_hybas08,'/hybas_lake_',cont,'_lev08_v1c.shp')) 
  return(poly)
}
valid <- st_is_valid(hblakes) # which are valid geometries
hblakes[valid==F,] <- st_make_valid(hblakes[valid==F,]) # make valid so no self intersection errors



# load species data
cat('Reading species data..\n')

if(TedescoSu_filter_species==FALSE){ #4,789846 obs, 11,465 species total
  fish <- foreach(i = 1:6,.combine='rbind') %do% {
    poly <- read.csv(paste0(dir_sp08, '/fish_hybas_table_part',i,'.csv'))
    colnames(poly)[1] = "objectid"
    return(poly)
  }
  fish <- fish[fish$presence %in% c(1,2),]  #only keep polygons indicating current presence
}


if(TedescoSu_filter_species==TRUE){ #2,999,373 obs, 8,169 species
  # see 04B_Select_TedescoSu_IUCN.R
  fish <- foreach(cont = c('af','ar','as','au','eu','gr','na','sa','si'),.combine='rbind') %do% {
    d <- readRDS(paste0('Tedesco/comp/Select_TedescoSu_IUCNspec_',cont,'.rds')) 
    return(d)
  }
  #this is already filtered on presence
}


# inner join 
fish_hb <- inner_join(fish,hblakes, by = c('hybas_id'='HYBAS_ID'))
names(fish_hb)[names(fish_hb) == 'hybas_id'] <- 'HYBAS_ID'

#check species iucn code
check <- fish_hb %>% dplyr::select(binomial, category) %>% distinct() %>% pull(category) %>% table()
barplot(check)


#-------------------------------------------------------------------------------
# Select species
#-------------------------------------------------------------------------------
traits <- read.csv("Fishdata/species_traits_extra.csv") # species without fishbase name were already filtered out
traits <- traits %>% dplyr::select(-id_no) #remove fishsuit id, clashes with iucn id

length(unique(fish_hb$binomial)) #10610
#filter out species without length data
fish_sel <- traits %>% filter(!is.na(Length))
nrow(fish_sel) #9466
fish_sel$Lengthround=round(fish_sel$Length,0)
table(fish_sel$Lengthround)
ggplot(fish_sel,aes(Lengthround))+
  geom_bar()



if(lentic_only == FALSE){
  fish_sel <- fish_sel %>% filter(lentic_only==0)
  nrow(fish_sel) #removes 1017 species from original
}

if(marine == FALSE){
  fish_sel <- fish_sel %>% filter(Marine==0)
  nrow(fish_sel) #removes 416 species from original
}

if(brackish == FALSE){
  fish_sel <- fish_sel %>% filter(Brackish==0)
  nrow(fish_sel) #removes 938 species from original
}

if(diadromous == FALSE){
  fish_sel <- fish_sel %>% filter(diad=="f")
  nrow(fish_sel)
}

if(vulnerable == FALSE){
  fish_sel <- fish_sel %>% filter(code %in% iucncode_included)
  nrow(fish_sel) #removes 1801 species from original
}

fish_selection <- fish_sel %>% pull(binomial)



#-------------------------------------------------------------------------------
# Select basins
#-------------------------------------------------------------------------------
# no selection
mb_selection <- unique(hblakes$MAIN_BAS)
length(unique(hblakes$MAIN_BAS)) #24163


# coastal basins out
if(coastal_basins_filtered_out == TRUE){
  no_coastal <- hblakes %>% as_tibble() %>% dplyr::select(-geometry) %>% 
    group_by(MAIN_BAS) %>% summarise(coastal=mean(COAST)) %>% 
    filter(coastal ==0) %>% dplyr::pull(MAIN_BAS)
  
  mb_selection <- no_coastal
  length(mb_selection) #14571
}


#-------------------------------------------------------------------------------
# Apply filters
#-------------------------------------------------------------------------------

length(mb_selection)
length(fish_selection)

sp <- fish_hb %>% filter(binomial %in% fish_selection) 
sp <- sp %>% filter(MAIN_BAS %in% mb_selection)
length(unique(sp$binomial))
length(unique(sp$MAIN_BAS))


length(unique(sp$binomial)) #4162
fish_sel <- traits %>% filter(binomial %in% sp$binomial)
fish_sel$Lengthround=round(fish_sel$Length,0)
table(fish_sel$Lengthround) # check the mode
#ggplot(fish_sel,aes(Lengthround))+geom_bar()


mb_size <- hblakes %>% as.data.frame() %>% group_by(MAIN_BAS) %>% summarise(MB_AREA=sum(SUB_AREA)) # size of basins



#-------------------------------------------------------------------------------
# Establish MVA relation
#-------------------------------------------------------------------------------


#### get the total species range within main basins ####

sp_mb <- sp %>% group_by(binomial, MAIN_BAS) %>% summarise(SUM_AREA=sum(SUB_AREA))


#### get one value per species ####
sp1 <- sp_mb %>% group_by(binomial) %>% summarise(range=mean(SUM_AREA))


sp1 <- left_join(sp1, traits %>% select(binomial,Length))

#number observations per species histogram, so in how many basins species occur
cnt=table(sp_mb$binomial)
freq=table(cnt)
see=as.data.frame(freq)
sum(see$Freq[which(as.numeric(as.character(see$cnt))>=100)]) # how many species occur in more than 100 basins
plot(as.numeric(as.character(see$cnt)),see$Freq)
dev.off()





#### calculate value per bin and plot MVA relation ####



# small vs large species
df_small <- sp1 %>% filter(Length<=bp)
df_large <- sp1 %>% filter(Length>=bp)

# bins
#get nice amount of bin levels
width_s=0.05 # CHECK
width_l=0.1 # CHECK

bins_groups_s <- cut_width(log10(df_small$Length),width=width_s)
length(unique(bins_groups_s)) # 16 levels OR when not TedescoSu filter 17
bins_groups_l <- cut_width(log10(df_large$Length),width=width_l) 
length(unique(bins_groups_l)) # 17 levels OR when not TedescoSu filter 18

df_small$bin=cut_width(log10(df_small$Length),width=width_s)
df_small$binmid=round(min(log10(df_small$Length)),1) + (cut_width(log10(df_small$Length),width=width_s,labels=F)-1) * width_s # minimum plus bin step size
df_large$bin=cut_width(log10(df_large$Length),width=width_l)
df_large$binmid=round(min(log10(df_large$Length)),1) + (cut_width(log10(df_large$Length),width=width_l,labels=F)-1) * width_l 

df_bins=rbind(df_small,df_large)
#head(df_bins)
#plot with count per bin
#table(df_small$binmid)
#table(df_large$binmid)
#barplot(table(df_small$binmid), main="Small species, number of observations per bin", xlab="Binmid (log10(length))", las=2, ylab="Count")
#barplot(table(df_large$binmid), main="Large species, number of observations per bin", xlab="Binmid (log10(length))", las=2, ylab="Count")

# Get one value per bin
df_binvalue_s <- df_small %>% group_by(bin) %>% summarise(value=min(range), binmid=min(binmid)) # small species (left side graph)
df_binvalue_l <- df_large %>% group_by(bin) %>% summarise(value=min(range), binmid=min(binmid)) # large species (right side graph)


#create log length otherwise problems ggplot text
df_binvalue_s$logarea=log10(df_binvalue_s$value)
df_binvalue_l$logarea=log10(df_binvalue_l$value)
df_binvalue=rbind(df_binvalue_s,df_binvalue_l)

points=rbind(df_small %>% group_by(bin)%>%slice_min(order_by=range),df_large %>% group_by(bin)%>%slice_min(order_by=range))
write.table(points,paste0(figdir, folder, "points.txt"), row.names = F) # save which species represent the minima in each bin


# get regression lines
# plot regression lines



# this below is for making it 2 seperate labels

lm_eqn1 <- function(df){
  m <- lm(logarea ~ binmid, df);
  b = (coef(m)[2])
  eq <- substitute(italic(y) == a ~p~ b* " "*italic(x),
                   list(a = format(unname(round(coef(m)[1],2)), nsmall = 2),
                        b = format(unname(round(abs(coef(m)[2]),2)), nsmall = 2),
                        p = format(unname(ifelse(b>0,"+","-"))))
                   
  )
  as.character(as.expression(eq));
}

lm_eqn2 <- function(df){
  m <- lm(logarea ~ binmid, df);
  b = (coef(m)[2])
  eq <- substitute(~~italic(r)^2~"="~r2, # the dot symbol may give errors with saving the script
                   list(r2 = format(summary(m)$r.squared, digits = 3))
  )
  as.character(as.expression(eq));
}




#the realfig

# sameplot no significance


Aplot= 
  ggplot(sp1, aes(x=log10(Length), y=log10(range)) ) + 
  geom_bin2d(bins = 30) + #TODO
  scale_fill_continuous(type = "viridis",trans="log10") +
  xlab(expression('Log'[10]*' Body Size (cm)')) + ylab(expression('Log'[10]*' Range Size (km'^2* ')')) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,1,2), minor_breaks=c(0.5,1.5,2.5)) +
  annotate("text", x = 2, y = 0, hjust = 0.3, vjust = 0.8, label = bquote(atop(NA,atop(textstyle(n[~species]~"="~.(nrow(sp1))),textstyle(n[~main~basins]~"="~.(length(unique(sp$MAIN_BAS))))))), size=2.25 ) +
  coord_cartesian( xlim=c(0,2.6), ylim=c(-1,6.5), clip = "off")+
  theme(text = element_text(size = 10))+
  theme(legend.key.size = unit(5, 'mm'), #change legend key size
        legend.key.height = unit(4, 'mm'), #change legend key height
        legend.key.width = unit(4, 'mm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size

Bplot= 
  ggplot(NULL, aes(x=binmid, y=logarea) ) +
  geom_vline(xintercept = df_binvalue_l$binmid[1], linetype="dashed", color="grey20", lwd=0.25) +
  geom_point(data=df_binvalue_s, color="blue",  size=0.35) +
  geom_smooth(data=df_binvalue_s, method=lm, se=TRUE, fill ="lightblue",lwd=0.5) +
  geom_text(x = 0.35, y = -0.4, aes(label = lm_eqn1(df_binvalue_s)), size=2, color="blue", parse = TRUE) +
  geom_text(x = 0.35, y = -0.9, aes(label = lm_eqn2(df_binvalue_s)), size=2, color="blue", parse = TRUE) +
  geom_point(data=df_binvalue_l, color="red", size=0.35) +
  geom_smooth(data=df_binvalue_l, method=lm, se=TRUE,color="red", fill ="#FF7F7F", lwd=0.5) +
  geom_text(x = 2.0, y = -0.4, aes(label = lm_eqn1(df_binvalue_l)), size=2, color="red", parse = TRUE) + # couldnt do it in 1 line so made it 2 seperate
  geom_text(x = 2.0, y = -0.9, aes(label = lm_eqn2(df_binvalue_l)), size=2, color="red", parse = TRUE) +
  xlab(expression('Log'[10]*' Body Size (cm)')) + ylab(expression('Log'[10]*' Range Size (km'^2* ')')) +
  theme_bw() +
  theme(text = element_text(size = 10),legend.position= "none") +
  xlim(0,2.6)+
  ylim(-1,6.5)


# put in A and B letters in figs
library(ggpubr)
Allplot=ggarrange(Aplot,Bplot,labels = c("A ", "B"),
                  font.label = list(size = 12),
                  ncol=2,nrow=1, widths = c(1.25, 1))
ggsave(bothplots,Allplot,
       width = 140,height = 60,dpi = 1000,units = 'mm')





mb_size <- hblakes %>% as.data.frame() %>% group_by(MAIN_BAS) %>% summarise(MB_AREA=sum(SUB_AREA))
ms <- lm(logarea ~ binmid, df_binvalue_s)
ml <- lm(logarea ~ binmid, df_binvalue_l)

#table with info stats
file(paste0(figdir, folder,"stats.txt"))
sink(paste0(figdir, folder,"stats.txt"))
print(paste0("Info on size of the ",length(mb_size %>% filter(MAIN_BAS %in% sp$MAIN_BAS) %>% pull(MB_AREA))," basins included:"))
print(summary(mb_size %>% filter(MAIN_BAS %in% sp$MAIN_BAS) %>% pull(MB_AREA)))
print("")
print("Info on regression line left:")
print(summary(ms))
print("Confidence interval slope:")
print(confint(ms, 'binmid', level=0.95))
print("")
print("Info on regression line right:")
print(summary(ml))
print("Confidence interval slope:")
print(confint(ml, 'binmid', level=0.95))
print("")
print(paste0("Breakpoint= ",bp, " cm"))
sink()
close(file(paste0(figdir, folder,"stats.txt")))
