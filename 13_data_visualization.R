# Making figures for results section
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
#install.packages('ghibli')
library(ghibli)
library(reshape2)

# read in data
dat<-read.csv("pgls_final_data.csv")
dat$nitro_range<-dat$nitro_maxquant-dat$nitro_minquant
dat$mutualism<-ifelse(dat$EFN==1 | dat$Domatia==1 | dat$fixer==1, "1", "0")

# make list of factors we want to loop over to make plots-- make a plot for each
# mutualism type

factor_list<-c("EFN", "Domatia", "fixer")

# popsicle plot for occurrence number vs species

mutualism_plot<-dat %>%
mutate(species = fct_reorder(species, rank(mutualism))) %>%
ggplot() +
aes(x=species, y=n, colour=mutualism) +
geom_point()+
geom_segment(aes(x=species, y=0, yend=n))+
scale_x_discrete()+
theme(axis.text.x=element_blank())+
ylab("number of occurrences after thinning and cleaning")+
scale_colour_manual(values=c("#39568CFF", "#FDE725FF"), labels=c("no mutualism", "mutualism"))+
labs(color="mutualism")
ggsave(plot=last_plot(), filename = str_c("data_viz/","occs_per_species.pdf"), width = 14, height = 6)

# Let's make some boxplots

dat$EFN<-as.factor(dat$EFN)

dat$Domatia<-as.factor(dat$Domatia)

dat$fixer<-as.factor(dat$fixer)

EFN<-ggplot(dat, aes(x=EFN, y=precip_range, fill=EFN))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Extrafloral nectaries")+
ylab("Precipitation breadth \n (mm/year)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())


Domatia<-ggplot(dat, aes(x=Domatia, y=precip_range, fill=Domatia))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Domatia")+
ylab("Precipitation breadth \n (mm/year)")+
scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())+
theme(axis.title.y=element_blank())


fixer<-ggplot(dat, aes(x=fixer, y=precip_range, fill=fixer))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Nitrogen-fixing bacteria")+
ylab("Precipitation breadth \n (mm/year)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())+
theme(axis.title.y=element_blank())

# the same thing but for temperature niche
EFN_temp<-ggplot(dat, aes(x=EFN, y=temp_range, fill=EFN))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Extrafloral nectaries")+
ylab("Temperature breadth \n (degrees)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())

Domatia_temp<-ggplot(dat, aes(x=Domatia, y=temp_range, fill=Domatia))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Domatia")+
ylab("Temperature breadth \n (degrees)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())+
theme(axis.title.y=element_blank())


fixer_temp<-ggplot(dat, aes(x=fixer, y=temp_range, fill=fixer))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Nitrogen-fixing bacteria")+
ylab("Temperature breadth \n (degrees)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.x=element_blank())+
theme(axis.title.y=element_blank())

# For nitrogen

EFN_nitro<-ggplot(dat, aes(x=EFN, y=nitro_range, fill=EFN))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Extrafloral nectaries")+
ylab("Nitrogen breadth \n (cg/kg)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)


Domatia_nitro<-ggplot(dat, aes(x=Domatia, y=nitro_range, fill=Domatia))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Domatia")+
ylab("Nitrogen breadth \n (cg/kg)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.y=element_blank())

fixer_nitro<-ggplot(dat, aes(x=fixer, y=nitro_range, fill=fixer))+
geom_boxplot(show.legend = FALSE)+
theme_classic()+
xlab("Nitrogen-fixing bacteria")+
ylab("Nitrogen breadth \n (cg/kg)")+
scale_x_discrete(labels=c("no", "yes"))+
scale_fill_ghibli_d("LaputaMedium", direction = -1)+
theme(axis.title.y=element_blank())

cowplot::plot_grid(EFN, Domatia, fixer, 
                   EFN_temp, Domatia_temp, fixer_temp, 
                   EFN_nitro, Domatia_nitro, fixer_nitro,
                   nrow=3, ncol=3, hjust=0.3,
                   labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"))


### Making boxplots for max precipitation

data_short<-dat %>% dplyr::select(species, precip_maxquant,
                                 precip_minquant, temp_maxquant,
                                 temp_minquant, nitro_maxquant,
                                 nitro_minquant,
                                 EFN, Domatia,
                                 fixer, mutualism)

## Calculate averages for each variable
#dat$average_tempmax<-mean(dat$temp_maxquant)
#dat$average_tempmin<-mean(dat$temp_minquant)

#dat$average_precipmax<-mean(dat$precip_maxquant)
#dat$average_precipmin<-mean(dat$precip_minquant)

#dat$average_nitromax<-mean(dat$nitro_maxquant)
#dat$average_nitromin<-mean(dat$nitro_minquant)

## Then use these to convert each species' niche axes into percentages-- that way,
## we can have standardized measures on the y-axes

#dat$temp_max_per<-(dat$temp_maxquant/dat$average_tempmax)*100
#dat$temp_min_per<-(dat$temp_minquant/dat$average_tempmin)*100

#dat$precip_max_per<-(dat$precip_maxquant/dat$average_precipmax)*100
#dat$precip_min_per<-(dat$precip_minquant/dat$average_precipmin)*100

#dat$nitro_max_per<-(dat$nitro_maxquant/dat$average_nitromax)*100
#dat$nitro_min_per<-(dat$nitro_minquant/dat$average_nitromin)*100

  
data_melt<-reshape2::melt(data_short, id.vars=c("species", "EFN", "Domatia", "fixer", "mutualism"),
                           measure.vars=c("precip_maxquant",
                                          "precip_minquant", "temp_maxquant",
                                          "temp_minquant", "nitro_maxquant",
                                          "nitro_minquant"))

data_melt$EFN<-as.factor(data_melt$EFN)
data_melt$Domatia<-as.factor(data_melt$Domatia)
data_melt$fixer<-as.factor(data_melt$fixer)

EFN_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  ggplot()+
  aes(x=EFN, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("EFN")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")


EFN_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  ggplot()+
  aes(x=EFN, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("EFN")+
  ylab("annual precipitation (mm)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")

EFN_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  ggplot()+
  aes(x=EFN, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)
  #theme(legend.position="none")


# Domatia

domatia_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  ggplot()+
  aes(x=Domatia, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Domatia")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")


domatia_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  ggplot()+
  aes(x=Domatia, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Domatia")+
  ylab("annual precipitation (mm)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")

domatia_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  ggplot()+
  aes(x=Domatia, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Domatia")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)
#theme(legend.position="none")



# Fixer

fixer_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  ggplot()+
  aes(x=fixer, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Fixer")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")


fixer_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  ggplot()+
  aes(x=fixer, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Fixer")+
  ylab("annual precipitation (mm)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  theme(legend.position="none")

fixer_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  ggplot()+
  aes(x=fixer, y=value, fill=variable)+
  geom_boxplot()+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_ghibli_d("LaputaMedium", direction = -1)
#theme(legend.position="none")
  

cowplot::plot_grid(EFN_precip, EFN_temp, EFN_nitro,
                   domatia_precip, domatia_temp, domatia_nitro,
                   fixer_precip, fixer_temp, fixer_nitro, nrow=3, ncol=3)



  





