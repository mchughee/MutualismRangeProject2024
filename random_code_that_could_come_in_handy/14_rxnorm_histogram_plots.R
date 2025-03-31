# Making figures for results section
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
#install.packages('ghibli')
library(ghibli)
library(reshape2)

# read in data
dat<-read.csv("pgls_polydropped_final.csv")
dat$nitro_range<-dat$nitro_maxquant-dat$nitro_minquant
dat$mutualism<-ifelse(dat$EFN==1 | dat$Domatia==1 | dat$fixer==1, "1", "0")


# Finding basic summary stats
sum(dat$EFN==0)
sum(dat$EFN==1)


sum(dat$Domatia==0)
sum(dat$Domatia==1)

sum(dat$fixer==0)
sum(dat$fixer==1)

mutualism<-filter(dat, EFN==1 | fixer==1 | Domatia==1)
no_mutualism<-filter(dat, EFN==0 & fixer==0 & Domatia==0)

### Add mutualism column to df
dat$mutualism<-ifelse(dat$EFN==1 | dat$fixer==1 | dat$Domatia==1, 1, 0)
dat$mutualism<-as.factor(dat$mutualism)

### find smallest and largest number of occs
summary(dat$n)

### Make sure the categorical variables are being read as categorical by R
### This is SO FREAKING KEY
dat$Domatia<-as.factor(dat$Domatia)
dat$EFN<-as.factor(dat$EFN)
dat$fixer<-as.factor(dat$fixer)


### Making data usable for boxplots and reaction norm plots!

# shorter version of dataframe with just what we need
data_short<-dat %>% dplyr::select(species, precip_maxquant,
                                 precip_minquant, temp_maxquant,
                                 temp_minquant, nitro_maxquant,
                                 nitro_minquant,
                                 EFN, Domatia,
                                 fixer, mutualism)

# use melt to make the dataframe longer (this way, can graph with
# multiple measures on same axes)
data_melt<-reshape2::melt(data_short, id.vars=c("species", "EFN", "Domatia", "fixer", "mutualism"),
                           measure.vars=c("precip_maxquant",
                                          "precip_minquant", "temp_maxquant",
                                          "temp_minquant", "nitro_maxquant",
                                          "nitro_minquant"))

data_melt$EFN<-as.factor(data_melt$EFN)
data_melt$Domatia<-as.factor(data_melt$Domatia)
data_melt$fixer<-as.factor(data_melt$fixer)
data_melt$variable<-as.factor(data_melt$variable)

#### Making reaction norms for max/min precipitation

EFN_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=EFN, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual temp. \n (Celsius)")+
  scale_x_discrete(labels=c("no EFN", "EFN"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=13), 
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))
  


EFN_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=EFN, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Annual precip. \n (mm)")+
  scale_x_discrete(labels=c("no EFN", "EFN"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("Max. niche value", "Min. niche value"))+
  #theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=13), 
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))+
  labs(colour = "")

EFN_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=EFN, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no EFN", "EFN"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none", axis.title=element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))
  


# Fixer

fixer_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=fixer, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("no rhizobia", "rhizobia"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=13), axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))



fixer_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=fixer, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Annual precipitation (mm)")+
  scale_x_discrete(labels=c("no rhizobia", "rhizobia"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("not present", "present"))+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("Max. niche value", "Min. niche value"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=13), axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))+
 # guides(fill=guide_legend(title="Mutualism"))
 # theme(legend.position="none")+
  labs(colour = "")

fixer_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=fixer, y=average, colour=variable, group=variable)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no rhizobia", "rhizobia"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none", axis.title=element_text(size=13), 
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))
  #annotate("text", label = "NS/NS", x=2, y=300, size = 7)
  

plot<-cowplot::plot_grid(EFN_precip, fixer_precip,
                   EFN_temp, fixer_temp,
                   EFN_nitro, fixer_nitro, nrow=3, ncol=2,
                   labels = c('A', 'D', 'B', 'E', 'C', 'F'),
                   label_size = 12,
                   label_x = c(0.03, -0.05,  0.05, -0.05,  0.03, -0.05))
plot

jpeg("maxmin.jpeg", width=700, height=600)

plot

dev.off()

################################################################################  

### Plot all three mutualisms by latitude on one histogram

# make column with EFN, EFN+rhizobia, just rhizobia, and none
dat$traits<-ifelse(dat$EFN=="0" & dat$fixer=="0", "None", 
                   ifelse(dat$EFN=="1" & dat$fixer=="0", "EFN only",
                          ifelse(dat$EFN=="0" & dat$fixer=="1", "Rhizobia only",
                                 "Both")))

dat1<-dat %>% mutate(traits=fct_relevel(traits, c("Both","EFN only","Rhizobia only",
                                                     "None"))) %>% 
  arrange(traits)

complex_histo<-ggplot(data=dat1, aes(x=median_lat, fill=traits))+
  geom_histogram()+
  theme_classic()+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  xlab("median latitude")+
  theme(axis.title.x=element_text(size=13), axis.title.y=element_text(size=13),
        title.text.y = element_text(size=13), axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  theme(legend.text=element_text(size=13), legend.title=element_text(size=13))+
  guides(fill=guide_legend("Mutualism types"))


###############################################################################
# latitude figure for max and min plots




 





