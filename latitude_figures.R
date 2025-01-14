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


## Make sure the categorical variables are being read as categorical by R
dat$Domatia<-as.factor(dat$Domatia)
dat$EFN<-as.factor(dat$EFN)
dat$fixer<-as.factor(dat$fixer)

# EFN temp
EFN_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=EFN))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temperature \n range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())


# EFN precip
EFN_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=EFN))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())


# EFN nitro
EFN_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none", axis.title.x = element_blank())

### Domatia time

# Domatia temp
domatia_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=Domatia))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#4D6D93FF", "#92BBD9FF"))+
  #scale_colour_ghibli_d("MarnieMedium2")+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


# domatia precip
domatia_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=Domatia))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#4D6D93FF", "#92BBD9FF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


# domatia nitro
domatia_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=Domatia))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#4D6D93FF", "#92BBD9FF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())



### Rhizobia o'clock

# fixer temp
fixer_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=fixer))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction=1)+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


# fixer precip
fixer_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=fixer))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


# fixer nitro
fixer_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=fixer))+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())

P<-cowplot::plot_grid(EFN_temp, domatia_temp, fixer_temp,
                   EFN_precip, domatia_precip, fixer_precip,
                   EFN_nitro, domatia_nitro, fixer_nitro)

P <- add_sub(P, "absolute median latitude", hjust = 0.5)

plot(P)


