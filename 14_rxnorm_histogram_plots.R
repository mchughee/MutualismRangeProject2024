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
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual temperature \n (Celsius)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", y=22.5, x=2, size = 7, label = "NS/*")
  


EFN_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Annual precipitation \n (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  #theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", x=2, y=1900, label = "*/NS", size = 7)

EFN_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none", axis.title=element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", x=2, y=350, label = "***/NS", size = 7)

# Domatia

Domatia_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=26, size = 7)



Domatia_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Annual precipitation (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  #theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=3000, size = 7)

Domatia_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none", axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=350, size = 7)




# Fixer

fixer_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", label = "*/NS", x=2, y=24, size = 7)



fixer_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Annual precipitation (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("not present", "present"))+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
 # guides(fill=guide_legend(title="Mutualism"))
 # theme(legend.position="none")+
  labs(colour = "Fixer")+
  annotate("text", label = "***/*", x=2, y=2000, size = 7)

fixer_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none", axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=300, size = 7)
  

plot<-cowplot::plot_grid(EFN_precip, Domatia_precip, fixer_precip,
                   EFN_temp, Domatia_temp, fixer_temp,
                   EFN_nitro, Domatia_nitro, fixer_nitro, nrow=3, ncol=3,
                   labels = c('A', 'D', 'H', 'B', 'E', 'I', 'C', 'F', 'J'),
                   label_size = 14,
                   label_x = c(0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05))
plot

jpeg("maxmin.jpeg", width=700, height=600)

plot

dev.off()



### absolute median latitude plot

dat$abs_med_lat<-abs(dat$median_lat)

latitude_EFN<-ggplot(data=dat, aes(x=abs_med_lat, fill=EFN))+
  geom_histogram()+
  theme_classic()+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_text(size=18),
        title.text.y = element_text(size=15), axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
  guides(fill=guide_legend("EFN"))


latitude_domatia<-ggplot(data=dat, aes(x=abs_med_lat, fill=Domatia))+
  geom_histogram()+
  theme_classic()+
  scale_fill_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        title.text.y = element_text(size=15))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15),
        axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  guides(fill=guide_legend("Domatia"))

latitude_fixer<-ggplot(data=dat, aes(x=abs_med_lat, fill=fixer))+
  geom_histogram()+
  theme_classic()+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
        title.text.y = element_text(size=15), axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
  guides(fill=guide_legend("Rhizobia"))


p<-cowplot::plot_grid(latitude_EFN, latitude_domatia, latitude_fixer, nrow=1, ncol=3,
                      labels = c('A', 'B', 'C'),
                      label_x = c(0, -0.05, -0.05),
                      label_size = 18)

p <- add_sub(p, "absolute median latitude", hjust = 0.4, size=18)

plot(p) 
  



 





