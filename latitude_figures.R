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
  geom_point(aes(x=abs(median_lat), y=temp_range, color=EFN), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temperature \n range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank(), text = element_text(size = 10), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "**", x=65, y=22.5, size = 8)


# EFN precip
EFN_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=EFN), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "***", x=65, y=4500, size = 8)


# EFN nitro
EFN_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=EFN), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none", axis.title.x = element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "***", x=65, y=1500, size = 8)

### Domatia time

# Domatia temp
domatia_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  #scale_colour_ghibli_d("MarnieMedium2")+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# domatia precip
domatia_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# domatia nitro
domatia_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))



### Rhizobia o'clock

# fixer temp
fixer_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction=1)+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# fixer precip
fixer_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# fixer nitro
fixer_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))

P<-cowplot::plot_grid(EFN_temp, domatia_temp, fixer_temp,
                   EFN_precip, domatia_precip, fixer_precip,
                   EFN_nitro, domatia_nitro, fixer_nitro,
                   labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                   label_size = 11,
                   label_x = c(0, -0.05, -0.05, 0, -0.05, -0.05, 0, -0.05, -0.05))

P <- add_sub(P, "absolute median latitude", hjust = 0.4, size=12)

plot(P)

dev.copy2jpg(file="latitude_v_breadth_fig.pdf", width = 7.5, height = 7.5)

jpeg('latitude.jpg', quality=100, height=1800, width=1961, pointsize=14, res=600)
plot(P)
dev.off()
