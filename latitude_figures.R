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


## Make sure the categorical variables are being read as categorical by R
dat$Domatia<-as.factor(dat$Domatia)
dat$EFN<-as.factor(dat$EFN)
dat$fixer<-as.factor(dat$fixer)


# read in model output
temp_efn<-read.csv("model_fits/temp_EFN_breadth_means.csv")
temp_efn$group<-as.factor(temp_efn$group)
# EFN temp
EFN_temp <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=temp_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+

  geom_line(data=temp_efn, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  
  geom_ribbon(data=temp_efn, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
annotate("text", label = "**", x=65, y=20, size = 8)
                              

precip_efn<-read.csv("model_fits/precip_EFN_breadth_means.csv")
precip_efn$group<-as.factor(precip_efn$group)
# EFN precip
EFN_precip <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=precip_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  geom_line(data=precip_efn, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=precip_efn, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
annotate("text", label = "***", x=65, y=4500, size = 8)


nitro_efn<-read.csv("model_fits/nitro_EFN_breadth_means.csv")
nitro_efn$group<-as.factor(nitro_efn$group)
# EFN nitro
EFN_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none", axis.title.x = element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  annotate("text", label = "***", x=65, y=1500, size = 8)+
  geom_line(data=nitro_efn, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=nitro_efn, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                   fill=group, 
                                   alpha=0.4))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)


### Domatia time

temp_dom<-read.csv("model_fits/temp_dom_breadth_means.csv")
temp_dom$group<-as.factor(temp_dom$group)
# Domatia temp
domatia_temp <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=temp_range, color=Domatia), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  #scale_colour_ghibli_d("MarnieMedium2")+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=temp_dom, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  geom_ribbon(data=temp_dom, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                  fill=group, 
                                  alpha=0.4), show.legend = FALSE)+
  scale_fill_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylim(-10, 100)

precip_dom<-read.csv("model_fits/precip_dom_breadth_means.csv")
precip_dom$group<-as.factor(precip_dom$group)
# domatia precip
domatia_precip <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=precip_range, color=Domatia), alpha=0.4)+
 # geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 12),
        axis.text.x = element_text(size=12))+
  geom_line(data=precip_dom, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=precip_dom, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4))+
  scale_fill_manual(values=c("#26432FFF", "#92BBD9FF"))

nitro_dom<-read.csv("model_fits/nitro_dom_breadth_means.csv")
nitro_dom$group<-as.factor(nitro_dom$group)
# domatia nitro
domatia_nitro <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=nitro_range, color=Domatia), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=nitro_dom, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=nitro_dom, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                   fill=group, 
                                   alpha=0.4))+
  scale_fill_manual(values=c("#26432FFF", "#92BBD9FF"))



### Rhizobia o'clock

temp_fix<-read.csv("model_fits/temp_fix_breadth_means.csv")
temp_fix$group<-as.factor(temp_fix$group)

# fixer temp
fixer_temp <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=temp_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=temp_fix, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  geom_ribbon(data=temp_fix, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                  fill=group, 
                                  alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))


precip_fix<-read.csv("model_fits/precip_fix_breadth_means.csv")
precip_fix$group<-as.factor(precip_fix$group)
# fixer precip
fixer_precip <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=precip_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  annotate("text", label = "***", x=65, y=4500, size = 8)+
  geom_line(data=precip_fix, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=precip_fix, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4))+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))

nitro_fix<-read.csv("model_fits/nitro_fix_breadth_means.csv")
nitro_fix$group<-as.factor(nitro_fix$group)
# fixer nitro
fixer_nitro <- ggplot()+
  geom_point(data=dat, aes(x=abs(median_lat), y=nitro_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=nitro_fix, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=nitro_fix, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                   fill=group, 
                                   alpha=0.4))+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))

### make a compound plot with all the plots we've made!

P<-cowplot::plot_grid(EFN_temp, domatia_temp, fixer_temp,
                   EFN_precip, domatia_precip, fixer_precip,
                   EFN_nitro, domatia_nitro, fixer_nitro,
                   labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                   label_size = 14,
                   label_x = c(0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05))

P <- add_sub(P, "absolute median latitude", hjust = 0.4, size=17)

plot(P)

#dev.copy2pdf(file="latitude_v_breadth_fig.pdf", width = 7.5, height = 7.5)

jpeg("latitudefig.jpg", width=900, height=900)

plot(P)

dev.off()
