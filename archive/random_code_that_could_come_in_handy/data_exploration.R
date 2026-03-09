### data exploration

library(ggplot2)
library(tidyverse)
library(cowplot)
library(ghibli)

dat<-read.csv("pgls_polydropped_final.csv", stringsAsFactors = TRUE)

## Make sure the categorical variables are being read as categorical by R
dat$Domatia<-as.factor(dat$Domatia)
dat$EFN<-as.factor(dat$EFN)
dat$fixer<-as.factor(dat$fixer)
dat$uses_num_uses<-as.integer(dat$uses_num_uses)

###############################################################################
## Plot niche breadth vs. number of human uses
temp_human <- ggplot()+
  geom_point(data=dat, aes(x=uses_num_uses, y=temp_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
 # scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

precip_human <- ggplot()+
  geom_point(data=dat, aes(x=uses_num_uses, y=precip_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("annual precip. \n range (mm)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

nitro_human <- ggplot()+
  geom_point(data=dat, aes(x=uses_num_uses, y=nitro_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("average nitrogen \n range (cg/kg)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))


### cowplot em together
cowplot::plot_grid(temp_human, precip_human, nitro_human, nrow=1, ncol=3)

################################################################################
## Plot niche breadth vs. number of human uses
temp_annual <- ggplot()+
  geom_point(data=dat, aes(x=annual, y=temp_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  # scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("pr(annual)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

precip_annual <- ggplot()+
  geom_point(data=dat, aes(x=annual, y=precip_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("annual precip. \n range (mm)")+
  xlab("pr(annual)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

nitro_annual <- ggplot()+
  geom_point(data=dat, aes(x=annual, y=nitro_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("average nitrogen \n range (cg/kg)")+
  xlab("pr(annual)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))


### cowplot em together
cowplot::plot_grid(temp_annual, precip_annual, nitro_annual, nrow=1, ncol=3)

################################################################################
# woody
temp_woody <- ggplot()+
  geom_point(data=dat, aes(x=woody, y=temp_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  # scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("pr(woody)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

precip_woody <- ggplot()+
  geom_point(data=dat, aes(x=woody, y=precip_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("annual precip. \n range (mm)")+
  xlab("pr(woody)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

nitro_woody <- ggplot()+
  geom_point(data=dat, aes(x=woody, y=nitro_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("average nitrogen \n range (cg/kg)")+
  xlab("pr(woody)")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))


### cowplot em together
cowplot::plot_grid(temp_woody, precip_woody, nitro_woody, nrow=1, ncol=3)

################################################################################

# number of occurrences
temp_n <- ggplot()+
  geom_point(data=dat, aes(x=n, y=temp_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  # scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("n")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

precip_n <- ggplot()+
  geom_point(data=dat, aes(x=n, y=precip_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("annual precip. \n range (mm)")+
  xlab("n")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))

nitro_n <- ggplot()+
  geom_point(data=dat, aes(x=n, y=nitro_range), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  #scale_colour_manual(values=c("#92BBD9FF"))+
  ylab("average nitrogen \n range (cg/kg)")+
  xlab("n")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))


### cowplot em together
cowplot::plot_grid(temp_n, precip_n, nitro_n, nrow=1, ncol=3)
