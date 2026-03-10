### PGLS without n
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(ggplot2)
library(cowplot)
#install.packages("ggeffects")
library(ggeffects)

# Removing these weird species with 0-little niche breadth at all, and writing 
# it into a new file that I can read in at any time (very exciting stuff):
#data_2<-data_1 %>% filter(n>=25)
#write.csv(data_2, "pgls_final_data.csv")
# we will read this in next

# Read back in PGLS dataframe
data<-read.csv("pgls_polydropped_final.csv")
#data$mutualism<-ifelse(data$EFN==1 | data$Domatia==1 | data$fixer==1, "1", "0")


# Bring in tree-- this tree has the polytomy removed,
# so we also need to trim down the dataset to only include
# species in the tree!
mytree<-read.tree("polytomy_removed.tre")


diff <- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, diff)

# make rows in data match rows in tree
data_1 <- data[match(tree_pruned$tip.label,data$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(data_1$temp_range)
hist(log(data_1$temp_range))


# precip data
hist(data_1$precip_range)
hist(log(data_1$precip_range))


# distribution of nitrogen
hist(data_1$nitro_range)
hist(log(data_1$nitro_range))
min(data_1$nitro_range)



# make sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

######################################
## precip first

# I took n out to try without it!!!! But please know that it should be put back
# in!
precip_range <- gls(log(precip_range) ~ EFN + fixer+woody
                    + uses_num_uses + annual+poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                      fixer*poly(median_lat, 2),
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))


##################################

### Pull predicted means for EFN

EFN_precip_means<-ggpredict(precip_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_precip_means)


###########################

### Pull predicted means for fixers

fix_precip_means<-ggpredict(precip_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_precip_means)


#############################################
# pgls for temp range

temp_range <- gls(temp_range ~ EFN + fixer + woody + uses_num_uses
                  + annual+ poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                    fixer*poly(median_lat, 2),
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)

### Pull predicted means for EFN

EFN_temp_means<-ggpredict(temp_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_temp_means)

################################################################################
### Pull predicted means for fixers

fix_temp_means<-ggpredict(temp_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_temp_means)


##########################################################################
#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + fixer+woody + uses_num_uses
                   + annual +poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                     fixer*poly(median_lat, 2),
                   
                   data=data_1, 
                   
                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))

### Pull predicted means for EFN

EFN_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_nitro_means)


### Pull predicted means for fixers

fix_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_nitro_means)

###############################################################################
# EFN temp
EFN_temp <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=temp_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  
  geom_line(data=EFN_temp_means, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  
  geom_ribbon(data=EFN_temp_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  annotate("text", label = "**", x=65, y=20, size = 8)


# EFN precip
EFN_precip <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=precip_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  geom_line(data=EFN_precip_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=EFN_precip_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                   fill=group, 
                                   alpha=0.4))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  annotate("text", label = "***", x=65, y=4500, size = 8)


# EFN nitro
EFN_nitro <- ggplot(data=data_1)+
  geom_point(aes(x=median_lat, y=nitro_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none", axis.title.x = element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  annotate("text", label = "***", x=65, y=1500, size = 8)+
  geom_line(data=EFN_nitro_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=EFN_nitro_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                  fill=group, 
                                  alpha=0.4))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)




### Rhizobia o'clock

# fixer temp
fixer_temp <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=temp_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=fix_temp_means, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  geom_ribbon(data=fix_temp_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                 fill=group, 
                                 alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))


# fixer precip
fixer_precip <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=precip_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  annotate("text", label = "***", x=65, y=4500, size = 8)+
  geom_line(data=fix_precip_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=fix_precip_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                   fill=group, 
                                   alpha=0.4))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))

# fixer nitro
fixer_nitro <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=nitro_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=fix_nitro_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=fix_nitro_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                  fill=group, 
                                  alpha=0.4))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))

### make a compound plot with all the plots we've made!

P<-cowplot::plot_grid(EFN_temp, fixer_temp,
                      EFN_precip, fixer_precip,
                      EFN_nitro, fixer_nitro,
                      ncol=2, nrow=3,
                      labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                      label_size = 14,
                      label_x = c(0.05, -0.05, 0.05, -0.05, 0.05, -0.05))

P <- add_sub(P, "absolute median latitude", hjust = 0.4, size=17)

plot(P)