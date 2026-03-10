### Trying 90/10 percentiles

library(ape)
library(tidyverse)
library(cowplot)
library(nlme)
library(ggeffects)
library(ghibli)


# Read in my data
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
n_distinct(points$species)

# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
n_distinct(points_1$species)
points_1<-points_1 %>% drop_na(intrdcd)
n_distinct(points_1$species)

# Group by species and get summarizing!
summary_df<-points_1 %>% 
  group_by(species) %>% 
  reframe(n=n(),
          precip_maxquant=quantile(precip, 0.975), 
          precip_minquant=quantile(precip, 0.025),
          precip_mean=mean(precip),
          precip_median=median(precip),
          nitro_maxquant=quantile(nitrogen, 0.975),
          nitro_minquant=quantile(nitrogen, 0.025),
          nitro_mean=mean(nitrogen),
          nitro_median=median(nitrogen),
          temp_maxquant=quantile(temp, 0.975),
          temp_minquant=quantile(temp, 0.025),
          temp_mean=mean(temp),
          temp_median=median(temp),
          max_lat=max(Y),
          min_lat=min(Y),
          mean_lat=mean(Y),
          median_lat=median(Y)
  )

# As a shortcut, I'm going to read in our final dataframe for the 95-5 
# percentiles, which we've dropped all the problematic species from,
# and I'm going to use it to drop all the problematic species from this 
# dataset...'cause I don't care to run this through every script again

final_dat<-read_csv("pgls_polydropped_final.csv")

summary_df<-summary_df %>% filter(summary_df$species %in% final_dat$species)

# Bring in phylogenetic tree (polytomy trimmed)
mytree<-read.tree("polytomy_removed.tre")

# drop extra tips from phylogenetic tree

diff <- setdiff(mytree$tip.label, summary_df$species)
pruned_tree <- drop.tip(mytree, diff)

# make rows in data match rows in tree
data_1 <- summary_df[match(pruned_tree$tip.label, summary_df$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant
data_1$temp_range<-data_1$temp_maxquant-data_1$temp_minquant
data_1$precip_range<-data_1$precip_maxquant-data_1$precip_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# Merge our niche dataset with our trait dataset
traits<-read_csv("legume_range_traits.csv")

traits$Phy<-gsub(" ", "_", traits$Phy)
data_1<-left_join(data_1, traits, join_by(species==Phy), multiple="any")

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

precip_range <- gls(log(precip_range) ~ EFN + fixer + woody
                    + uses_num_uses + annual + n + poly(median_lat, 2)+EFN*poly(median_lat, 2)
                    +fixer*poly(median_lat, 2),
                    data=data_1, 
                    correlation=corPagel(1, pruned_tree, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))

### Save as RDS file

write_rds(precip_range, "90-10_analyses/90-10_precip_niche_breadth.rds")
precip_range<-readRDS("90-10_analyses/90-10_precip_niche_breadth.rds")

##################################

### Pull predicted means for EFN

EFN_precip_means<-ggpredict(precip_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_precip_means)


### save df
write.csv(EFN_precip_means, "97.5-2.5_analyses/97.5_2.5_precip_EFN_breadth_means.csv")

###########################

### Pull predicted means for fixers

fix_precip_means<-ggpredict(precip_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_precip_means)

### save df
write.csv(fix_precip_means, "97.5-2.5_analyses/97.5_2.5_precip_fix_breadth_means.csv")

#############################################
# pgls for temp range

temp_range <- gls(temp_range ~ EFN + fixer + woody + uses_num_uses
                  + annual + n + poly(median_lat, 2)+EFN*poly(median_lat, 2)
                  +fixer*poly(median_lat, 2),
                  data=data_1, 
                  correlation=corPagel(1, pruned_tree, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)

### Save as RDS file

#write_rds(temp_range, "temp_niche_breadth.rds")

################################################################################
### Pull predicted means for EFN

EFN_temp_means<-ggpredict(temp_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_temp_means)

### save df
write.csv(EFN_temp_means, "97.5-2.5_analyses/97.5_2.5_temp_EFN_breadth_means.csv")

###############################################################################
### Pull predicted means for fixers

fix_temp_means<-ggpredict(temp_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_temp_means)

### save df
write.csv(fix_temp_means, "97.5-2.5_analyses/97.5_2.5_temp_fix_breadth_means.csv")



##########################################################################
#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody + uses_num_uses
                   + annual + n + poly(median_lat, 2)+EFN*poly(median_lat, 2)
                   +fixer*poly(median_lat, 2),
                   
                   data=data_1, 
                   
                   correlation=corPagel(1, pruned_tree, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))
###############################################################################
### Pull predicted means for EFN

EFN_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_nitro_means)

### save df
write.csv(EFN_nitro_means, "97.5-2.5_analyses/97.5_2.5_nitro_EFN_breadth_means.csv")

###############################################################################
### Pull predicted means for fixers

fix_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_nitro_means)

### save df
write.csv(fix_nitro_means, "97.5-2.5_analyses/97.5_2.5_nitro_fix_breadth_means.csv")

###############################################################################

# EFN temp
EFN_temp <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=temp_range, color=EFN), alpha=0.4)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temp. \n range (\u00B0C)")+
  xlab("median latitude")+
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
  xlab("median latitude")+
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
  xlab("median latitude")+
  theme(legend.position="none", axis.title.x = element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=17))+
  annotate("text", label = "**", x=65, y=1500, size = 8)+
  geom_line(data=EFN_nitro_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=EFN_nitro_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                        fill=group, 
                                        alpha=0.4))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)



##############################################################################
###Rhizobia o'clock


# fixer temp
fixer_temp <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=temp_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("Average annual temperature range (Celsius)")+
  xlab("median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=fix_temp_means, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE)+
  geom_ribbon(data=fix_temp_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                       fill=group, 
                                       alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))



# fixer precip
fixer_precip <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=precip_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  annotate("text", label = "***", x=65, y=4500, size = 8)+
  geom_line(data=fix_precip_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=fix_precip_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                         fill=group, 
                                         alpha=0.4))+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))

# fixer nitro
fixer_nitro <- ggplot()+
  geom_point(data=data_1, aes(x=median_lat, y=nitro_range, color=fixer), alpha=0.4)+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 11),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))+
  geom_line(data=fix_nitro_means, aes(x=x, y=predicted, group = group, colour=group))+
  geom_ribbon(data=fix_nitro_means, aes(x=x, ymax=conf.high, ymin=conf.low, group=group,
                                        fill=group, 
                                        alpha=0.4))+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))

### make a compound plot with all the plots we've made!

P<-cowplot::plot_grid(EFN_temp, fixer_temp,
                      EFN_precip, fixer_precip,
                      EFN_nitro, fixer_nitro,
                      ncol=2, nrow=3,
                      labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                      label_size = 14,
                      label_x = c(0.05,  -0.05, 0.05, -0.05, 0.05, -0.05))

P <- add_sub(P, "absolute median latitude", hjust = 0.4, size=17)

plot(P)

#dev.copy2pdf(file="latitude_v_breadth_fig.pdf", width = 7.5, height = 7.5)

jpeg("latitudefig.jpg", width=900, height=900)

plot(P)

dev.off()


