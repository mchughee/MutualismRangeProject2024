# PGLS for max and min

# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(ggplot2)
library(ggeffects)
library(cowplot)

# Read back in PGLS dataframe
data<-read.csv("pgls_polydropped_final.csv")


# Bring in tree
mytree<-read.tree("polytomy_removed.tre")

# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, dropped_species)

# here, we'll trim down
data1<-filter(data, data$species %in% tree_pruned$tip.label)
# don't worry about the fact that it's 133 species dropped, not 139
# when dropping species without occurrences, I think we ended up dropping
# some species in the polytomy

# make rows in data match rows in tree
data_1 <- data1[match(tree_pruned$tip.label,data1$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# make sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

# Add in hemisphere data
data_1$hemisphere<-ifelse(data_1$median_lat>0, "0", "1")

sum(data_1$hemisphere=="0")
sum(data_1$hemisphere=="1")

data_1$hemisphere<-as.factor(data_1$hemisphere)

###############################################################################
### Running PGLS on maxquant data

# Precipitation
hist(data_1$precip_maxquant)
hist(log(data_1$precip_maxquant))

precip_maxquant <- gls(log(precip_maxquant) ~ EFN*abs_med_lat*hemisphere+
                         fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_maxquant)

plot(precip_maxquant)

hist(residuals(precip_maxquant))

qqnorm(precip_maxquant, abline = c(0,1))

# save rds file

write_rds(precip_maxquant, "precip_maxquant.rds")
precip_maxquant<-read_rds("precip_maxquant.rds")
summary(precip_maxquant)

precip_max<-data.frame(coef(summary(precip_maxquant))) %>% format(scientific=F)
precip_max$p.value<-as.numeric(precip_max$p.value) %>% round(4)
write.csv(precip_max, "precip_max_output_table.csv")

### Extract predicted values
EFN_precip_max_means<-ggpredict(precip_maxquant, 
                                terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_precip_max_means)

fixer_precip_max_means<-ggpredict(precip_maxquant, terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_precip_max_means)

## Plot values!

p1 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_maxquant, 
                                           shape= hemisphere, colour=EFN),
                                           alpha=0.1)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("maximum annual \n precipitation (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_precip_max_means, aes(x=x, y=predicted, linetype = facet, 
                                           colour=group), show.legend = FALSE, linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")

save_plot("precip_max_efn.pdf", p1)



p2 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_maxquant, shape= hemisphere, color=fixer),
                          alpha=0.2)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("maximum annual \n precipitation (mm))")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_precip_max_means, aes(x=x, y=predicted, linetype = facet, 
                                             colour=group), linewidth=1.3)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none")


save_plot("precip_max_fixer.pdf", p2)

p3 <- plot_grid(p1, p2, nrow=2)
save_plot("precip_max_efn_fixer.pdf", p3, base_height=8, base_width=6)
p3

#############################
# pgls for temp maxquant
data_1$scale_tempmax<-scale(data_1$temp_maxquant, scale=TRUE)
hist(data_1$temp_maxquant)
hist(log(data_1$temp_maxquant))
# pov when you do the transformation and it makes the data look WORSE

temp_maxquant <- gls(temp_maxquant ~ EFN*abs_med_lat*hemisphere+
                       fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                     data=data_1, 
                     correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_maxquant)

hist(residuals(temp_maxquant))
qqnorm(temp_maxquant, abline = c(0,1))
plot(temp_maxquant)

# Write RDS file
write_rds(temp_maxquant, "temp_maxquant.rds")
temp_maxquant<-read_rds("temp_maxquant.rds")

temp_max<-data.frame(coef(summary(temp_maxquant))) %>% format(scientific=F)
temp_max$p.value<-as.numeric(temp_max$p.value) %>% round(4)
write.csv(temp_max, "temp_max_output_table.csv")

EFN_temp_max_means<-ggpredict(temp_maxquant, 
                                terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_temp_max_means)

fixer_temp_max_means<-ggpredict(temp_maxquant, terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_temp_max_means)

# Plot values!

p4 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_maxquant, 
                                           shape= hemisphere, colour=EFN
                                           ), alpha=0.1)+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("maximum average \n annual temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_temp_max_means, aes(x=x, y=predicted, linetype = facet, 
                                           colour=group), show.legend = FALSE, linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")

save_plot("temp_max_efn.pdf", p4)



p5 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_maxquant, shape= hemisphere, color=fixer),
                          alpha=0.2)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("maximum average \n annual temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_temp_max_means, aes(x=x, y=predicted, linetype = facet, 
                                             colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none")


save_plot("temp_max_fixer.pdf", p5)

p6 <- plot_grid(p4, p5, nrow=2)
save_plot("temp_max_efn_fixer.pdf", p6, base_height=8, base_width=6)
p6

##########################################
# pgls for nitro maxquant
hist(log(data_1$nitro_maxquant))
hist(data_1$nitro_maxquant)

nitro_maxquant <- gls(log(nitro_maxquant) ~ EFN*abs_med_lat*hemisphere+
                        fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_maxquant)

plot(nitro_maxquant)
hist(residuals(nitro_maxquant))
qqnorm(nitro_maxquant, abline = c(0,1))


# Write RDS file
write_rds(nitro_maxquant, "nitro_maxquant.rds")
nitro_maxquant<-read_rds("nitro_maxquant.rds")

# write into file
nitro_max<-data.frame(coef(summary(nitro_maxquant))) %>% format(scientific=F)
nitro_max$p.value<-as.numeric(nitro_max$p.value) %>% round(4)
write.csv(nitro_max, "nitro_max_output_table.csv")

EFN_nitro_max_means<-ggpredict(nitro_maxquant, 
                                terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_nitro_max_means)

fixer_nitro_max_means<-ggpredict(nitro_maxquant, terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_nitro_max_means)

# Plot values

p7 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_maxquant, 
                                            colour=EFN),
                                           alpha=0.1)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("maximum nitrogen \n(cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_nitro_max_means, aes(x=x, y=predicted, linetype = facet, 
                                         colour=group), linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  guides(linetype=guide_legend("hemisphere"))+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  guides(linetype="none")
  

save_plot("nitro_max_efn.pdf", p7)



p8 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_maxquant, color=fixer),
                          alpha=0.1)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("maximum nitrogen \n(cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_nitro_max_means, aes(x=x, y=predicted, linetype = facet, 
                                           colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+ 
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  guides(linetype=guide_legend("hemisphere"))




save_plot("nitro_max_fixer.pdf", p8)

p9 <- plot_grid(p7, p8, nrow=2)
save_plot("nitro_max_efn_fixer.pdf", p9, base_height=8, base_width=6)
p9

# cowplot all our maximum plots together

efn_leg_max<-get_legend(p7)
fixer_leg_max<-get_legend(p8)
combine_leg<-plot_grid(efn_leg_max, fixer_leg_max, ncol=1, nrow=2)

maximum_plots<-cowplot::plot_grid(p1+theme(axis.title.x = element_blank()), p2+theme(axis.title.y = element_blank(), axis.title.x = element_blank()), combine_leg,
                                  p4+theme(axis.title.x = element_blank()), p5+theme(axis.title.y = element_blank(), axis.title.x = element_blank()), NA,
                                  p7+ theme(legend.position="none"), p8+ theme(legend.position="none", axis.title.y = element_blank()), NA,
                                  ncol=3, nrow=3)

################################################################################
################################################################################
### Running PGLS on minquant data

# First check that the residuals do not, in fact, have equal variance

hist(log(data_1$precip_minquant))
hist(data_1$precip_minquant)

precip_minquant <- gls(log(precip_minquant) ~ EFN*abs_med_lat*hemisphere+
                         fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_minquant)

plot(precip_minquant)
hist(residuals(precip_minquant))
qqnorm(precip_minquant, abline = c(0,1))

# write RDS

write_rds(precip_minquant, "precip_minquant.rds")
precip_minquant<-read_rds("precip_minquant.rds")

# output table
precip_min<-data.frame(coef(summary(precip_minquant))) %>% format(scientific=F)
precip_min$p.value<-as.numeric(precip_min$p.value) %>% round(4)
write.csv(precip_min, "precip_min_output_table.csv")

# grab model output
EFN_precip_min_means<-ggpredict(precip_minquant, 
                               terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_precip_min_means)

fixer_precip_min_means<-ggpredict(precip_minquant, 
                                 terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_precip_min_means)

# Plot values

p10 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_minquant, 
                                            colour=EFN
                                           ), alpha=0.1)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("minimum annual \n precipitation (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_precip_min_means, aes(x=x, y=predicted, linetype = facet, 
                                          colour=group), linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  guides(linetype=guide_legend("hemisphere"))+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  theme(legend.position="none")

save_plot("precip_min_efn.pdf", p10)



p11 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_minquant, 
                                            shape= hemisphere, color=fixer), alpha=0.1
                                            )+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("minimum annual \n precipitation (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_precip_min_means, aes(x=x, y=predicted, linetype = facet, 
                                            colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  guides(linetype=guide_legend("hemisphere"))+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  theme(legend.position="none")


save_plot("precip_min_fixer.pdf", p11)

p12 <- plot_grid(p10, p11, nrow=2)
save_plot("precip_min_efn_fixer.pdf", p12, base_height=8, base_width=6)
p12

###############################################
# pgls for temp minquant

temp_minquant <- gls(temp_minquant ~ EFN*abs_med_lat*hemisphere+
                       fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                     data=data_1, 
                     correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_minquant)

hist(residuals(temp_minquant))
qqnorm(temp_minquant, abline = c(0,1))
plot(temp_minquant)

# write RDS

write_rds(temp_minquant, "temp_minquant.rds")
temp_minquant<-read_rds("temp_minquant.rds")

# output table
temp_min<-data.frame(coef(summary(temp_minquant))) %>% format(scientific=F)
temp_min$p.value<-as.numeric(temp_min$p.value) %>% round(4)
write.csv(temp_min, "temp_min_output_table.csv")

# pull model output
EFN_temp_min_means<-ggpredict(temp_minquant, 
                                terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_temp_min_means)

fixer_temp_min_means<-ggpredict(temp_minquant, 
                                  terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_temp_min_means)

# Plot values

p13 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_minquant, 
                                            shape= hemisphere, colour=EFN),
                                            alpha=0.1)+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("minimum average annual \n temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_temp_min_means, aes(x=x, y=predicted, linetype = facet, 
                                           colour=group), show.legend = FALSE, linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")

save_plot("temp_min_efn.pdf", p13)



p14 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_minquant, 
                                            shape= hemisphere, color=fixer),
                           alpha=0.1)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"))+
  ylab("minimum average annual \n temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_temp_min_means, aes(x=x, y=predicted, linetype = facet, 
                                             colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none")


save_plot("temp_min_fixer.pdf", p14)

p15 <- plot_grid(p13, p14, nrow=2)
save_plot("temp_min_efn_fixer.pdf", p15, base_height=8, base_width=6)
p15

####################################################
# pgls for nitro range
hist(data_1$nitro_minquant)
hist(log(data_1$nitro_minquant))
hist(sqrt(data_1$nitro_minquant))

nitro_minquant <- gls(log(nitro_minquant) ~ EFN*abs_med_lat*hemisphere+
                        fixer*abs_med_lat*hemisphere+woody+uses_num_uses+annual,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_minquant)

plot(nitro_minquant)
hist(residuals(nitro_minquant))
qqnorm(nitro_minquant, abline = c(0,1))

# write RDS

write_rds(nitro_minquant, "nitro_minquant.rds")
nitro_minquant<-read_rds("nitro_minquant.rds")

# output table
nitro_min<-data.frame(coef(summary(nitro_minquant))) %>% format(scientific=F)
nitro_min$p.value<-as.numeric(nitro_min$p.value) %>% round(4)
write.csv(nitro_min, "nitro_min_output_table.csv")

# Pull model output
EFN_nitro_min_means<-ggpredict(nitro_minquant, 
                              terms=c("abs_med_lat [all]", "EFN [all]", "hemisphere"), type="fixed")
plot(EFN_nitro_min_means)

fixer_nitro_min_means<-ggpredict(nitro_minquant, 
                                terms=c("abs_med_lat [all]", "fixer [all]", "hemisphere"), type="fixed")
plot(fixer_nitro_min_means)

# Plot values

p16 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_minquant, 
                                            colour=EFN
                                            ), alpha=0.1)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("minimum nitrogen \n (cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_nitro_min_means, aes(x=x, y=predicted, linetype = facet, 
                                         colour=group), linewidth=1.3)+
  #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, linetype=facet,
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))

save_plot("nitro_min_efn.pdf", p16)



p17 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_minquant, 
                                            color=fixer),
                           alpha=0.2)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("minimum nitrogen \n (cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_nitro_min_means, aes(x=x, y=predicted, linetype = facet, 
                                           colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #linetype=facet, fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))


save_plot("nitro_min_fixer.pdf", p17)

p18 <- plot_grid(p16, p17, nrow=2)
save_plot("nitro_min_efn_fixer.pdf", p15, base_height=8, base_width=6)
p18

efn_leg_min<-get_legend(p16)
fixer_leg_min<-get_legend(p17)
combine_leg_min<-plot_grid(efn_leg_min, fixer_leg_min, ncol=1, nrow=2)

minimum_plots<-plot_grid(p10+theme(axis.title.x = element_blank()), p11+theme(axis.title.y = element_blank(), axis.title.x = element_blank()), combine_leg,
                         p13+theme(axis.title.x = element_blank()), p14+theme(axis.title.y = element_blank(), axis.title.x = element_blank()), NA,
                         p16+ theme(legend.position="none"), 
                         p17+ theme(legend.position="none", axis.title.y = element_blank()), NA,
                         ncol=3, nrow=3)