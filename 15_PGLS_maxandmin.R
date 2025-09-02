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
data<-read.csv("data_files/pgls_polydropped_final_biome.csv")

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
data_1$biome<-as.factor(data_1$biome)

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

precip_maxquant <- gls(log(precip_maxquant) ~ EFN*abs_med_lat+
                         fixer*abs_med_lat+woody+uses_num_uses+annual,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_maxquant)

plot(precip_maxquant)
hist(residuals(precip_maxquant))
qqnorm(precip_maxquant, abline = c(0,1))

# save rds file so we can read it back in later and not have to wait 
# for everything to rerun

write_rds(precip_maxquant, "pgls_rds_files/precip_maxquant.rds")
precip_maxquant<-read_rds("pgls_rds_files/precip_maxquant.rds")
summary(precip_maxquant)

precip_max<-data.frame(coef(summary(precip_maxquant))) %>% format(scientific=F)
precip_max$Value<-as.numeric(precip_max$Value) %>% round(3)
precip_max$Std.Error<-as.numeric(precip_max$Std.Error) %>% round(3)
precip_max$t.value<-as.numeric(precip_max$t.value) %>% round(3)
precip_max$p.value<-as.numeric(precip_max$p.value) %>% round(3)
write.csv(precip_max, "pgls_rds_files/precip_max_output_table.csv")

### Extract predicted values
EFN_precip_max_means<-ggpredict(precip_maxquant, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_max_means)

fixer_precip_max_means<-ggpredict(precip_maxquant, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_max_means)

## Plot values!

p1 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=precip_maxquant, colour=EFN, shape=EFN), alpha=0.1)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("annual \n maximum precip. (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_precip_max_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                           colour=group), show.legend = FALSE, linewidth=1.4)+
  #geom_ribbon(data=EFN_precip_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  theme(legend.position="none")+
  annotate("text", label="EFN: **\nInt.:  NS", x=45, y=3000, lineheight = .75, hjust=0)

save_plot("precip_max_efn.pdf", p1)



p2 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=precip_maxquant, color=fixer), alpha=0.05)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"))+
  ylab("annual \n maximum precip. (mm)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_precip_max_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, 
                                             colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_precip_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group), 
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  theme(legend.position="none")+
  annotate("text", label="Rhizobia: NS\nInt.:          *", x=30, y=3000, lineheight = .75, hjust=0)


save_plot("precip_max_fixer.pdf", p2)



#############################
# pgls for temp maxquant
hist(data_1$temp_maxquant)
hist(log(data_1$temp_maxquant))
# pov when you do the transformation and it makes the data look WORSE

temp_maxquant <- gls(temp_maxquant ~ EFN*abs_med_lat+
                       fixer*abs_med_lat+woody+uses_num_uses+annual,
                     data=data_1, 
                     correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_maxquant)

hist(residuals(temp_maxquant))
qqnorm(temp_maxquant, abline = c(0,1))
plot(temp_maxquant)

# save rds file

write_rds(temp_maxquant, "pgls_rds_files/temp_maxquant.rds")
temp_maxquant<-read_rds("pgls_rds_files/temp_maxquant.rds")
summary(temp_maxquant)


temp_max<-data.frame(coef(summary(temp_maxquant))) %>% format(scientific=F)
temp_max$Value<-as.numeric(temp_max$Value) %>% round(3)
temp_max$Std.Error<-as.numeric(temp_max$Std.Error) %>% round(3)
temp_max$t.value<-as.numeric(temp_max$t.value) %>% round(3)
temp_max$p.value<-as.numeric(temp_max$p.value) %>% round(3)
write.csv(temp_max, "pgls_rds_files/temp_max_output_table.csv")

### Extract predicted values
EFN_temp_max_means<-ggpredict(temp_maxquant, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_max_means)

fixer_temp_max_means<-ggpredict(temp_maxquant, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_max_means)

## Plot values!

p3 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=temp_maxquant, colour=EFN, shape=EFN), alpha=0.1)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("maximum avg. annual \n temp.(\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_temp_max_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                           colour=group), show.legend = FALSE, linewidth=1.4)+
  #geom_ribbon(data=EFN_temp_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                             #fill=group),
                                             #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  theme(legend.position="none")+
  annotate("text", label="EFN: NS\nInt.:   NS", x=45, y=25, lineheight = .75, hjust=0)

save_plot("temp_max_efn.pdf", p3)



p4 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=temp_maxquant, color=fixer, shape=fixer), alpha=0.05)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"))+
  ylab("maximum avg. annual \n temp.(\u00B0C)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_temp_max_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, 
                                             colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_temp_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                               #fill=group), 
                                               #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  theme(legend.position="none")+
  annotate("text", label="Rhizobia: **\nInt.:          NS", x=34, y=25, lineheight = .75, hjust=0)


save_plot("temp_max_fixer.pdf", p4)
##########################################
# pgls for nitro maxquant
hist(log(data_1$nitro_maxquant))
hist(data_1$nitro_maxquant)

nitro_maxquant <- gls(log(nitro_maxquant) ~ EFN*abs_med_lat+
                        fixer*abs_med_lat+woody+uses_num_uses+annual,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_maxquant)

plot(nitro_maxquant)
hist(residuals(nitro_maxquant))
qqnorm(nitro_maxquant, abline = c(0,1))


# Write RDS file
write_rds(nitro_maxquant, "pgls_rds_files/nitro_maxquant.rds")
nitro_maxquant<-read_rds("pgls_rds_files/nitro_maxquant.rds")

# write into file
nitro_max<-data.frame(coef(summary(nitro_maxquant))) %>% format(scientific=F)
nitro_max$Value<-as.numeric(nitro_max$Value) %>% round(3)
nitro_max$Std.Error<-as.numeric(nitro_max$Std.Error) %>% round(3)
nitro_max$t.value<-as.numeric(nitro_max$t.value) %>% round(3)
nitro_max$p.value<-as.numeric(nitro_max$p.value) %>% round(3)
write.csv(nitro_max, "pgls_rds_files/nitro_max_output_table.csv")

### PULL MODEL OUTPUT
EFN_nitro_max_means<-ggpredict(nitro_maxquant, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_max_means)

fixer_nitro_max_means<-ggpredict(nitro_maxquant, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_max_means)

# Plot values

p5 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_maxquant, colour=EFN, shape=EFN), alpha=0.1)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("maximum soil \n nitrogen (cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_nitro_max_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                         colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_nitro_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group,
  #alpha=0.4), show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  annotate("text", label="EFN: ***\nInt.:  NS", x=45, y=1000, lineheight = .75, hjust=0)
  

save_plot("nitro_max_efn.pdf", p5)



p6 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_maxquant, color=fixer, shape=fixer), alpha=0.05)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("maximum soil \n nitrogen (cg/kg)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_nitro_max_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, 
                                           colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_nitro_max_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group), 
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  annotate("text", label="Rhizobia: NS\nInt.:           NS", x=30, y=1000, lineheight = .75, hjust=0)
  


save_plot("nitro_max_fixer.pdf", p6)


################################################################################
################################################################################
### Running PGLS on minquant data

# First check that the residuals do not, in fact, have equal variance

hist(log(data_1$precip_minquant))
hist(data_1$precip_minquant)

precip_minquant <- gls(log(precip_minquant) ~ EFN*abs_med_lat+
                         fixer*abs_med_lat+woody+uses_num_uses+annual,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_minquant)

plot(precip_minquant)
hist(residuals(precip_minquant))
qqnorm(precip_minquant, abline = c(0,1))

# write RDS

write_rds(precip_minquant, "pgls_rds_files/precip_minquant.rds")
precip_minquant<-read_rds("pgls_rds_files/precip_minquant.rds")

# output table
precip_min<-data.frame(coef(summary(precip_minquant))) %>% format(scientific=F)
precip_min$Value<-as.numeric(precip_min$Value) %>% round(3)
precip_min$Std.Error<-as.numeric(precip_min$Std.Error) %>% round(3)
precip_min$t.value<-as.numeric(precip_min$t.value) %>% round(3)
precip_min$p.value<-as.numeric(precip_min$p.value) %>% round(3)
write.csv(precip_min, "pgls_rds_files/precip_min_output_table.csv")

# grab model output
EFN_precip_min_means<-ggpredict(precip_minquant, 
                               terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_min_means)

fixer_precip_min_means<-ggpredict(precip_minquant, 
                                 terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_min_means)

# Plot values

p10 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=precip_minquant, colour=EFN, shape=EFN), alpha=0.1)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("annual \n minimum precip. (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_precip_min_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                          colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_precip_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  #guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  theme(legend.position="none")+
  annotate("text", label="EFN: NS\nInt.:   NS", x=45, y=1100, lineheight = .75, hjust=0)

save_plot("pgls_rds_files/precip_min_efn.pdf", p10)



p11 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=precip_minquant, color=fixer, shape=fixer), alpha=0.05)+
  scale_shape_manual(values = c(21,19), guide="none")+
  theme_cowplot()+scale_y_log10()+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("annual \n minimum precip. (mm)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_precip_min_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, 
                                            colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_precip_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group), 
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  theme(legend.position="none")+
  annotate("text", label="Rhizobia: **\nInt.:          ***", x=34, y=1200, lineheight = .75, hjust=0)


save_plot("pgls_rds_files/precip_min_fixer.pdf", p11)


###############################################
# pgls for temp minquant

temp_minquant <- gls(temp_minquant ~ EFN*abs_med_lat+
                       fixer*abs_med_lat+woody+uses_num_uses+annual,
                     data=data_1, 
                     correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_minquant)

hist(residuals(temp_minquant))
qqnorm(temp_minquant, abline = c(0,1))
plot(temp_minquant)

# write RDS

write_rds(temp_minquant, "pgls_rds_files/temp_minquant.rds")
temp_minquant<-read_rds("pgls_rds_files/temp_minquant.rds")

temp_min<-data.frame(coef(summary(temp_minquant))) %>% format(scientific=F)
temp_min$Value<-as.numeric(temp_min$Value) %>% round(3)
temp_min$Std.Error<-as.numeric(temp_min$Std.Error) %>% round(3)
temp_min$t.value<-as.numeric(temp_min$t.value) %>% round(3)
temp_min$p.value<-as.numeric(temp_min$p.value) %>% round(3)
write.csv(temp_min, "pgls_rds_files/temp_min_output_table.csv")


# pull model output
EFN_temp_min_means<-ggpredict(temp_minquant, 
                                terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_min_means)

fixer_temp_min_means<-ggpredict(temp_minquant, 
                                  terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_min_means)

# Plot values

p13 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=temp_minquant, colour=EFN, shape=EFN),alpha=0.1)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide = "none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("minimum avg. annual \n temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_temp_min_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted,
                                           colour=group), show.legend = FALSE, linewidth=1.4)+
  #geom_ribbon(data=EFN_temp_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  theme(legend.position="none")+
  annotate("text", label="EFN: *\nInt.:  NS", x=45, y=25, lineheight = .75, hjust=0)

save_plot("pgls_rds_files/temp_min_efn.pdf", p13)



p14 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=temp_minquant, color=fixer, shape=fixer),alpha=0.05)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide = "none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"))+
  ylab("minimum avg. annual \n temp. (\u00B0C)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_temp_min_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted,  
                                             colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_temp_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group), 
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  theme(legend.position="none")+
  annotate("text", label="Rhizobia: NS\nInt.:          NS", x=34, y=25, lineheight = .75, hjust=0)


save_plot("pgls_rds_files/temp_min_fixer.pdf", p14)


####################################################
# pgls for nitro range
hist(data_1$nitro_minquant)
hist(log(data_1$nitro_minquant))
hist(sqrt(data_1$nitro_minquant))

nitro_minquant <- gls(log(nitro_minquant) ~ EFN*abs_med_lat +
                        fixer*abs_med_lat+woody+uses_num_uses+annual,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_minquant)

plot(nitro_minquant)
hist(residuals(nitro_minquant))
qqnorm(nitro_minquant, abline = c(0,1))

# write RDS

write_rds(nitro_minquant, "pgls_rds_files/nitro_minquant.rds")

nitro_minquant<-read_rds("pgls_rds_files/nitro_minquant.rds")

# output table
nitro_min<-data.frame(coef(summary(nitro_minquant))) %>% format(scientific=F)
nitro_min$p.value<-as.numeric(nitro_min$p.value) %>% round(3)
nitro_min$Value<-as.numeric(nitro_min$Value) %>% round(3)
nitro_min$Std.Error<-as.numeric(nitro_min$Std.Error) %>% round(3)
nitro_min$t.value<-as.numeric(nitro_min$t.value) %>% round(3)
write.csv(nitro_min, "pgls_rds_files/nitro_min_output_table.csv")

# Pull model output
EFN_nitro_min_means<-ggpredict(nitro_minquant, 
                              terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_min_means)

fixer_nitro_min_means<-ggpredict(nitro_minquant, 
                                terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_min_means)

# Plot values

p15 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_minquant, colour=EFN, shape=EFN), alpha=0.1)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide = "none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("minimum soil \n nitrogen (cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_nitro_min_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                         colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_nitro_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  annotate("text", label="EFN: NS\nInt.:   NS", x=45, y=300, lineheight = .75, hjust=0)

save_plot("pgls_rds_files/nitro_min_efn.pdf", p15)



p16 <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_minquant, color=fixer, shape=fixer),alpha=0.05)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide = "none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("minimum soil \n nitrogen (cg/kg)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  geom_line(data=fixer_nitro_min_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, 
                                           colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_nitro_min_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  annotate("text", label="Rhizobia: NS\nInt.:          NS", x=32, y=300, lineheight = .75, hjust=0)


save_plot("pgls_rds_files/nitro_min_fixer.pdf", p16)

###############################################################################

efn_leg<-get_legend(p15)
plot1<-plot_grid(p1+theme(axis.title.x = element_blank()), p3+theme(axis.title.x = element_blank()), 
          p5+theme(axis.title.x = element_blank(), legend.position="none"), efn_leg, p10+theme(axis.title.x = element_blank()), 
          p13+theme(axis.title.x = element_blank()), p15+theme(axis.title.x = element_blank(), legend.position="none"), NA,
          ncol=4, nrow=2, labels=c("A", "B", "C", "", "D", "E", "F"))
plot1 <- add_sub(plot1, "absolute median latitude", hjust = 0.9, size=14)
plot(plot1)
save_plot("pgls_rds_files/niche_efn_pres_fig.jpeg", plot1, base_height=8, base_width=11)


fixer_leg<-get_legend(p6)
plot2<-plot_grid(p2+theme(axis.title.x = element_blank()), p4+theme(axis.title.x = element_blank()), 
          p6+theme(axis.title.x = element_blank(), legend.position="none"), fixer_leg, p11+theme(axis.title.x = element_blank()), 
          p14+theme(axis.title.x = element_blank()), p16+theme(axis.title.x = element_blank(), legend.position="none"), NA,
          ncol=4, nrow=2, labels=c("A", "B", "C", "", "D", "E", "F"))
plot2 <- add_sub(plot2, "absolute median latitude", hjust = 0.9, size=14)
plot(plot2)

save_plot("pgls_rds_files/niche_fixer_pres_fig.jpeg", plot2, base_height=8, base_width=11)
