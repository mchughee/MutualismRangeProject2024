# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(cowplot)
#install.packages("ggeffects")
library(ggeffects)



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

# make sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

# Erin's attempt to incorporate northern vs southern hemisphere in the analysis,
# a la MJ

data_1$hemisphere<-ifelse(data_1$median_lat>0, "0", "1")

sum(data_1$hemisphere=="0")
sum(data_1$hemisphere=="1")

data_1$hemisphere<-as.factor(data_1$hemisphere)

# Do niche variables vary in northern vs southern hemispheres?
g1<-ggplot()+geom_point(data=data_1, aes(x=median_lat, y=precip_range, colour=hemisphere))+
  geom_smooth(data=data_1, aes(x=median_lat, y=precip_range, group=hemisphere), method="lm")

g2<-ggplot()+geom_point(data=data_1, aes(x=median_lat, y=temp_range, colour=hemisphere))+
  geom_smooth(data=data_1, aes(x=median_lat, y=temp_range, group=hemisphere), method="lm")

g3<-ggplot()+geom_point(data=data_1, aes(x=median_lat, y=nitro_range, colour=hemisphere))+
  geom_smooth(data=data_1, aes(x=median_lat, y=nitro_range, group=hemisphere), method="lm")

cowplot::plot_grid(g1, g2, g3, ncol=3)

######################################

### Megan's attempt at making this simpler

### Precipitation
hist(log(data_1$precip_range))
precip_mef <- gls(log(precip_range) ~ EFN*poly(abs_med_lat, 2)+fixer*poly(abs_med_lat, 2)+woody+uses_num_uses+annual,
                  data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")
summary(precip_mef)
plot(precip_mef)
qqnorm(precip_mef, abline = c(0,1))
hist(residuals(precip_mef))

### Save as RDS file

write_rds(precip_mef, "precip_niche_breadth_mef.rds")
precip_mef<-read_rds("precip_niche_breadth_mef.rds")
summary(precip_mef)

# save model output!:')
precip_df<-data.frame(coef(summary(precip_mef))) %>% format(scientific=F)
precip_df$p.value<-as.numeric(precip_df$p.value) %>% round(4)
write.csv(precip_df, "precip_breadth_output_table.csv")


### Extract predicted values
EFN_precip_means_mef<-ggpredict(precip_mef, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_means_mef)


fixer_precip_means_mef<-ggpredict(precip_mef, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_means_mef)

### Plot
p1 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, 
                                            colour=EFN),
                          alpha=0.1)+theme_cowplot()+
 scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
 geom_line(data=EFN_precip_means_mef, aes(x=x, y=predicted, 
                    colour=group), linewidth=1.2)+
 #geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                             #fill=group, linetype=facet,
                                            #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  #guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  #theme(legend.position="none")

save_plot("precip_abs_lat_efn.pdf", p1)



p2 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, color=fixer),
                          alpha=0.1)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_precip_means_mef, aes(x=x, y=predicted,  
                                           colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                             #linetype=facet, fill=group, 
                                             #alpha=0.4), show.legend=FALSE)+
  #scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  #guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  #theme(legend.position="none")


save_plot("precip_abs_lat_fixer.pdf", p2)

p3 <- plot_grid(p1, p2, nrow=2)
save_plot("precip_abs_lat_efn_fixer.pdf", p3, base_height=8, base_width=6)
p3

##################################################################################
### Temperature
temp_mef <- gls(temp_range ~ EFN*poly(abs_med_lat, 2) + fixer*poly(abs_med_lat, 2)
                +woody+uses_num_uses+annual,
                data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_mef)

plot(temp_mef)
qqnorm(temp_mef, abline = c(0,1))
hist(residuals(temp_mef))



### Save as RDS file

write_rds(temp_mef, "temp_niche_breadth_mef.rds")
temp_mef<-read_rds("temp_niche_breadth_mef.rds")

# pull model summary table, put into conventional notation, and export
temp_df<-data.frame(coef(summary(temp_mef))) %>% format(scientific=F)
temp_df$p.value<-as.numeric(temp_df$p.value) %>% round(4)
write.csv(temp_df, "temp_breadth_output_table.csv")
  
### Extract predicted values
EFN_temp_means_mef<-ggpredict(temp_mef, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_means_mef)

fixer_temp_means_mef<-ggpredict(temp_mef, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_means_mef)

###############################################################################
### Plot
p4 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, color=EFN),
                          alpha=0.1)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("average annual temp. range (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_temp_means_mef, aes(x=x, y=predicted, 
                                           colour=group), linewidth=1.2)+
  #geom_ribbon(data=EFN_temp_means_mef, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                             #group=group, fill=group, 
                                             #alpha=0.4), show.legend=FALSE)+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  #guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  #theme(legend.position="none")

save_plot("temp_abs_lat_efn.pdf", p4)

######################################

p5<-ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, color=fixer),
                    alpha=0.1)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("average annual temp. range (\u00B0C)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_temp_means_mef, aes(x=x, y=predicted, 
                                         colour=group), linewidth=1.2)+
  #geom_ribbon(data=fixer_temp_means_mef, aes(x=x, ymin=predicted-(std.error), ymax=predicted+(std.error), 
                                               #group=group, fill=group, 
                                               #alpha=0.7), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))
  #guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))+
  #theme(legend.position="none")


save_plot("temp_abs_lat_fixer.pdf", p5)

p6 <- plot_grid(p4, p5, nrow=2)
save_plot("temp_abs_lat_efn_fixer.pdf", p6, base_height=8, base_width=6)
p6

###############################################################################
nitro_mef <- gls(log(nitro_range) ~ EFN*poly(median_lat, 2)+fixer*poly(median_lat, 2)+woody+uses_num_uses+annual,
                  data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_mef)

plot(nitro_mef)
qqnorm(nitro_mef, abline = c(0,1))
hist(residuals(nitro_mef))

### Save as RDS file

write_rds(nitro_mef, "nitro_niche_breadth_mef.rds")
nitro_mef<-read_rds("nitro_niche_breadth_mef.rds")

# pull model summary table, put into conventional notation, and export
nitro_df<-data.frame(coef(summary(nitro_mef))) %>% format(scientific=F)
nitro_df$p.value<-as.numeric(nitro_df$p.value) %>% round(4)
write.csv(nitro_df, "nitro_breadth_output_table.csv")

### Extract predicted values
EFN_nitro_means_mef<-ggpredict(nitro_mef, terms=c("median_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_means_mef)

fixer_nitro_means_mef<-ggpredict(nitro_mef, terms=c("median_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_means_mef)

############################################

### Plot

p7 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, color=EFN),
                          alpha=0.4)+theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("nitrogen range \n(cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=EFN_nitro_means_mef, aes(x=x, y=predicted, 
                                          colour=group), linewidth=1.2)
  #geom_ribbon(data=EFN_nitro_means_mef, aes(x=x, ymin=predicted-std.error.exp, ymax=predicted+std.error.exp, 
                                             #group=group, fill=group, 
                                             #alpha=0.7), show.legend=FALSE)+
 # scale_fill_ghibli_d("YesterdayMedium", direction = -1)
 # guides(linetype=guide_legend("hemisphere"))+
  #scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))

save_plot("nitro_abs_lat_efn.pdf", p7)


p8 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, color=fixer),
                          alpha=0.1)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("nitrogen range \n(cg/kg)")+
  xlab("absolute median latitude")+
  geom_line(data=fixer_nitro_means_mef, aes(x=x, y=predicted,  
                                           colour=group), show.legend = FALSE, linewidth=1.2)+
  #geom_ribbon(data=fixer_nitro_means_mef, aes(x=x, ymin=predicted-4*exp(std.error), ymax=predicted+4*exp(std.error), 
                                             #group=group, fill=group, 
                                             #alpha=0.7), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  guides(linetype=guide_legend("hemisphere"))+
  scale_linetype_manual(values=c(1, 3), labels=c("northern", "southern"))


save_plot("nitro_abs_lat_fixer.pdf", p8)

p9 <- plot_grid(p7, p8, nrow=2)
save_plot("nitro_abs_lat_efn_fixer.pdf", p9, base_height=8, base_width=6)
p9

leg_fixer<-cowplot::get_legend(p8)
leg_efn<-cowplot::get_legend(p7)

p10<-cowplot::plot_grid(p1, p4, p7,  
                        p2, p5, p8,
                        leg_efn, leg_fixer,
                        nrow=2, ncol=3)
p10
