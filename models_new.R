# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(cowplot)
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

######################################

### Megan's attempt at making this simpler

### Precipitation
precip_mef <- gls(log(precip_range) ~ EFN*abs_med_lat+fixer*abs_med_lat+woody+uses_num_uses+annual,
                  data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")
summary(precip_mef)
plot(precip_mef)
qqnorm(precip_mef, abline = c(0,1))
hist(residuals(precip_mef))

### Save as RDS file

write_rds(precip_mef, "precip_niche_breadth_mef.rds")

### Extract predicted values
EFN_precip_means_mef<-ggpredict(precip_mef, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_means_mef)

fixer_precip_means_mef<-ggpredict(precip_mef, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_means_mef)

### Plot
p1 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, color=EFN), alpha=0.4)+theme_cowplot()+scale_y_log10()
#p1 <- p1+geom_ribbon(data=EFN_precip_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p1 <- p1+geom_line(data=EFN_precip_means_mef, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE, linewidth=1.2
)
save_plot("precip_abs_lat_efn.pdf", p1)

p2 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, color=fixer), alpha=0.4)+theme_cowplot()+scale_y_log10()
#p2 <- p2+geom_ribbon(data=fixer_precip_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p2 <- p2+geom_line(data=fixer_precip_means_mef, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE, linewidth=1.2)
save_plot("precip_abs_lat_fixer.pdf", p2)

p3 <- plot_grid(p1, p2, nrow=2)
save_plot("precip_abs_lat_efn_fixer.pdf", p3, base_height=8, base_width=6)
p3

##################################################################################
### Temperature
temp_mef <- gls(log(temp_range) ~ EFN*abs_med_lat+fixer*abs_med_lat+woody+uses_num_uses+annual,
                data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")
summary(temp_mef)

### Save as RDS file

write_rds(temp_mef, "temp_niche_breadth_mef.rds")

### Extract predicted values
EFN_temp_means_mef<-ggpredict(temp_mef, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_means_mef)

fixer_temp_means_mef<-ggpredict(temp_mef, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_means_mef)

### Plot
p4 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, color=EFN), alpha=0.4)+theme_cowplot()+scale_y_log10()
#p4 <- p4+geom_ribbon(data=EFN_temp_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p4 <- p4+geom_line(data=EFN_temp_means_mef, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE, linewidth=1.2
)
save_plot("temp_abs_lat_efn.pdf", p4)

p5 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, color=fixer), alpha=0.4)+theme_cowplot()+scale_y_log10()
#p5 <- p5+geom_ribbon(data=fixer_temp_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p5 <- p5+geom_line(data=fixer_temp_means_mef, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE, linewidth=1.2)
save_plot("temp_abs_lat_fixer.pdf", p5)

p6 <- plot_grid(p4, p5, nrow=2)
save_plot("temp_abs_lat_efn_fixer.pdf", p6, base_height=8, base_width=6)
p6

###############################################################################
nitro_mef <- gls(log(nitro_range) ~ EFN*abs_med_lat+fixer*abs_med_lat+woody+uses_num_uses+annual,
                  data=data_1, correlation=corPagel(1, tree_pruned, form=~species), method="ML")
summary(nitro_mef)
plot(nitro_mef)
qqnorm(nitro_mef, abline = c(0,1))
hist(residuals(nitro_mef))

### Save as RDS file

write_rds(temp_mef, "nitro_niche_breadth_mef.rds")

### Extract predicted values
EFN_nitro_means_mef<-ggpredict(nitro_mef, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_means_mef)

fixer_nitro_means_mef<-ggpredict(nitro_mef, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_means_mef)

### Plot
p7 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, color=EFN), 
                          alpha=0.4)+theme_cowplot()+scale_y_log10()
#p4 <- p4+geom_ribbon(data=EFN_temp_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p7 <- p7+geom_line(data=EFN_nitro_means_mef, aes(x=x, y=predicted, group = group, colour=group), 
                   show.legend = FALSE, linewidth=1.2
)
save_plot("nitro_abs_lat_efn.pdf", p7)

p8 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, color=fixer), alpha=0.4)+theme_cowplot()+scale_y_log10()
#p5 <- p5+geom_ribbon(data=fixer_temp_means_mef, aes(x=x, ymax=conf.high, ymin=conf.low, group=group, fill=group, alpha=0.001), show.legend=FALSE)
p8 <- p8+geom_line(data=fixer_nitro_means_mef, aes(x=x, y=predicted, group = group, colour=group), show.legend = FALSE, linewidth=1.2)
save_plot("nitro_abs_lat_fixer.pdf", p8)

p9 <- plot_grid(p7, p8, nrow=2)
save_plot("temp_abs_lat_efn_fixer.pdf", p6, base_height=8, base_width=6)
p9
