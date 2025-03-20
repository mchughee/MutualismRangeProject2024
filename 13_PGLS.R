# Running PGLS
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
data$mutualism<-ifelse(data$EFN==1 | data$fixer==1, "1", "0")


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

sum(data_1$mutualism=="0")
sum(data_1$mutualism=="1")

sum(data_1$EFN=="0")
sum(data_1$EFN=="1")

sum(data_1$fixer=="0")
sum(data_1$fixer=="1")

######################################
## precip first

# I took n out to try without it!!!! But please know that it should be put back
# in!
precip_range <- gls(log(precip_range) ~ EFN*abs_med_lat + fixer*abs_med_lat+woody
                    + uses_num_uses + annual,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)
qqnorm(precip_range, abline = c(0,1))
hist(residuals(precip_range))


### Save as RDS file

write_rds(precip_range, "precip_niche_breadth.rds")

# Read in RDS file (if coming back to code)
precip_range<-read_rds("precip_niche_breadth.rds")

# save model output!:')
precip_df<-data.frame(coef(summary(precip_range))) %>% format(scientific=F)
precip_df$p.value<-as.numeric(precip_df$p.value) %>% round(4)
write.csv(precip_df, "precip_breadth_output_table.csv")


##################################

### Pull predicted means for EFN and fixer

EFN_precip_means<-ggpredict(precip_range, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_means)


fixer_precip_means<-ggpredict(precip_range, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_means)


###########################
# make ggplots

p1 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, 
                                           colour=EFN),
                          alpha=0.4)+theme_cowplot()+scale_y_log10()+
  scale_colour_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("logged annual \n precip. range (mm)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_precip_means, aes(x=x, y=predicted, 
                                           colour=group), linewidth=1.4)+
  geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  fill=group),
  alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"))+
  annotate("text", label="EFN: **\nInt.:   NS", x=50, y=3700, lineheight = .75, hjust=0)


save_plot("precip_breadth_lat_efn.pdf", p1)



p2 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=precip_range, color=fixer),
                          alpha=0.4)+theme_cowplot()+scale_y_log10()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("logged annual \n precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_precip_means, aes(x=x, y=predicted,  
                                             colour=group), linewidth=1.4)+
  geom_ribbon(data=fixer_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  fill=group), 
  alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  annotate("text", label="Fixer: NS\nInt.:   **", x=50, y=3700, lineheight = .75, hjust=0)
  
  
save_plot("precip_breadth_lat_fixer.pdf", p2)

################################################################################
# pgls for temp range

temp_range <- gls(temp_range ~ EFN*abs_med_lat + fixer*abs_med_lat
                  + woody + uses_num_uses
                  + annual,
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))
hist(residuals(temp_range))
plot(temp_range)


### Save as RDS file

write_rds(temp_range, "temp_niche_breadth.rds")

# Read in RDS file (if coming back to code)
temp_range<-read_rds("temp_niche_breadth.rds")

# save model output!:')
temp_df<-data.frame(coef(summary(temp_range))) %>% format(scientific=F)
temp_df$p.value<-as.numeric(temp_df$p.value) %>% round(4)
write.csv(temp_df, "temp_breadth_output_table.csv")


##################################

### Pull predicted means for EFN and fixer

EFN_temp_means<-ggpredict(temp_range, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_means)


fixer_temp_means<-ggpredict(temp_range, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_means)


###########################
# make ggplots

p3 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, 
                                           colour=EFN),
                          alpha=0.4)+theme_cowplot()+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("average annual \n temp. range (\u00B0C)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_temp_means, aes(x=x, y=predicted, 
                                       colour=group), linewidth=1.2)+
  geom_ribbon(data=EFN_temp_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                         fill=group),
                                         alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"))+
  annotate("text", label="EFN: *\nInt.:   NS", x=50, y=24, lineheight = .75, hjust=0)


save_plot("temp_lat_efn.pdf", p3)



p4 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=temp_range, color=fixer),
                          alpha=0.4)+theme_cowplot()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("average annual temp. range \n (\u00B0C)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_temp_means, aes(x=x, y=predicted,  
                                         colour=group), linewidth=1.4)+
  geom_ribbon(data=fixer_temp_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                           fill=group, 
                                           alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  annotate("text", label="Fixer: ***\nInt.: ***", x=50, y=24, lineheight = .75, hjust=0)


save_plot("temp_lat_fixer.pdf", p4)


##########################################################################
#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN*abs_med_lat + fixer*abs_med_lat
              + woody + uses_num_uses + annual,
              data=data_1, 
              correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)
hist(residuals(nitro_range))
qqnorm(temp_range, abline = c(0,1))

### Save as RDS file

write_rds(nitro_range, "nitro_niche_breadth.rds")

# Read in RDS file (if coming back to code)
nitro_range<-read_rds("nitro_niche_breadth.rds")

# save model output!:')
nitro_df<-data.frame(coef(summary(nitro_range))) %>% format(scientific=F)
nitro_df$p.value<-as.numeric(nitro_df$p.value) %>% round(4)
write.csv(nitro_df, "nitro_breadth_output_table.csv")


##################################

### Pull predicted means for EFN and fixer

EFN_nitro_means<-ggpredict(nitro_range, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_means)


fixer_nitro_means<-ggpredict(nitro_range, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_means)


###########################
# make ggplots

p5 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, 
                                           colour=EFN),
                          alpha=0.4)+theme_cowplot()+scale_y_log10()+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("logged soil nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_nitro_means, aes(x=x, y=predicted, 
                                     colour=group), linewidth=1.4)+
  geom_ribbon(data=EFN_nitro_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                       fill=group),
                                       alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"))+
  annotate("text", label="EFN: **\nInt.:   NS", x=50, y=1600, lineheight = .75, hjust=0)


save_plot("nitro_lat_efn.pdf", p5)



p6 <- ggplot()+geom_point(data=data_1, aes(x=abs_med_lat, y=nitro_range, color=fixer),
                          alpha=0.4)+theme_cowplot()+scale_y_log10()+
  scale_colour_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("logged soil nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_nitro_means, aes(x=x, y=predicted,  
                                       colour=group), linewidth=1.4)+
  geom_ribbon(data=fixer_nitro_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
                                         fill=group), 
                                         alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  annotate("text", label="Fixer: NS\nInt.:   *", x=50, y=1600, lineheight = .75, hjust=0)


save_plot("nitro_lat_fixer.pdf", p6)

leg_fixer<-get_legend(p2)
efn_fixer<-get_legend(p1)
comp_leg<-plot_grid(leg_fixer, efn_fixer, ncol=1, nrow=2)

p<-cowplot::plot_grid(p1+ theme(legend.position="none"), p2+ theme(legend.position="none", axis.title.y=element_blank()), comp_leg,
                   p3+ theme(legend.position="none"), p4+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                   p5+ theme(legend.position="none"), p6+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                   ncol=3, nrow=3)

p <- add_sub(p, "median latitude", hjust = 1.8, size=12)

plot(p)

