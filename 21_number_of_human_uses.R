

library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(ggplot2)
library(cowplot)
#install.packages("ggeffects")
library(ggeffects)



# Read back in PGLS dataframe
data<-read.csv("data_files/pgls_polydropped_final_biome.csv")


# investigating latitudinal gradient in life-history

data$pannual<-ifelse(data$annual<0.5, "0", "1")
data$pannual
data$pannual<-as.factor(data$pannual)
ggplot(data=data, aes(x=median_lat, fill=pannual))+
  geom_histogram()

data$pwoody<-ifelse(data$woody<0.5, "0", "1")
data$pwoody
data$pwoody<-as.factor(data$pwoody)
ggplot(data=data, aes(x=median_lat, fill=pwoody))+
  geom_histogram()



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


# VERY, VERRRRYYYYYY IMPORTANT! DO NOT RUN WITHOUT:
# making sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

sum(data_1$mutualism=="0")
sum(data_1$mutualism=="1")

sum(data_1$EFN=="0")
sum(data_1$EFN=="1")

sum(data_1$fixer=="0")
sum(data_1$fixer=="1")

# Find farther extent of EFN

max(data_1[data_1$EFN=="1",]$abs_med_lat)
max(data_1[data_1$fixer=="0",]$abs_med_lat)
min(data_1[data_1$fixer=="0",]$abs_med_lat)

#################################
# PRECIP FIRST

precip_range<-read_rds("pgls_rds_files/precip_niche_breadth.rds")

##################################

### Pull predicted means for EFN and fixer

EFN_precip_means<-ggpredict(precip_range, terms=c("uses_num_uses [all]", "EFN [all]"), type="fixed")
plot(EFN_precip_means)


fixer_precip_means<-ggpredict(precip_range, terms=c("uses_num_uses [all]", "fixer [all]"), type="fixed")
plot(fixer_precip_means)


###########################
# make ggplots for EFN and rhizobia separately

p1 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=precip_range, shape=EFN, colour=EFN),alpha=0.5)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21, 19), guide = "none")+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"), name = "EFN")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("annual \n precip. range (mm)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_precip_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                                                        colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))
  #annotate("text", label="EFN: **\nInt.:   NS", x=50, y=2000, lineheight = .75, hjust=0)

p1 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=precip_range, shape=EFN, colour=EFN),alpha=0.5)+
  #geom_boxplot(data=data_1, aes(x=uses_num_uses, y=precip_range, group=EFN, colour=EFN),alpha=0.5)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21, 19), guide = "none")+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"), name = "EFN")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("annual \n precip. range (mm)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_precip_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                                                        colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))
#annotate("text", label="EFN: **\nInt.:   NS", x=50, y=2000, lineheight = .75, hjust=0)


save_plot("human_uses/precip_breadth_lat_efn.pdf", p1)



p2 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=precip_range, color=fixer, shape=fixer),
             alpha=0.5)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide = "none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("annual \n precipitation range (mm)")+
  xlab("number of human uses")+
  labs(colour="rhizobia")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_precip_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted,  
                                                                          colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group), 
  #alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))
  #annotate("text", label="Rhizobia: NS\n      Int.:   **", x=42, y=2000, lineheight = .75, hjust=0)


save_plot("human_uses/precip_breadth_lat_fixer.pdf", p2)


###############################################################################
# Temp next

temp_range<-read_rds("pgls_rds_files/temp_niche_breadth.rds")

##################################

### Pull predicted means for EFN and fixer

EFN_temp_means<-ggpredict(temp_range, terms=c("uses_num_uses [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_means)


fixer_temp_means<-ggpredict(temp_range, terms=c("uses_num_uses [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_means)


###########################
# make ggplots

p3 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=temp_range, colour=EFN, shape=EFN),alpha=0.5)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("average annual \n temp. range (\u00B0C)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_temp_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                                                      colour=group), linewidth=1.2)+
  #geom_ribbon(data=EFN_temp_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))
  #annotate("text", label="EFN: *\nInt.:   NS", x=50, y=15, lineheight = .75, hjust=0)


save_plot("human_uses/temp_lat_efn.pdf", p3)



p4 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=temp_range, color=fixer, shape=fixer),alpha=0.5)+
  theme_cowplot()+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  scale_shape_manual(values = c(21,19), guide="none")+
  ylab("average annual temp. range \n (\u00B0C)")+
  xlab("number of human uses")+
  labs(colour="rhizobia")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_temp_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted,  
                                                                        colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_temp_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group, 
  #alpha=0.4), show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))
  #annotate("text", label="Rhizobia: ***\n         Int.: ***", x=44, y=15, lineheight = .75, hjust=0)


save_plot("human_uses/temp_lat_fixer.pdf", p4)


##########################################################################
# Nitrogen, finally

nitro_range<-read_rds("biome_pgls_output/nitro_niche_breadth.rds")

### Pull predicted means for EFN and fixer

EFN_nitro_means<-ggpredict(nitro_range, terms=c("uses_num_uses [all]", "EFN [all]"), type="fixed")
plot(EFN_nitro_means)


fixer_nitro_means<-ggpredict(nitro_range, terms=c("uses_num_uses [all]", "fixer [all]"), type="fixed")
plot(fixer_nitro_means)


###########################
# make ggplots

p5 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=nitro_range, colour=EFN, shape=EFN), alpha=0.2)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("soil nitrogen \n range (cg/kg)")+
  xlab("number of human uses")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_nitro_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, 
                                                                       colour=group), linewidth=1.4)+
  #geom_ribbon(data=EFN_nitro_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_ghibli_d("YesterdayMedium", direction = -1)
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))
  #annotate("text", label="EFN: **\nInt.:   NS", x=50, y=800, lineheight = .75, hjust=0)


save_plot("human_uses/nitro_lat_efn.pdf", p5)



p6 <- ggplot()+
  geom_point(data=data_1, aes(x=uses_num_uses, y=nitro_range, color=fixer, shape=fixer),alpha=0.5)+
  theme_cowplot()+scale_y_log10()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("soil nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  labs(colour="rhizobia")+
  theme(axis.title.x=element_blank())+
  geom_line(data=fixer_nitro_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted,  
                                                                         colour=group), linewidth=1.4)+
  #geom_ribbon(data=fixer_nitro_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #                                      fill=group), 
  #                                     alpha=0.4, show.legend=FALSE)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))
  #annotate("text", label="Rhizobia: NS\n       Int.:   *", x=42, y=800, lineheight = .75, hjust=0)

save_plot("human_uses/nitro_lat_fixer.pdf", p6)


### making compound plot
leg_fixer<-get_legend(p2)
efn_fixer<-get_legend(p1)
comp_leg<-plot_grid(leg_fixer, efn_fixer, ncol=1, nrow=2)

p<-cowplot::plot_grid(p1+ theme(legend.position="none"), p2+ theme(legend.position="none", axis.title.y=element_blank()), comp_leg,
                      p3+ theme(legend.position="none"), p4+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                      p5+ theme(legend.position="none"), p6+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                      ncol=3, nrow=3, labels=c("A", "D", "", "B", "E", "", "C", "F", ""),
                      label_x = c(0, 0, 0, 0, -0.035, 0, 0, 0, 0))

p <- add_sub(p, "number of human uses", hjust = 1.5, size=12)

plot(p)

save_plot("human_uses/niche_breadth_thesis_fig.jpeg", p, base_height=10, base_width=10)
