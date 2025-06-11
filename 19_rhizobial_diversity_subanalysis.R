### Subanalysis of how level of specialization in plant-rhizobial interactions
# influences niche breadth

library(ggplot2)
library(tidyverse)
library(ape)
library(phytools)
library(nlme)

# read in csv file with my species-level environmental data

mydat<-read.csv("data_files/pgls_polydropped_final_biome.csv")

divdat<-read.csv("data_files/Legume_ploidy_dataset.csv")
divdat$numGenera<-as.numeric(divdat$numGenera)
summary(divdat$numGenera)

# Which diversity/specialization metric has the highest coverage?
colSums(!is.na(divdat))
# numOTUs- 19
# numGenera- 141
# Specialist- 141

# make new dataframe with just species that have a numGenera
divfil<-divdat %>% filter(!is.na(numGenera))

# attach these data to my dataset
mydat$numGenera <- divfil$numGenera[match(mydat$species, divfil$Species)]

# reduce dataset to just the species with numGenera
myfil<-mydat %>% filter(!is.na(numGenera))

# add in abs med lat
myfil$abs_med_lat<-abs(myfil$median_lat)
myfil$EFN<-as.factor(myfil$EFN)
# Try pgls?

mytree<-read.tree("polytomy_removed.tre")

diff <- setdiff(mytree$tip.label, myfil$species)
tree_pruned <- drop.tip(mytree, diff)

# make rows in data match rows in tree
myfil1 <- myfil[match(tree_pruned$tip.label, myfil$species),]
class(myfil1$numGenera)
max(myfil1$numGenera)
min(myfil1$numGenera)

ggplot(myfil1, aes(x=numGenera, y=precip_range))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

ggplot(myfil1, aes(x=numGenera, y=temp_range))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

ggplot(myfil1, aes(x=numGenera, y=nitro_range))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

# run actual models
## precip first

# I took n out to try without it!!!! But please know that it should be put back
# in!
precip_range <- gls(log(precip_range) ~ EFN*abs_med_lat + woody
                    + uses_num_uses + annual + biome + numGenera,
                    data=myfil1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)
qqnorm(precip_range, abline = c(0,1))
hist(residuals(precip_range))


### Save as RDS file

write_rds(precip_range, "biome_pgls_output/numgen_precip_niche_breadth.rds")

# Read in RDS file (if coming back to code)
precip_range<-read_rds("biome_pgls_output/numgen_precip_niche_breadth.rds")

# save model output!:')
precip_df<-data.frame(coef(summary(precip_range))) %>% format(scientific=F)
precip_df$p.value<-as.numeric(precip_df$p.value) %>% round(4)
write.csv(precip_df, "biome_pgls_output/numgen_precip_breadth_output_table.csv")


##################################

### Pull predicted means for EFN and fixer

precip_means<-ggpredict(precip_range, terms=c("numGenera [all]"), type="fixed")
plot(precip_means)


###########################
# make ggplots for EFN and rhizobia separately
p1 <- ggplot()+
  geom_point(data=myfil1, aes(x=numGenera, y=precip_range, colour=as.factor(numGenera)),alpha=0.7)+
  theme_cowplot()+scale_y_log10()+
  #scale_shape_manual(values = c(21, 19), guide = "none")+
  #scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"), name = "EFN")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("annual \n precip. range (mm)")+
  xlab("number of genera")+
  #theme(axis.title.x=element_blank())+
  geom_line(data=precip_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted), linewidth=1.4)
  #geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
  #fill=group),
  #alpha=0.4, show.legend=FALSE)+
  #scale_fill_("YesterdayMedium", direction = -1)
  #scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  #annotate("text", label="EFN: **\nInt.:   NS", x=50, y=2000, lineheight = .75, hjust=0)


save_plot("biome_pgls_output/numgen_precip_breadth.pdf", p1)


################################################################################
# pgls for temp range

temp_range <- gls(temp_range ~ EFN*abs_med_lat
                  + woody + uses_num_uses
                  + annual+biome+numGenera,
                  data=myfil1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))
hist(residuals(temp_range))
plot(temp_range)


### Save as RDS file

write_rds(temp_range, "biome_pgls_output/numgen_temp_niche_breadth.rds")

# Read in RDS file (if coming back to code)
temp_range<-read_rds("biome_pgls_output/numgen_temp_niche_breadth.rds")

# save model output!:')
temp_df<-data.frame(coef(summary(temp_range))) %>% format(scientific=F)
temp_df$p.value<-as.numeric(temp_df$p.value) %>% round(4)
write.csv(temp_df, "biome_pgls_output/numgen_temp_breadth_output_table.csv")


##################################
### Pull predicted means for EFN and fixer

temp_means<-ggpredict(temp_range, terms=c("numGenera [all]"), type="fixed")
plot(temp_means)


###########################
# make ggplots for EFN and rhizobia separately
p2 <- ggplot()+
  geom_point(data=myfil1, aes(x=numGenera, y=temp_range, colour=as.factor(numGenera)),alpha=0.7)+
  theme_cowplot()+scale_y_log10()+
  #scale_shape_manual(values = c(21, 19), guide = "none")+
  #scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"), name = "EFN")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("annual \n temp. range (mm)")+
  xlab("numGenera")+
  theme(axis.title.x=element_blank())+
  geom_line(data=temp_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted), linewidth=1.4)
#geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
#fill=group),
#alpha=0.4, show.legend=FALSE)+
#scale_fill_("YesterdayMedium", direction = -1)
#scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
#annotate("text", label="EFN: **\nInt.:   NS", x=50, y=2000, lineheight = .75, hjust=0)


save_plot("biome_pgls_output/numgen_temp_breadth.pdf", p2)



##########################################################################
#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN*abs_med_lat
                   + woody + uses_num_uses + annual + biome +numGenera,
                   data=myfil1, 
                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)
hist(residuals(nitro_range))
qqnorm(temp_range, abline = c(0,1))

### Save as RDS file

write_rds(nitro_range, "biome_pgls_output/numgen_nitro_niche_breadth.rds")

# Read in RDS file (if coming back to code)
nitro_range<-read_rds("biome_pgls_output/numgen_nitro_niche_breadth.rds")

# save model output!:')
nitro_df<-data.frame(coef(summary(nitro_range))) %>% format(scientific=F)
nitro_df$p.value<-as.numeric(nitro_df$p.value) %>% round(4)
write.csv(nitro_df, "biome_pgls_output/numgen_nitro_breadth_output_table.csv")


##################################

##################################
### Pull predicted means for EFN and fixer

nitro_means<-ggpredict(nitro_range, terms=c("numGenera [all]"), type="fixed")
plot(nitro_means)


###########################
# make ggplots for EFN and rhizobia separately
p3 <- ggplot()+
  geom_point(data=myfil1, aes(x=numGenera, y=nitro_range, colour=as.factor(numGenera)),alpha=0.7)+
  theme_cowplot()+scale_y_log10()+
  #scale_shape_manual(values = c(21, 19), guide = "none")+
  #scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"), name = "EFN")+
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("no", "yes"))+
  ylab("nitro. range (mm)")+
  xlab("numGenera")+
  theme(axis.title.x=element_blank())+
  geom_line(data=nitro_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted), linewidth=1.4)
#geom_ribbon(data=EFN_precip_means, aes(x=x, ymin=conf.low, ymax=conf.high, 
#fill=group),
#alpha=0.4, show.legend=FALSE)+
#scale_fill_("YesterdayMedium", direction = -1)
#scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
#annotate("text", label="EFN: **\nInt.:   NS", x=50, y=2000, lineheight = .75, hjust=0)


save_plot("biome_pgls_output/numgen_nitro_breadth.pdf", p3)



leg_fixer<-get_legend(p2)
efn_fixer<-get_legend(p1)
comp_leg<-plot_grid(leg_fixer, efn_fixer, ncol=1, nrow=2)

p<-cowplot::plot_grid(p1+ theme(legend.position="none"), p2+ theme(legend.position="none", axis.title.y=element_blank()), comp_leg,
                      p3+ theme(legend.position="none"), p4+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                      p5+ theme(legend.position="none"), p6+ theme(legend.position="none", axis.title.y=element_blank()), NA,
                      ncol=3, nrow=3, labels=c("A", "D", "", "B", "E", "", "C", "F", ""),
                      label_x = c(0, 0, 0, 0, -0.035, 0, 0, 0, 0))

p <- add_sub(p, "absolute median latitude", hjust = 1.5, size=12)

plot(p)

save_plot("biome_pgls_output/niche_breadth_thesis_fig.jpeg", p, base_height=10, base_width=10)

