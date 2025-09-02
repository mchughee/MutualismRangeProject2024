###### Making biome figure

# library
library(ggplot2)
library(tidyverse)
library(ape)
library(ggtree)
library(ghibli)
library(ape)
library(ggtree)

# read in file
master_thin<-read.csv("data_files/pgls_summary_data_long_added_biome_added.csv")

###############################################################################
# Making figures to explore the effects of biome
# Bringing in phylogeny to thin to just the species in our final dataset
# (remember, we took some species not in the phylogeny out of our analysis)
tree<-read.tree("phylogeny/polytomy_removed.tre")


setdiff(master_thin$species, tree$tip.label)
master_thin<-filter(master_thin, master_thin$species %in% tree$tip.label)

########################
# Do species engaged in mutualisms occur in more biomes than non-mutualistic species?
master_thin$EFN<-as.factor(master_thin$EFN)
master_thin$fixer<-as.factor(master_thin$fixer)

efn_biome<-master_thin %>%
  ggplot(aes(x=EFN, y=num_biome, fill=EFN))+
  geom_boxplot(stat="boxplot")+
  #geom_violin()+
  theme_classic()+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  scale_x_discrete(labels= c("no", "yes"))+
  ylab("biome number")

rhiz_biome<-master_thin %>%
  ggplot(aes(x=fixer, y=num_biome, fill=fixer))+
  geom_boxplot(stat="boxplot")+
  #geom_violin()+
  theme_classic()+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"), name="Rhizobia")+
  scale_x_discrete(labels= c("no", "yes"))+
  xlab("Rhizobia")+
  theme(axis.title.y=element_blank())

biomefig<-cowplot::plot_grid(efn_biome, rhiz_biome)

########################################
### cowplot this compound figure together with other figure
### that contains histogram showing distribution of 
### species by latitude and mutualism
master_thin$mutualism<-ifelse(master_thin$EFN=="1" & master_thin$fixer=="0", "EFN",
                              ifelse(master_thin$EFN=="0" & master_thin$fixer=="1", "Rhizobia",
                                     ifelse(master_thin$EFN=="1" & master_thin$fixer=="1", "Both", "None")))
                              

histo<-master_thin %>% 
  mutate(mutualism=fct_relevel(mutualism, "Both", "EFN", "Rhizobia", "None")) %>% 
  ggplot(aes(x=median_lat, fill=mutualism))+
  geom_histogram()+
  theme_classic()+
  xlab("Median latitude")+
  ylab("Count")+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF", "#26432FFF", "#E7A79BFF"))


###################################
#cowplot them alll together!

twopanel<-cowplot::plot_grid(histo, biomefig, nrow=2, ncol=1, labels=c("A", "B"))
save_plot("twopanel_descriptiveresults.jpg", twopanel, base_height = 4.5)


########################################
# Are the means different?
mean(master_thin[master_thin$EFN=="0",]$num_biome)
mean(master_thin[master_thin$EFN=="1",]$num_biome)

mean(master_thin[master_thin$fixer=="0",]$num_biome)
mean(master_thin[master_thin$fixer=="1",]$num_biome)

# t-test to check if difference is significant
t.test(num_biome ~ EFN, data = master_thin, var.equal = TRUE)
t.test(num_biome ~ fixer, data = master_thin, var.equal = TRUE)

