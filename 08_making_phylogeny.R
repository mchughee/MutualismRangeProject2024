# Making a tree using V.phylomaker

# Read in devtools to download V.phylomaker from github
#install.packages("devtools")
library(devtools)
#devtools::install_github("jinyizju/V.PhyloMaker")
library("V.PhyloMaker")
library(tidyverse)

# setwd
#setwd("C:/Users/erinm/Downloads")

# read in dataset
dat<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
#dat$species<-gsub("_", " ", dat$species)

# make species a factor
dat$species<-as.factor(dat$species)

# create the required dataframe with species, genus, and family
sp_list<-dat %>% 
  group_by(species) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  reframe(species, genus, family)

# add the two columns to the dataframe that V.phylomaker needs
sp_list$species.relative<-NA
sp_list$genus.relative<-NA

# make the actual tree-- choose more conservative arguments
tree <- phylo.maker(sp.list = sp_list,
                    tree = GBOTB.extended,
                    nodes=nodes.info.1,
                    scenarios="S1")

# write tree into a .tre file
write.tree(tree$scenario.1, "phylogeny_buildnodes1_droppedspecies.tre")
