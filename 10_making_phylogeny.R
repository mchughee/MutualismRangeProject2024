# Trying out V.phylomaker

#install.packages("devtools")
library(devtools)
#devtools::install_github("jinyizju/V.PhyloMaker")
library("V.PhyloMaker")
library(tidyverse)

# setwd
#setwd("C:/Users/erinm/Downloads")

# read in dataset
dat<-read.csv("thinned_data.csv")
dat$species<-gsub("_", " ", dat$species)

dat$species<-as.factor(dat$species)

sp_list<-dat %>% 
  group_by(species) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  reframe(species, genus, family)


sp_list$species.relative<-NA
sp_list$genus.relative<-NA


tree <- phylo.maker(sp.list = sp_list,
                    tree = GBOTB.extended,
                    nodes=nodes.info.2,
                    scenarios="S1")

write.tree(tree$scenario.1, "phylogeny_all_buildnodes2.tre")
