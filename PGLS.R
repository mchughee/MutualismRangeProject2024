install.packages("caper")
install.packages("geiger")

library(ape)
library(geiger)
library(treeplyr)
library(caper)
library(tidyverse)

# Read in phylogenetic tree

tree<-ape::read.tree("phylogeny_all_buildnodes.tre")
plot(tree)

# Read in summarized trait data (this file is NOT the be all, end all, but
# what I have right now)

traits<-read.csv("summary_df_august2024.csv")

traits$precip_range<-traits$maxquant_precip-traits$minquant_precip

legumes <- comparative.data(phy = tree, data = traits, 
                         names.col = species, vcv = TRUE, 
                         na.omit = FALSE, warn.dropped = TRUE, force.root = TRUE)

legumes$dropped$tips

legumes$dropped$unmatched.rows

# Try out a PGLS model

model.pgls <- pgls(precip_range ~ , 
                   data = frog, lambda = "ML")