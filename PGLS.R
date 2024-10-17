install.packages("caper")
install.packages("geiger")
#install.packages("treeplyr")

library(ape)
library(geiger)
#library(treeplyr)
library(caper)
library(tidyverse)

# Read in phylogenetic tree

tree<-ape::read.tree("phylogeny_all_buildnodes.tre")
plot(tree)

# Read in summarized trait data (this file is NOT the be all, end all, but
# what I have right now)

traits<-read.csv("summary_df_august2024.csv")

traits$precip_range<-traits$maxquant_precip-traits$minquant_precip

# Read in the original legume trait data
mutualisms<-read.csv("legume_range_traits.csv")
mutualisms$Phy<-gsub(" ", "_", mutualisms$Phy)
mutualisms<-mutualisms %>% filter(Phy %in% traits$species)
duplicated(mutualisms$Phy)
mutualisms<-mutualisms[-c(67), ]

master<-left_join(traits, mutualisms, join_by(species==Phy))


legumes <- comparative.data(phy = tree, data = master, 
                         names.col = species, vcv = TRUE, 
                         na.omit = FALSE, warn.dropped = TRUE, force.root = TRUE)

legumes$dropped$tips

legumes$dropped$unmatched.rows

# Plot data to check if it is normal or not




# Try out a PGLS model

model.pgls <- pgls(precip_range ~ fixer, 
                   data = legumes, lambda = "ML")
