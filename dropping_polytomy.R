### Attempting to trim phylogeny

library(ape)
#install.packages("BiocManager")
#BiocManager::install("ggtree")
library(ggtree)
library(tidyverse)
library(tidytree)

# Bring in tree
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")

plot(mytree)

# get a cute tibble of the tree to figure out which specices
# form this giant polytomy
x<-as_tibble(mytree)

# We can see that the polytomy is comprised of the first 139 rows
# of the tibble, and every species in the polytomy has branch
# length of 84.763337
species_to_drop<-subset(x, branch.length==84.763337)

# we're going to save the polytomy species as a nice little csv
# to make them easy to drop from the species dataset
write.csv(species_to_drop, "list_of_species_in_polytomy_jan2025.csv")

# filter out species with a branch length that is 84.763337

dropped<-drop.tip(mytree, 1:139)

check<-as_tibble(dropped)

plot(dropped)

write.tree(dropped, "dropped_polytomy_polytomy_removed.tre")
