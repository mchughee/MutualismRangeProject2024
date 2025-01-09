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
x<-as.tibble(mytree)
subset(x, branch.length==84.763337)

# filter out species with a branch length that is 84.763337
x1<-filter(x, branch.length!= 84.763337)

polytomy_dropped<-as.phylo(x1, use.labels=TRUE)

polytomy_dropped

plot(polytomy_dropped)

dropped<-drop.tip(mytree, 1:139)

dropped

plot(dropped)

write.tree(dropped, "droppity_drop_drop.tre")
