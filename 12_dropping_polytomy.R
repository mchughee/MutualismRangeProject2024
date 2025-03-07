### Attempting to trim phylogeny

library(ape)
library(tidyverse)
library(tidytree)
library(ggtree)

# Bring in tree
mytree<-read.tree("phylogeny_2771_buildnodes1.tre")

p <- ggtree(mytree, alpha=0.1, layout="circular")+geom_nodelab(aes(label = node))
p <- p %>% ggtree::collapse(node=2775)
p <- p %>% ggtree::collapse(node=4185)
p <- p %>% ggtree::collapse(node=2773)
p <- p + geom_tiplab()
p

# get a cute tibble of the tree to figure out which species
# form this giant polytomy
x<-as_tibble(mytree)

# We can see that the polytomy is comprised of the first 139 rows
# of the tibble, and every species in the polytomy has branch
# length of 84.763337
species_to_drop<-subset(x, branch.length==84.763337)

# we're going to save the polytomy species as a nice little csv
# to make them easy to reference in the future
write.csv(species_to_drop, "list_of_species_in_polytomy_jan2025.csv")

# filter out species with a branch length that is 84.763337

dropped<-drop.tip(mytree, species_to_drop$label)

# Check that it worked by looking at output as a tibble
check<-as_tibble(dropped)

# check that number of species lines up with what we would expect
dropped
# a visual check
plot(dropped)

# write tree
write.tree(dropped, "polytomy_removed.tre")

# remove polytomy species from pgls final dataset
pgls<-read.csv("pgls_final_data.csv")

setdiff(pgls$species, dropped$tip.label)



pgls_new<-filter(pgls, pgls$species %in% dropped$tip.label)

# write it as a csv for future use!

write.csv(pgls_new, "pgls_polydropped_final.csv")

setdiff(pgls$species, mytree$tip.label)
