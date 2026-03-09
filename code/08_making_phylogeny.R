# Trying out V.phylomaker

library(devtools)
library(BiocManager)
# devtools::install_github("jinyizju/V.PhyloMaker")
# BiocManager::install("ggtree")
library("V.PhyloMaker")
library(tidyverse)
library(phytools)
library(ape)
library(tidytree)
library(ggtree)


# read in dataset
dat <- read_csv("data/pgls_species_data.csv") %>% 
  separate(species, into = c("genus", NA), remove = FALSE) %>% 
  mutate(family = "Fabaceae")

dat$species <- as.factor(dat$species)

sp_list <- dat %>% 
  group_by(species) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  reframe(species, genus, family)

sp_list$genus <- as.factor(sp_list$genus)
sp_list$family <- as.factor(sp_list$family)

sp_list$species.relative <- NA
sp_list$genus.relative <- NA

summary(sp_list)

tree <- phylo.maker(sp.list = sp_list,
                    tree = GBOTB.extended,
                    nodes = nodes.info.1,
                    scenarios = "S1")

plot(tree$scenario.1)

write.tree(tree$scenario.1, "phylogeny/phylogeny_2667_buildnodes1.tre")


# Trim phylogeny

mytree <- read.tree("phylogeny/phylogeny_2667_buildnodes1.tre")

p <- ggtree(mytree, alpha = 0.1, layout = "circular") + 
  geom_nodelab(aes(label = node))
p <- p + geom_tiplab()
p

# get a cute tibble of the tree to figure out which species form this giant polytomy
x <- as_tibble(mytree)

# We can see that the polytomy is comprised of the first 139 rows
# of the tibble, and every species in the polytomy has branch
# length of 84.763337
species_to_drop <- subset(x, branch.length == 84.763337)

# we're going to save the polytomy species as a nice little csv
# to make them easy to reference in the future
write.csv(species_to_drop, "data/list_of_species_in_polytomy.csv")

# filter out species with a branch length that is 84.763337

dropped <- drop.tip(mytree, species_to_drop$label)

# Check that it worked by looking at output as a tibble
check <- as_tibble(dropped)

# check that number of species lines up with what we would expect
dropped
# a visual check
plot(dropped)

# write tree
write.tree(dropped, "phylogeny/phylogeny_polytomy_removed.tre")

# remove polytomy species from pgls final dataset
pgls <- read.csv("data/pgls_species_data.csv")

setdiff(pgls$species, dropped$tip.label)

pgls_new <- filter(pgls, pgls$species %in% dropped$tip.label)

# write it as a csv for future use!

write_csv(pgls_new, "data/pgls_species_data_poly_dropped.csv")

setdiff(pgls$species, mytree$tip.label)


