# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(emmeans)
library(ghibli)
library(ggplot2)

# Removing these weird species with 0-little niche breadth at all, and writing 
# it into a new file that I can read in at any time (very exciting stuff):
#data_2<-data_1 %>% filter(n>=25)
#write.csv(data_2, "pgls_final_data.csv")
# we will read this in next

# Read back in PGLS dataframe
data<-read.csv("pgls_final_data.csv")
#data$mutualism<-ifelse(data$EFN==1 | data$Domatia==1 | data$fixer==1, "1", "0")


# Bring in tree-- this tree has the polytomy removed,
# so we also need to trim down the dataset to only include
# species in the tree!
mytree<-read.tree("polytomy_removed.tre")

# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, dropped_species)

# here, we'll trim down
data1<-filter(data, data$species %in% tree_pruned$tip.label)
# don't worry about the fact that it's 133 species dropped, not 139
# when dropping species without occurrences, I think we ended up dropping
# some species in the polytomy


# make rows in data match rows in tree
data_1 <- data1[match(tree_pruned$tip.label,data1$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(data_1$temp_range)
hist(log(data_1$temp_range))


# precip data
hist(data_1$precip_range)
hist(log(data_1$precip_range))


# distribution of nitrogen
hist(data_1$nitro_range)
hist(log(data_1$nitro_range))
min(data_1$nitro_range)



# make sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

# First check that the residuals do not, in fact, have equal variance

precip_range <- gls(log(precip_range) ~ EFN + Domatia + fixer + woody
                    + uses_num_uses + annual + n + poly(abs_med_lat, 2)+EFN*abs_med_lat+
                      Domatia*abs_med_lat+fixer*abs_med_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))



# pgls for temp range

temp_range <- gls(temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + n + abs_med_lat+EFN*abs_med_lat+
                    Domatia*abs_med_lat+fixer*abs_med_lat,
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)



#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat
                   +EFN*abs_med_lat+Domatia*abs_med_lat+fixer*abs_med_lat,

                   data=data_1, 

                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))


### Running PGLS models, but just using mutualism as a factor

precip_range_m <- gls(log(precip_range) ~ mutualism + woody + uses_num_uses + annual + n + abs_med_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range_m)

plot(precip_range_m)

qqnorm(precip_range_m, abline = c(0,1))

hist(residuals(precip_range_m))


temp_range_m <- gls(temp_range ~ mutualism + woody + uses_num_uses
                  + annual + n + abs_med_lat,
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range_m)

qqnorm(temp_range_m, abline = c(0,1))

hist(residuals(temp_range_m))

plot(temp_range_m)


nitro_range_m <- gls(log(nitro_range) ~ mutualism + woody + uses_num_uses + annual + n + abs_med_lat,
                   
                   data=data_1, 
                   
                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range_m)

plot(nitro_range_m)

hist(residuals(nitro_range_m))




