# PGLS for max and min

# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(emmeans)
library(ghibli)
library(ggplot2)


# Read back in PGLS dataframe
data<-read.csv("pgls_final_data.csv")


# Bring in tree
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



### Running PGLS on maxquant data

hist(data_1$precip_maxquant)
hist(log(data_1$precip_maxquant))

precip_maxquant <- gls(log(precip_maxquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_maxquant)

plot(precip_maxquant)

hist(residuals(precip_maxquant))

qqnorm(precip_maxquant, abline = c(0,1))

# pgls for temp maxquant
data_1$scale_tempmax<-scale(data_1$temp_maxquant, scale=TRUE)
hist(data_1$temp_maxquant)
hist(sqrt(data_1$temp_maxquant))


temp_maxquant <- gls(temp_maxquant ~ EFN + Domatia + fixer + woody + uses_num_uses
                     + annual + n + abs_med_lat,
                     data=data_1, 
                     correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_maxquant)

hist(residuals(temp_maxquant))

qqnorm(temp_maxquant, abline = c(0,1))

plot(temp_maxquant)

# pgls for nitro range
hist(log(data_1$nitro_maxquant))

nitro_maxquant <- gls(log(nitro_maxquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_maxquant)

plot(nitro_maxquant)

hist(residuals(nitro_maxquant))

qqnorm(nitro_maxquant, abline = c(0,1))




### Running PGLS on minquant data

# First check that the residuals do not, in fact, have equal variance

hist(log(data_1$precip_minquant))
hist(data_1$precip_minquant)

precip_minquant <- gls(log(precip_minquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_minquant)

plot(precip_minquant)

hist(residuals(precip_minquant))

qqnorm(precip_minquant, abline = c(0,1))



# pgls for temp minquant
# this is where I left off
hist(data_1$temp_minquant)
hist(log(data_1$temp_minquant+273.15))
hist(sqrt(data_1$temp_minquant+273.15))
hist(scale(data_1$temp_minquant, scale=TRUE))


shapiro.test(data_1$temp_minquant)

temp_minquant <- gls(temp_minquant ~ EFN + Domatia + fixer + woody + uses_num_uses
                     + annual + n + abs_med_lat,
                     data=data_1, 
                     correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_minquant)

hist(residuals(temp_minquant))

qqnorm(temp_minquant, abline = c(0,1))

plot(temp_minquant)

# pgls for nitro range
hist(data_1$nitro_minquant)
hist(log(data_1$nitro_minquant))
hist(sqrt(data_1$nitro_minquant))

nitro_minquant <- gls(log(nitro_minquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_minquant)

plot(nitro_minquant)

hist(residuals(nitro_minquant))

qqnorm(nitro_minquant, abline = c(0,1))
