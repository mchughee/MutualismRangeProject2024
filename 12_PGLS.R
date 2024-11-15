# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)


# Removing these weird species with 0-little niche breadth at all, and writing 
# it into a new file that I can read in at any time (very exciting stuff):
#data_2<-data_1 %>% filter(n>=25)
#write.csv(data_2, "pgls_final_data.csv")
# we will read this in next

# Read back in PGLS dataframe
data<-read.csv("pgls_final_data.csv")


# Bring in tree
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")


# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, dropped_species)


# make rows in data match rows in tree
data_1 <- data[match(tree_pruned$tip.label,data$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(data_1$temp_range)

hist(log(data_1$temp_range))

hist(scale(data_1$temp_range))

data_1$log_temp_range<-(log(data_1$temp_range))
data_1$scale_temp_range<-(scale(data_1$temp_range))

# precip data
hist(data_1$precip_range)
hist(log(data_1$precip_range))
data_1$log_precip_range<-(log(data_1$precip_range))


# distribution of nitrogen
hist(data_1$nitro_range)
hist(log(data_1$nitro_range))
min(data_1$nitro_range)
data_1$log_nitro_range<-(log(data_1$nitro_range))


### Looking at predictor variables:
# First, number of human uses
hist(data_1$uses_num_uses)
hist(log(data_1$uses_num_uses))
hist(scale(data_1$uses_num_uses))

# median latitude
hist(scale(data_1$median_lat))

# number of occurrences
hist(data_1$n)
hist(log(data_1$n))
data_1$log_n<-log(data_1$n)

# Let's run a gls model for precip range

# First check that the residuals do not, in fact, have equal variance

precip_range <- gls(log_precip_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

plot(precip_range, resid(., type = "p") ~ fitted(.) | species, abline = 0)



# pgls for temp range

temp_range <- gls(temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + log_n + median_lat,
                  data=data_1, 
                  correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

plot(temp_range)

# pgls for temp range

nitro_range <- gls(log_nitro_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,

                   data=data_1, 

                   correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)






### Running PGLS on maxquant data

# First check that the residuals do not, in fact, have equal variance

hist(log(data_1$precip_maxquant))

precip_maxquant <- gls(log(precip_maxquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_maxquant)

plot(precip_maxquant)

hist(residuals(precip_maxquant))



# pgls for temp range
data_1$scale_tempmax<-scale(data_1$temp_maxquant, scale=TRUE)

temp_maxquant <- gls(scale_tempmax ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + log_n + median_lat,
                  data=data_1, 
                  correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_maxquant)

hist(residuals(temp_maxquant))

qqnorm(temp_maxquant, abline = c(0,1))

plot(temp_maxquant)

# pgls for nitro range
hist(log(data_1$nitro_maxquant))

nitro_maxquant <- gls(log(nitro_maxquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                   
                   data=data_1, 
                   
                   correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_maxquant)

plot(nitro_maxquant)

hist(residuals(nitro_maxquant))

qqnorm(nitro_maxquant, abline = c(0,1))




### Running PGLS on minquant data

# First check that the residuals do not, in fact, have equal variance

hist(log(data_1$precip_minquant))

precip_minquant <- gls(log(precip_minquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                       data=data_1, 
                       correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_minquant)

plot(precip_minquant)

hist(residuals(precip_minquant))

qqnorm(precip_minquant, abline = c(0,1))



# pgls for temp minquant
# this is where I left off
hist(data_1$temp_minquant)

temp_minquant <- gls(temp_minquant ~ EFN + Domatia + fixer + woody + uses_num_uses
                     + annual + log_n + median_lat,
                     data=data_1, 
                     correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_maxquant)

hist(residuals(temp_maxquant))

qqnorm(temp_maxquant, abline = c(0,1))

plot(temp_maxquant)

# pgls for temp range
hist(log(data_1$nitro_maxquant))

nitro_maxquant <- gls(log(nitro_maxquant) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                      
                      data=data_1, 
                      
                      correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_maxquant)

plot(nitro_maxquant)

hist(residuals(nitro_maxquant))

qqnorm(nitro_maxquant, abline = c(0,1))
