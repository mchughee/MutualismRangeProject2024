# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ghibli)
library(ggplot2)
library(cowplot)
#install.packages("ggeffects")
library(ggeffects)

# Removing these weird species with 0-little niche breadth at all, and writing 
# it into a new file that I can read in at any time (very exciting stuff):
#data_2<-data_1 %>% filter(n>=25)
#write.csv(data_2, "pgls_final_data.csv")
# we will read this in next

# Read back in PGLS dataframe
data<-read.csv("pgls_polydropped_final.csv")
#data$mutualism<-ifelse(data$EFN==1 | data$Domatia==1 | data$fixer==1, "1", "0")


# Bring in tree-- this tree has the polytomy removed,
# so we also need to trim down the dataset to only include
# species in the tree!
mytree<-read.tree("polytomy_removed.tre")


diff <- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, diff)

# make rows in data match rows in tree
data_1 <- data[match(tree_pruned$tip.label,data$species),]

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

######################################
## precip first

# I took n out to try without it!!!! But please know that it should be put back
# in!
precip_range <- gls(log(precip_range) ~ EFN + fixer + woody
                    + uses_num_uses + annual + poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                      fixer*poly(median_lat, 2),
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))

### Save as RDS file

write_rds(precip_range, "precip_niche_breadth.rds")
precip_range<-readRDS("precip_niche_breadth.rds")

##################################

### Pull predicted means for EFN

EFN_precip_means<-ggpredict(precip_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_precip_means)


### save df
write.csv(EFN_precip_means, "precip_EFN_breadth_means.csv")


###########################

### Pull predicted means for fixers

fix_precip_means<-ggpredict(precip_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_precip_means)

### save df
write.csv(fix_precip_means, "precip_fix_breadth_means.csv")

#############################################
# pgls for temp range

temp_range <- gls(temp_range ~ EFN + fixer + woody + uses_num_uses
                  + annual + poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                    fixer*poly(median_lat, 2),
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)

### Save as RDS file

write_rds(temp_range, "temp_niche_breadth.rds")

### Pull predicted means for EFN

EFN_temp_means<-ggpredict(temp_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_temp_means)

### save df
write.csv(EFN_temp_means, "temp_EFN_breadth_means.csv")

################################################################################
### Pull predicted means for fixers

fix_temp_means<-ggpredict(temp_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_temp_means)

### save df
write.csv(fix_temp_means, "temp_fix_breadth_means.csv")



##########################################################################
#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody + uses_num_uses
                   + annual + n + poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                     fixer*poly(median_lat, 2),

                   data=data_1, 

                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))

### Pull predicted means for EFN

EFN_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "EFN [all]"), type="fixed")
plot(EFN_nitro_means)

### save df
write.csv(EFN_nitro_means, "nitro_EFN_breadth_means.csv")


### Pull predicted means for fixers

fix_nitro_means<-ggpredict(nitro_range, terms=c("median_lat", "fixer [all]"), type="fixed")
plot(fix_nitro_means)

### save df
write.csv(fix_nitro_means, "nitro_fix_breadth_means.csv")


