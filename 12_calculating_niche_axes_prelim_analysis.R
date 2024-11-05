# Bringing in phylogeny and making new dataframe with various quantifiers of
# niche, plus latitude

library(ape)
library(phytools)
library(nlme)
library(visreg)
library(tidyverse)


# Read in my data
points<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

# check that we still have the same number of species!
points_1$species<-as.factor(points_1$species)
nlevels(points_1$species)
# okay, nice, we have the same number! (the magic number here is 2771)

# Now read in traits

traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy

sum(is.na(traits$fixer))
sum(is.na(traits$annual))

# Make new master dataset with species grouped, not grouped by native or invasive
summary_df_species<-points_1 %>% 
  group_by(species) %>% 
  reframe(
          n = n(),
          precip_maxquant=quantile(precip, 0.95), 
          precip_minquant=quantile(precip, 0.05),
          precip_mean=mean(precip),
          precip_median=median(precip),
          nitro_maxquant=quantile(nitrogen, 0.95),
          nitro_minquant=quantile(nitrogen, 0.05),
          nitro_mean=mean(nitrogen),
          nitro_median=median(nitrogen),
          temp_maxquant=quantile(temp, 0.95),
          temp_minquant=quantile(temp, 0.05),
          temp_mean=mean(temp),
          temp_median=median(temp),
          max_lat=max(Y),
          min_lat=min(Y),
          mean_lat=mean(Y),
          median_lat=median(Y),
          quant95=quantile(Y, 0.95),
          quant005=quantile(Y, 0.05)
  )

# make interquartile ranges for preicpitation and temperature
summary_df_species$precip_range<-summary_df_species$precip_maxquant-summary_df_species$precip_minquant
summary_df_species$temp_range<-summary_df_species$temp_maxquant-summary_df_species$temp_minquant

# make master data set that has niche and mutualism/trait data by doing a left join
# We need to put the _ inbetween the genus and species epithet in Phy first though
traits$Phy<-gsub(" ", "_", traits$Phy)
master_legume_species<-left_join(summary_df_species, traits, join_by(species==Phy))

# drop the row with a duplicate species
duplicated(master_legume_species$species)
master_legume_species <- master_legume_species[-65, ]

write.csv(master_legume_species, "legume_data_for_PGLS.csv")

