# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ghibli)
library(nlme)

# Read in my data
points<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

# Group by species and invasive status (0 or 1) and get summarizing!
summary_df<-points_1 %>% 
  group_by(species, intrdcd) %>% 
  reframe(n=n(),
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

# calculate niche breadth
summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# calculate absolute median latitude
summary_df$abs_med_lat<-abs(summary_df$median_lat)

# drop species with less than 25 occurrences
# Yes, I did this for PGLS and now I'm doing it for separate native
# and invasive ranges, but I think I need at least 25 occurrences for
# both native and invasive

summary_df<-summary_df %>% filter(n>=25)

## separate by introduced status
native_ranges<-summary_df %>% subset(intrdcd=="0")
intro_ranges<-summary_df %>% subset(intrdcd=="1")


# add "intro" to all introduced column names and make a species column that will
# allow us to bind the total, introduced, and native niche data together
colnames(intro_ranges) <- paste0('intro_', colnames(intro_ranges))
intro_ranges$species<-intro_ranges$intro_species


# Okay bring in traits
traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

# combine native and trait data
traits_native<-traits %>% filter(species %in% native_ranges$species)

native_data<-left_join(native_ranges, traits_native, join_by(species==species), multiple="any")

# combine intro and trait data 
traits_intro<-traits %>% filter(species %in% intro_ranges$species)

intro_data<-left_join(intro_ranges, traits_intro, join_by(species==species), multiple="any")


# Bring in tree
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")


# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, intro_data$species)
tree_pruned_intro <- drop.tip(mytree, dropped_species)


# make rows in data match rows in tree
intro_data1 <- intro_data[match(tree_pruned_intro$tip.label, intro_data$species),]


### Make sure R knows that our factor variables are indeed factors
intro_data1$EFN<-as.factor(intro_data1$EFN)
intro_data1$fixer<-as.factor(intro_data1$fixer)

### First, run PGLS models on invasive range
intro_precip_range <- gls(log(intro_precip_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                    data=intro_data1, 
                    correlation=corPagel(1, tree_pruned_intro, form=~species), method="ML")

intro_temp_range <- gls(log(intro_temp_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                          data=master_legume1, 
                          correlation=corPagel(0.4, tree_pruned, form=~species), method="ML")

intro_nitro_range <- gls(log(intro_nitro_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                        data=master_legume1, 
                        correlation=corPagel(0.4, tree_pruned, form=~species), method="ML")

# Using the original (not pared-down) native range df here. Am attaching the traits df
# here

traits_native<-traits %>% filter(species %in% native_ranges$species)
duplicated(traits_native$species)

# remove that stupid duplicate species that is always in the freaking dataset
# For future reference, it will always be row 64
traits_native<-traits_native[-64, ] 

native_data<-left_join(native_ranges, traits_native, join_by(species==species), multiple="any")

# Reprune tree to the species in this dataset

dropped_species_native<- setdiff(mytree$tip.label, native_data$species)
tree_native <- drop.tip(mytree, dropped_species_native)

native_precip_range <- gls(log(precip_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                          data=native_data, 
                          correlation=corPagel(0.4, tree_native, form=~species), method="ML")

summary(native_precip_range)

native_temp_range <- gls(log(temp_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                        data=native_data, 
                        correlation=corPagel(0.4, tree_native, form=~species), method="ML")

summary(native_temp_range)

native_nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                         data=native_data, 
                         correlation=corPagel(0.4, tree_native, form=~species), method="ML")
summary(native_nitro_range)

# Okay, screw this, I'm just using linear models
# Intro first

intro_precip_range<-lm(log(intro_precip_range) ~ EFN + fixer + woody + uses_num_uses + 
                         annual + n + abs_med_lat, data=master_legume1)
summary(intro_precip_range)

intro_temp_range<-lm(log(intro_temp_range) ~ EFN + fixer + woody + uses_num_uses + 
                         annual + n + abs_med_lat, data=master_legume1)
summary(intro_temp_range)

intro_nitro_range<-lm(log(intro_precip_range) ~ EFN + fixer + woody + uses_num_uses + 
                         annual + n + abs_med_lat, data=master_legume1)
summary(intro_nitro_range)


# Native range points next
nat_precip_range<-lm(log(precip_range) ~ EFN + fixer + woody + uses_num_uses + 
                         annual + n + abs_med_lat, data=native_data)
summary(nat_precip_range)

nat_temp_range<-lm(log(temp_range) ~ EFN + fixer + woody + uses_num_uses + 
                       annual + n + abs_med_lat, data=native_data)
summary(nat_temp_range)

nat_nitro_range<-lm(log(precip_range) ~ EFN + fixer + woody + uses_num_uses + 
                        annual + n + abs_med_lat, data=native_data)
summary(nat_nitro_range)



