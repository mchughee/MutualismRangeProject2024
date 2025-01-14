# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ghibli)
library(lme4)

# Read in my data
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
str(points)
head(points)
# The "warning" message here is about the dateIdentified not being in the 
# correct format, which like, I don't care about, so I'm ignoring
points$species<-as.factor(points$species)
levels(unique(points$species))
# 2771 species in dataset


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)


# Group by species and invasive status (0 or 1) and get summarizing!
# we have several measures of niche, latitude, etc.
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

## 3319 "species" observations-- this is okay! We are grouping by invasive and
# native, so we can expect not quite double the "species" we had in points_1

# calculate niche breadth
summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# calculate absolute median latitude
summary_df$abs_med_lat<-abs(summary_df$median_lat)

# drop species with less than 25 occurrences
# Yes, I did this for PGLS and now I'm doing it for separate native
# and invasive ranges, but I think I need at least 25 occurrences for
# both native and invasive!

summary_df<-summary_df %>% filter(n>=25)
n_distinct(unique(summary_df$species))
# There are now 2656 species in the dataset


## separate the summary_df into native and invasive range dataframes
native_ranges<-summary_df %>% subset(intrdcd=="0")
intro_ranges<-summary_df %>% subset(intrdcd=="1")




# Okay bring in traits (what species have EFN, rhizobia, etc.)
traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

# combine native and trait data to make df that we can use for analysis
traits_native<-traits %>% filter(species %in% native_ranges$species)

native_data_traits<-left_join(native_ranges, traits_native, join_by(species==species), multiple="any")

# make dataset with invasive niche breadth-- i.e., total minus native 
# frig, I really hate this dumb data wrangling! But that's okay
# Filter native traits df by species in invasive df
traits_native_intro<-native_data_traits %>% filter(species %in% intro_ranges$species)
# same prob as before-- there are eleven species in the dataset that 
# have an invasive range but not a native one-- I expect this is due to
# polygon weirdness (gbif occurrences not matching up with inv/nat pow polygons)

# add the term "nat" to every column in this df so that when we merge this 
# with the df for all species, we can tell what columns represent native range data

colnames(traits_native_intro) <- paste0('nat_', colnames(traits_native_intro))

# Next, make a new summary df grouped only by species, not by intrdcd status

all_df<-points_1 %>% 
  group_by(species) %>% 
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


# filter this dataset by the species in traits_native_intro

all_intro<-all_df %>% filter(all_df$species %in% traits_native_intro$nat_species)

# get measures of niche for native+invasive
all_intro$precip_range<-all_intro$precip_maxquant-all_intro$precip_minquant
all_intro$temp_range<-all_intro$temp_maxquant-all_intro$temp_minquant
all_intro$nitro_range<-all_intro$nitro_maxquant-all_intro$nitro_minquant

# now we have two dataframes: one has the native niches + their traits
# the other has niches for all species
# We want to find introduced niche by doing niche(all)-niche(native)

# to get this measure, we need to recombine the native and all data,
# and subtract the native niche from combined/all niche

intro_niche<-left_join(traits_native_intro, all_intro, join_by(nat_species==species), multiple="any")

# calculate the introduced part of the niche
intro_niche$introduced_precip_niche<-intro_niche$precip_range-intro_niche$nat_precip_range
intro_niche$introduced_temp_niche<-intro_niche$temp_range-intro_niche$nat_temp_range
intro_niche$introduced_nitro_niche<-intro_niche$nitro_range-intro_niche$nat_nitro_range

# check mean introduced part of niche
mean(intro_niche$introduced_temp_niche)
mean(intro_niche$introduced_precip_niche)
mean(intro_niche$introduced_nitro_niche)


# Make sure that in native and intro datasets, EFN, rhizobia, and fixer are
# being recognized as factors
native_data_traits$EFN<-as.factor(native_data_traits$EFN)
native_data_traits$fixer<-as.factor(native_data_traits$fixer)
native_data_traits$Domatia<-as.factor(native_data_traits$Domatia)

intro_niche$nat_EFN<-as.factor(intro_niche$nat_EFN)
intro_niche$nat_fixer<-as.factor(intro_niche$nat_fixer)
intro_niche$nat_Domatia<-as.factor(intro_niche$nat_Domatia)

#### Reading in new tree
mytree<-read.tree("polytomy_removed.tre")

# drop tips with species that aren't in the dataset
dropped_species<- setdiff(mytree$tip.label, intro_niche$nat_species)
tree_pruned <- drop.tip(mytree, dropped_species)

# Now vice versa-- since trimming the polytomy, there may be species our dataset
# that are not represented on the tree
intro_niche<-filter(intro_niche, intro_niche$nat_species %in% tree_pruned$tip.label)
# 286 species now-- so eight have been dropped

## Check that numeric values are being read as numeric
intro_niche$nat_woody<-as.numeric(intro_niche$nat_woody)
intro_niche$nat_uses_num_uses<-as.numeric(intro_niche$nat_uses_num_uses)
intro_niche$nat_annual<-as.numeric(intro_niche$nat_annual)


## Run linear mixed-effects models! Yay!
hist(intro_niche$introduced_precip_niche)

hist(intro_niche$introduced_temp_niche)
hist(log(intro_niche$introduced_temp_niche+14))


hist(intro_niche$introduced_nitro_niche)
hist(log(intro_niche$introduced_nitro_niche+243))

#### Introduced linear models
### Temp linear model-- introduced
temp_intro <- lm(introduced_temp_niche ~  nat_EFN + nat_fixer 
                   + nat_woody + nat_uses_num_uses + nat_annual + nat_n + 
                     nat_abs_med_lat, intro_niche)

summary(temp_intro)


### Precip linear model-- introduced
precip_intro <- lm(introduced_precip_niche ~  nat_EFN + nat_fixer 
                 + nat_woody + nat_uses_num_uses + nat_annual + nat_n + 
                   nat_abs_med_lat, intro_niche)

summary(precip_intro)

### nitro linear model-- introduced
nitro_intro <- lm(introduced_nitro_niche ~  nat_EFN + nat_fixer 
                   + nat_woody + nat_uses_num_uses + nat_annual + nat_n + 
                     nat_abs_med_lat, intro_niche)

summary(nitro_intro)



##### Linear models for native range
### Temp linear model-- native
temp_native <- lm(temp_range ~  EFN + fixer 
                 + Domatia+woody + uses_num_uses + annual + n + 
                   abs_med_lat, native_data_traits)

summary(temp_native)


### Precip linear model-- introduced
precip_native <- lm(precip_range ~  EFN + fixer 
                  + Domatia+woody + uses_num_uses + annual + n + 
                    abs_med_lat, native_data_traits)

summary(precip_native)

### nitro linear model-- introduced
nitro_native <- lm(nitro_range ~  EFN + fixer 
                    + Domatia+ woody + uses_num_uses + annual + n + 
                      abs_med_lat, native_data_traits)

summary(nitro_native)
