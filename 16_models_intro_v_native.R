# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ghibli)
library(nlme)

# Read in my data
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
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

## pare down native ranges to just species that have an introduced range
native_ranges<-native_ranges %>% filter(species %in% intro_ranges$species)

# and ughhhhh remove species that "have no native range" (polygon weirdness)
intro_ranges<-intro_ranges %>% filter(species %in% native_ranges$species)

setdiff(native_ranges$species, intro_ranges$species)

# okay, they now both have 294 of the same species:^)

# Okay bring in traits (what species have EFN, rhizobia, etc.)
traits<-read.csv("legume_range_traits.csv")

traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

# combine native and trait data to make df that we can use for analysis
traits_native<-traits %>% filter(species %in% native_ranges$species)

# combine species traits and species niche info
native_data_traits<-left_join(native_ranges, traits_native, join_by(species==species), multiple="any")

n_distinct(native_data_traits$species)

# add the term "nat" to every column in this df so that when we merge this 
# with the df for all species, we can tell what columns represent native range data

colnames(native_data_traits) <- paste0('nat_', colnames(native_data_traits))

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


# filter this dataset by the species in native_data_traits

all_intro<-all_df %>% filter(all_df$species %in% native_data_traits$nat_species)

# get measures of niche for native+invasive (i.e., total niche)
all_intro$precip_range<-all_intro$precip_maxquant-all_intro$precip_minquant
all_intro$temp_range<-all_intro$temp_maxquant-all_intro$temp_minquant
all_intro$nitro_range<-all_intro$nitro_maxquant-all_intro$nitro_minquant

# now we have two dataframes: one has the native niches + their traits
# the other has niches for all species
# We want to find introduced niche by doing niche(all)-niche(native)

# to get this measure, we need to recombine the native and all data,
# and subtract the native niche from combined/all niche

intro_niche<-left_join(native_data_traits, all_intro, join_by(nat_species==species), multiple="any")

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
native_data_traits$nat_EFN<-as.factor(native_data_traits$nat_EFN)
native_data_traits$nat_fixer<-as.factor(native_data_traits$nat_fixer)
native_data_traits$nat_Domatia<-as.factor(native_data_traits$nat_Domatia)

intro_niche$nat_EFN<-as.factor(intro_niche$nat_EFN)
intro_niche$nat_fixer<-as.factor(intro_niche$nat_fixer)
intro_niche$nat_Domatia<-as.factor(intro_niche$nat_Domatia)

intro_niche$all_median_lat <- 
  all_df$median_lat[match(intro_niche$nat_species, all_df$species)]


#### Reading in new tree
mytree<-read.tree("polytomy_removed.tre")

# drop tips with species that aren't in the dataset
dropped_species<- setdiff(intro_niche$nat_species, mytree$tip.label)
tree_pruned <- drop.tip(mytree, dropped_species)

# Now vice versa-- since trimming the polytomy, there may be species our dataset
# that are not represented on the tree
intro_niche<-filter(intro_niche, intro_niche$nat_species %in% tree_pruned$tip.label)
# 286 species now-- so eight have been dropped

setdiff(intro_niche$nat_species, tree_pruned$tip.label)
# Queenly! We have the same species in the tree and the data

## Check that numeric values are being read as numeric
intro_niche$nat_woody<-as.numeric(intro_niche$nat_woody)
intro_niche$nat_uses_num_uses<-as.numeric(intro_niche$nat_uses_num_uses)
intro_niche$nat_annual<-as.numeric(intro_niche$nat_annual)

intro_niche$EFN<-as.factor(intro_niche$nat_EFN)
intro_niche$fixer<-as.factor(intro_niche$nat_fixer)
intro_niche$species<-as.factor(intro_niche$nat_species)

### write intro_niche as a csv for figure generation
write.csv(intro_niche, "introduced_ranges_data.csv")


### Run models!! For introduced ranges first
hist(intro_niche$introduced_precip_niche)


precip_range <- gls(introduced_precip_niche ~ nat_EFN + nat_fixer + nat_woody +
                    nat_uses_num_uses + nat_annual + 
                    poly(all_median_lat, 2)+nat_EFN*poly(all_median_lat, 2)+
                    nat_fixer*poly(all_median_lat, 2),
                    data=intro_niche, 
                    correlation=corPagel(0.487, tree_pruned, form=~nat_species, fixed=TRUE),
                    method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))



# pgls for temp range

hist(intro_niche$introduced_temp_niche)


temp_range <- gls(introduced_temp_niche ~ nat_EFN + nat_fixer + nat_woody +
                      nat_uses_num_uses + nat_annual +
                      poly(all_median_lat, 2)+nat_EFN*poly(all_median_lat, 2)+
                      nat_fixer*poly(all_median_lat, 2),
                      data=intro_niche, 
                      correlation=corPagel(0.504, tree_pruned, form=~nat_species, fixed=TRUE), 
                  method="ML")

summary(temp_range)

plot(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))



#### pgls for nitro range

hist(log(intro_niche$introduced_nitro_niche+244))
hist(intro_niche$introduced_nitro_niche)

nitro_range <- gls(introduced_nitro_niche ~ nat_EFN + nat_fixer + nat_woody +
                    nat_uses_num_uses + nat_annual +
                    poly(all_median_lat, 2)+nat_EFN*poly(all_median_lat, 2)+
                    nat_fixer*poly(all_median_lat, 2),
                  data=intro_niche, 
                  correlation=corPagel(0.511, tree_pruned, form=~nat_species, fixed=TRUE),
                  method="ML")

summary(nitro_range)

plot(nitro_range)

qqnorm(nitro_range, abline = c(0,1))

hist(residuals(nitro_range))


###############################################################
### Native ranges next

# drop tips with species that aren't in the dataset
dropped_nat_species<- setdiff(mytree$tip.label, native_data_traits$nat_species)
tree_nat_pruned <- drop.tip(mytree, dropped_nat_species)

# Now vice versa-- since trimming the polytomy, there may be species our dataset
# that are not represented on the tree
setdiff(native_data_traits$nat_species, tree_nat_pruned$tip.label)
native_data_traits<-filter(native_data_traits, native_data_traits$nat_species %in% 
                             tree_nat_pruned$tip.label)


# write csv file with native_data_traits
write.csv(native_data_traits, "native_ranges_data.csv")

# add median latitude for all occurrences to the native data traits df
native_data_traits$all_median_lat <- 
  all_df$median_lat[match(native_data_traits$nat_species, all_df$species)]

### Run models!!
hist(native_data_traits$precip_range)
hist(log(native_data_traits$precip_range))

nat_precip_range <- gls(nat_precip_range ~ nat_EFN + nat_fixer + nat_woody+
                     nat_uses_num_uses + nat_annual + poly(all_median_lat, 2) +
                      nat_EFN*poly(all_median_lat, 2)+nat_fixer*poly(all_median_lat, 2),
                    data=native_data_traits, 
                    correlation=corPagel(0.487, tree_nat_pruned, form=~nat_species, fixed=TRUE),
                    method="ML")


summary(nat_precip_range)

plot(nat_precip_range)

qqnorm(nat_precip_range, abline = c(0,1))

hist(residuals(nat_precip_range))



# pgls for temp range

hist(native_data_traits$temp_range)
hist(log(native_data_traits$temp_range))

nat_temp_range <- gls(nat_temp_range ~ nat_EFN + nat_fixer +nat_woody
                        + nat_uses_num_uses + nat_annual + poly(all_median_lat, 2) +
                        nat_EFN*poly(all_median_lat, 2)+nat_fixer*poly(all_median_lat, 2),
                        data=native_data_traits, 
                        correlation=corPagel(0.504, tree_nat_pruned, form=~nat_species, fixed=TRUE),
                        method="ML")

summary(nat_temp_range)

plot(nat_temp_range)

qqnorm(nat_temp_range, abline = c(0,1))

hist(residuals(nat_temp_range))



#### pgls for nitro range

hist(native_data_traits$nitro_range)
hist(log(native_data_traits$nitro_range))

nat_nitro_range <- gls(nat_nitro_range ~ nat_EFN + nat_fixer + nat_woody
                         + nat_uses_num_uses + nat_annual + poly(all_median_lat, 2) +
                         nat_EFN*poly(all_median_lat, 2)+nat_fixer*poly(all_median_lat, 2),
                         data=native_data_traits, 
                         correlation=corPagel(0.511, tree_nat_pruned, form=~nat_species, fixed=TRUE),
                         method="ML")

summary(nat_nitro_range)

plot(nat_nitro_range)

qqnorm(nat_nitro_range, abline = c(0,1))

hist(residuals(nat_nitro_range))


#######################################################################

# let's save our dataframes for future use!
write.csv(intro_niche, "introduced_niche_analysis_df.csv")
write.csv(native_data_traits, "native_niche_analysis_df.csv")



