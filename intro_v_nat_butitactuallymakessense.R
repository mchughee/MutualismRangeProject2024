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
native_data_traits<-left_join(native_ranges, traits_native, 
                              join_by(species==species), multiple="any")

n_distinct(native_data_traits$species)

# combine intro and trait data to make df that we can use for analysis
traits_intro<-traits %>% filter(species %in% intro_ranges$species)

# combine species traits and species niche info
intro_data_traits<-left_join(intro_ranges, traits_intro, 
                             join_by(species==species), multiple="any")

n_distinct(intro_data_traits$species)

#### Reading in new tree
mytree<-read.tree("polytomy_removed.tre")

# drop tips with species that aren't in the dataset
dropped_species<- setdiff(mytree$tip.label, intro_data_traits$species)
tree_pruned <- drop.tip(mytree, dropped_species)

# Now vice versa-- since trimming the polytomy, there may be species our dataset
# that are not represented on the tree
intro_niche<-filter(intro_data_traits, intro_data_traits$species %in% tree_pruned$tip.label)
nat_niche<-filter(native_data_traits, native_data_traits$species %in% tree_pruned$tip.label)
# 286 species now-- so eight have been dropped


native_data_traits$EFN<-as.factor(native_data_traits$EFN)
native_data_traits$fixer<-as.factor(native_data_traits$fixer)

intro_niche$EFN<-as.factor(intro_niche$EFN)
intro_niche$fixer<-as.factor(intro_niche$fixer)

################################################################################
### Run models!! For introduced ranges first
hist(log(intro_niche$precip_range))


precip_range <- gls(log(precip_range) ~ EFN + fixer + woody +
                      uses_num_uses + annual + 
                      poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                      fixer*poly(median_lat, 2),
                    data=intro_niche, 
                    correlation=corPagel(0.445, tree_pruned, form=~species, fixed=TRUE),
                    method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))



# pgls for temp range

hist(intro_niche$temp_range)


temp_range <- gls(temp_range ~ EFN + fixer + woody +
                    uses_num_uses + annual +
                    poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                    fixer*poly(median_lat, 2),
                  data=intro_niche, 
                  correlation=corPagel(0.373, tree_pruned, form=~species, fixed=TRUE), 
                  method="ML")

summary(temp_range)

plot(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))



#### pgls for nitro range

hist(log(intro_niche$nitro_range))

nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody +
                     uses_num_uses + annual +
                     poly(median_lat, 2)+EFN*poly(median_lat, 2)+
                     fixer*poly(median_lat, 2),
                   data=intro_niche, 
                   correlation=corPagel(0.330, tree_pruned, form=~species, fixed=TRUE),
                   method="ML")

summary(nitro_range)

plot(nitro_range)

qqnorm(nitro_range, abline = c(0,1))

hist(residuals(nitro_range))


###############################################################
### Native ranges next

### Run models!!
hist(nat_niche$precip_range)
hist(log(nat_niche$precip_range))

nat_precip_range <- gls(log(precip_range) ~ EFN + fixer + woody+
                          uses_num_uses + annual + poly(median_lat, 2) +
                          EFN*poly(median_lat, 2)+fixer*poly(median_lat, 2),
                        data=nat_niche, 
                        correlation=corPagel(0.445, tree_pruned, form=~species, fixed=TRUE),
                        method="ML")


summary(nat_precip_range)

plot(nat_precip_range)

qqnorm(nat_precip_range, abline = c(0,1))

hist(residuals(nat_precip_range))



# pgls for temp range

hist(nat_niche$temp_range)
hist(log(nat_niche$temp_range))

nat_temp_range <- gls(temp_range ~ EFN + fixer + woody
                      + uses_num_uses + annual + poly(median_lat, 2) +
                        EFN*poly(median_lat, 2)+ fixer*poly(median_lat, 2),
                      data=nat_niche, 
                      correlation=corPagel(0.373, tree_pruned, form=~species, fixed=TRUE),
                      method="ML")

summary(nat_temp_range)

plot(nat_temp_range)

qqnorm(nat_temp_range, abline = c(0,1))

hist(residuals(nat_temp_range))



#### pgls for nitro range

hist(nat_niche$nitro_range)
hist(log(nat_niche$nitro_range))

nat_nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody
                       + uses_num_uses + annual + poly(median_lat, 2) +
                         EFN*poly(median_lat, 2)+ fixer*poly(median_lat, 2),
                       data=nat_niche, 
                       correlation=corPagel(0.330, tree_pruned, form=~species, fixed=TRUE),
                       method="ML")

summary(nat_nitro_range)

plot(nat_nitro_range)

qqnorm(nat_nitro_range, abline = c(0,1))

hist(residuals(nat_nitro_range))

################################################################################
### Running on total

total_df<-points_1 %>% 
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

# slim down to species in native and intro ranges
total_niche<-filter(total_df, total_df$species %in% nat_niche$species)
total_traits<-left_join(total_niche, traits, 
                             join_by(species==species), multiple="any")


# calculate niche breadth
total_traits$precip_range<-total_traits$precip_maxquant-total_traits$precip_minquant
total_traits$temp_range<-total_traits$temp_maxquant-total_traits$temp_minquant
total_traits$nitro_range<-total_traits$nitro_maxquant-total_traits$nitro_minquant

# make sure that R knows our categorical variables are factors
total_traits$EFN<-as.factor(total_traits$EFN)
total_traits$fixer<-as.factor(total_traits$fixer)

### Run models!!
hist(total_traits$precip_range)
hist(log(total_traits$precip_range))

total_precip_range <- gls(log(precip_range) ~ EFN + fixer + woody+
                          uses_num_uses + annual + poly(median_lat, 2) +
                          EFN*poly(median_lat, 2)+fixer*poly(median_lat, 2),
                        data=total_traits, 
                        correlation=corPagel(0.445, tree_pruned, form=~species, fixed=TRUE),
                        method="ML")


summary(total_precip_range)

plot(total_precip_range)

qqnorm(total_precip_range, abline = c(0,1))

hist(residuals(total_precip_range))



# pgls for temp range

hist(total_traits$temp_range)
hist(log(total_traits$temp_range))

total_temp_range <- gls(temp_range ~ EFN + fixer + woody
                      + uses_num_uses + annual + poly(median_lat, 2) +
                        EFN*poly(median_lat, 2)+ fixer*poly(median_lat, 2),
                      data=total_traits, 
                      correlation=corPagel(0.373, tree_pruned, form=~species, fixed=TRUE),
                      method="ML")

summary(total_temp_range)

plot(total_temp_range)

qqnorm(total_temp_range, abline = c(0,1))

hist(residuals(total_temp_range))



#### pgls for nitro range

hist(total_traits$nitro_range)
hist(log(total_traits$nitro_range))

total_nitro_range <- gls(log(nitro_range) ~ EFN + fixer + woody
                       + uses_num_uses + annual + poly(median_lat, 2) +
                         EFN*poly(median_lat, 2)+ fixer*poly(median_lat, 2),
                       data=total_traits, 
                       correlation=corPagel(0.330, tree_pruned, form=~species, fixed=TRUE),
                       method="ML")

summary(total_nitro_range)

plot(total_nitro_range)

qqnorm(total_nitro_range, abline = c(0,1))

hist(residuals(total_nitro_range))

###############################################################################
### saving files to make figure down the road

write.csv(nat_niche, "native_ranges_data.csv")
write.csv(intro_niche, "introduced_ranges_data.csv")
write.csv(total_traits, "total_ranges_data.csv")
