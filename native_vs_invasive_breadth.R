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

# remove extra species that only have native ranges and no introduced range
native_thin_ranges<-native_ranges %>% filter(species %in% intro_ranges$species)

# why are there more species in the intro than native now?
differences<-setdiff(intro_ranges$species, native_thin_ranges$species)

# remove those pesky species and look into them later
intro_ranges <- intro_ranges[!intro_ranges$species %in% differences,]

# add "intro" to all introduced column names
colnames(intro_ranges) <- paste0('intro_', colnames(intro_ranges))
intro_ranges$species<-intro_ranges$intro_species

# merge dataframes by species

all_ranges<-left_join(intro_ranges, native_thin_ranges, join_by(species==species), multiple="any")

# percent invasive over native
all_ranges$per_precip_breadth<-(all_ranges$intro_precip_range/all_ranges$precip_range*100)
all_ranges$per_temp_breadth<-(all_ranges$intro_temp_range/all_ranges$temp_range*100)
all_ranges$per_nitro_breadth<-(all_ranges$intro_nitro_range/all_ranges$nitro_range*100)

# To do further analysis, let's smoosh this dataframe with the traits dataframe
traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

traits<-traits %>% filter(species %in% native_thin_ranges$species)

master_legume<-left_join(all_ranges, traits, join_by(species==species), multiple="any")



# t-test for native vs invasive ranges

hist(log(master_legume$intro_precip_range))
hist(log(master_legume$precip_range))
t.test(log(master_legume$intro_precip_range), log(master_legume$precip_range), paired=TRUE)

hist(master_legume$intro_temp_range)
hist(master_legume$temp_range)
t.test(master_legume$intro_temp_range, master_legume$temp_range, paired=TRUE)

hist(log(master_legume$intro_nitro_range))
hist(log(master_legume$nitro_range))
t.test(master_legume$intro_nitro_range, master_legume$nitro_range, paired=TRUE)


### Read in phylogenetic tree for further analysis

mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")


# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, master_legume$species)
tree_pruned <- drop.tip(mytree, dropped_species)

# make rows in data match rows in tree
master_legume <- master_legume[match(tree_pruned$tip.label,master_legume$species),]

# add up intro and regular n to get old n
master_legume$n_total<-master_legume$intro_n+master_legume$n

### Set factors as such
master_legume$EFN<-as.factor(master_legume$EFN)
master_legume$Domatia<-as.factor(master_legume$Domatia)
master_legume$fixer<-as.factor(master_legume$fixer)


#### linear model for precip
## Not including Domatia in analysis, as no species *have* them

hist(master_legume$per_precip_breadth)
hist(log(master_legume$per_precip_breadth))

precip_per <- lm(log(per_precip_breadth) ~ EFN + fixer + woody + uses_num_uses + annual + n_total + abs_med_lat,
                    data=master_legume)
summary(precip_per)
hist(residuals(precip_per))
plot(precip_per)


#### linear model for temp

hist(master_legume$per_temp_breadth)
hist(log(master_legume$per_temp_breadth))

temp_per <- lm(log(per_temp_breadth) ~ EFN + fixer + woody + uses_num_uses + annual + n_total + abs_med_lat,
                 data=master_legume)
summary(temp_per)
hist(residuals(temp_per))
plot(temp_per)


#### linear model for nitro

hist(master_legume$per_nitro_breadth)
hist(log(master_legume$per_nitro_breadth))

nitro_per <- lm(log(per_nitro_breadth) ~ EFN + fixer + woody + uses_num_uses + annual + n_total + abs_med_lat,
               data=master_legume)
summary(nitro_per)
hist(residuals(nitro_per))
plot(nitro_per)





