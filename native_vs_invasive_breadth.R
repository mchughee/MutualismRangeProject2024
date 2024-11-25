# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)


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

summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# drop species with less than 25 occurrences
# Yes, I did this for PGLS and now I'm doing it for separate native
# and invasive ranges, but I think I need at least 25 occurrences for
# both native and invasive

summary_df<-summary_df %>% filter(n>=25)

## separate by introduced status
native_ranges<-summary_df %>% subset(intrdcd=="0")
intro_ranges<-summary_df %>% subset(intrdcd=="1")

# remove extra species that only have native ranges and no introduced range
native_thin_range<-native_ranges %>% filter(species %in% intro_ranges$species)

# why are there more species in the intro than native now?
differences<-setdiff(intro_ranges$species, native_thin_range$species)

# remove those pesky species and look into them later
intro_ranges <- intro_ranges[!intro_ranges$species %in% differences,]

# add "intro" to all introduced column names
colnames(intro_ranges) <- paste0('intro_', colnames(intro_ranges))
intro_ranges$species<-intro_ranges$intro_species

# merge dataframes by species

all_ranges<-left_join(intro_ranges, native_thin_range, join_by(species==species), multiple="any")

# calculate percent of native breadth invasive breadth is for every species
all_ranges$per_precip_breath<-(all_ranges$intro_precip_range/all_ranges$precip_range*100)
all_ranges$per_temp_breath<-(all_ranges$intro_temp_range/all_ranges$temp_range*100)
all_ranges$per_nitro_breath<-(all_ranges$intro_nitro_range/all_ranges$nitro_range*100)

mean(all_ranges$per_precip_breath)
mean(all_ranges$per_temp_breath)
mean(all_ranges$per_nitro_breath)
# are mutualisms affecting the extent to which this is happening?

# EFN
