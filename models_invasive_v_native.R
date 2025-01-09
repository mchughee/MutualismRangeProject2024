# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ghibli)
library(nlme)

# Read in my data
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
str(points)
head(points)
points$species<-as.factor(points$species)
levels(unique(points$species))


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


# Okay bring in traits (what species have EFN, rhizobia, etc.)
traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

# combine native and trait data
traits_native<-traits %>% filter(species %in% native_ranges$species)

native_data_traits<-left_join(native_ranges, traits_native, join_by(species==species), multiple="any")

# combine intro and trait data 
traits_intro<-traits %>% filter(species %in% intro_ranges$species)

intro_data_traits<-left_join(intro_ranges, traits_intro, join_by(species==species), multiple="any")

# now we have two dataframes: one has the introduced niches of species + their traits,
# and the other has the native niches of species and their traits

# But, we still have some wrangling to do: right now, our introduced dataframe
# has the introduced niche, which includes the existing niche of a species + the 
# unique parts of the introduced range. We only want the unique parts of the niche
# that are only found in introduced ranges!

# to get this measure, we need to recombine the native and introduced data,
# and subtract the native niche from the introduced niche

intro_niche<-left_join(intro_ranges, native_ranges, join_by(intro_species==species), multiple="any")

# remove the species that "don't have a native range" (they obviously do, but I suspect
# between cleaning data and the very small native ranges they have in POW, they don't have
# have any points in their native range


subset(intro_niche, is.na(intrdcd))
# there are eleven species like this! Inc. Lathyrus sativus, senna sophera, zornia diphylla

# drop those species
intro_niche<-intro_niche[!is.na(intro_niche$intrdcd),]

# calculate the introduced part of the niche
intro_niche$introduced_precip_niche<-intro_niche$intro_precip_range-intro_niche$precip_range
intro_niche$introduced_temp_niche<-intro_niche$intro_temp_range-intro_niche$temp_range
intro_niche$introduced_nitro_niche<-intro_niche$intro_nitro_range-intro_niche$nitro_range

# check mean introduced part of niche
mean(intro_niche$introduced_temp_niche)
mean(intro_niche$introduced_precip_niche)
mean(intro_niche$introduced_nitro_niche)

# Just checking that what I've got now aligns with what I've had in the past
intro_niche$temp_percent<-(intro_niche$intro_temp_range/intro_niche$temp_range*100)
intro_niche$temp_percent<-as.numeric(intro_niche$temp_percent)
mean(intro_niche$temp_percent, na.rm=TRUE)

intro_niche$precip_percent<-(intro_niche$intro_precip_range/intro_niche$precip_range*100)
intro_niche$precip_percent<-as.numeric(intro_niche$precip_percent)
mean(intro_niche$precip_percent, na.rm=TRUE)

intro_niche$nitro_percent<-(intro_niche$intro_nitro_range/intro_niche$nitro_range*100)
intro_niche$nitro_percent<-as.numeric(intro_niche$nitro_percent)
mean(intro_niche$nitro_percent, na.rm=TRUE)
# Okay, great, it does!



# drop the stuff we don't need from the dataframe!

intro_niche<-intro_niche %>% select(intro_species, introduced_precip_niche, introduced_temp_niche,
                                    introduced_nitro_niche, intro_abs_med_lat, intro_n)

# combine with traits
traits_new<-traits %>% filter(species %in% intro_niche$intro_species)
intro_niche<-left_join(intro_niche, traits_new, join_by(intro_species==species), multiple="any")


# Make sure that in native and intro datasets, EFN, rhizobia, and fixer are
# being recognized as factors
native_data_traits$EFN<-as.factor(native_data_traits$EFN)
native_data_traits$fixer<-as.factor(native_data_traits$fixer)
native_data_traits$Domatia<-as.factor(native_data_traits$Domatia)

intro_niche$EFN<-as.factor(intro_niche$EFN)
intro_niche$fixer<-as.factor(intro_niche$fixer)
intro_niche$Domatia<-as.factor(intro_niche$Domatia)




# Okay, screw this, I'm just using linear models
# Intro first

intro_precip_range<-lm(introduced_precip_niche ~ EFN + fixer + woody + uses_num_uses + 
                         annual + intro_n + intro_abs_med_lat, data=intro_niche)
summary(intro_precip_range)
hist(resid(intro_precip_range))
plot(intro_precip_range)

intro_temp_range<-lm(introduced_temp_niche ~ EFN + fixer + woody + uses_num_uses + 
                       annual + intro_n + intro_abs_med_lat, data=intro_niche)
summary(intro_temp_range)
hist(resid(intro_temp_range))
plot(intro_temp_range)

intro_nitro_range<-lm(introduced_nitro_niche ~ EFN + fixer + woody + uses_num_uses + 
                        annual + intro_n + intro_abs_med_lat, data=intro_niche)
summary(intro_nitro_range)
hist(resid(intro_nitro_range))
plot(intro_nitro_range)


# Native range points next
nat_precip_range<-lm(log(precip_range) ~ EFN + fixer + Domatia+ woody + uses_num_uses + 
                       annual + n + abs_med_lat, data=native_data_traits)
summary(nat_precip_range)
hist(resid(nat_precip_range))
plot(nat_precip_range)

nat_temp_range<-lm(log(temp_range) ~ EFN + fixer + Domatia + woody + uses_num_uses + 
                     annual + n + abs_med_lat, data=native_data_traits)
summary(nat_temp_range)
hist(resid(nat_temp_range))
plot(nat_temp_range)


nat_nitro_range<-lm(log(nitro_range) ~ EFN + fixer + Domatia + woody + uses_num_uses + 
                      annual + n + abs_med_lat, data=native_data_traits)
summary(nat_nitro_range)
hist(resid(nat_nitro_range))
plot(nat_nitro_range)







