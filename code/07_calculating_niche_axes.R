# Making df with summarized niche breadth measures

library(tidyverse)
library(cowplot)
library(ggplot2)


# Read in occurrences with environmental data etc.
points <- read_csv("data_large/allocc_with_native_status.csv")
n_distinct(points$species)


x = (points %>% filter(species=="Andira_coriacea"))

per_sp_count = points %>% 
  group_by(species) %>% 
  summarize(n = n())

# Drop points that have NA values for the niche axes and the intrdcd status

points_1 <- points %>% 
  drop_na(intrdcd)
n_distinct(points_1$species)
# Not sure if this filter was applied to final dataset

points_2 <- points_1 %>% 
  drop_na(precip) %>% 
  drop_na(temp) %>% 
  drop_na(nitrogen)
n_distinct(points_2$species)

# Check what percent of observations are retained on a species-by-species basis
per_sp_count_1 = points_1 %>% 
  group_by(species) %>% 
  summarize(n1 = n())

per_sp_count_2 = points_2 %>% 
  group_by(species) %>% 
  summarize(n2 = n())

check = left_join(per_sp_count, per_sp_count_1) %>%
  mutate(pct = n1/n) %>% 
  filter(n1 >= 25)
n_distinct(points$species) - n_distinct(check$species)


check2 = left_join(check, per_sp_count_2) %>% 
  mutate(pct2 = n2/n) %>% 
  filter(n2 >= 25)

n_distinct(check$species) - n_distinct(check2$species)

median(check$pct)
# 96.4
median(check2$pct2)
# 86.7

# make sure biome is a factor
class(points_2$biome)
levels(points_2$biome)
points_2$biome <- as.factor(points_2$biome)
points_2$species <- as.factor(points_2$species)

# let's see what biome looks like. Where are most points found?

levels(points_2$biome)
summary(points_2$biome)
summary(points_2$biome)

# as could be anticipated, most points are falling into tropical and temperate
# forests, and tropical grasslands

points_2 %>% group_by(species) %>%
  mutate(biome = if_else(biome %in% c("98", "99"), NA, biome)) %>% 
  summarize(num_biome = length(unique(biome)))

# common sense check that the code works
(points_2 %>% filter(species=="Acacia_acuminata"))$biome

(points_2 %>% filter(species=="Abrus_precatorius"))$biome

# common sense check-- species shouldn't be found in more than 16 biomes
# (this is the number of biome levels in the occurrence dataset-- remember,
# we have 98 and 99 in there)

# Group by species and get summarizing
summary_df <- points_2 %>% 
  group_by(species) %>% 
  mutate(biome = if_else(biome %in% c("98", "99"), NA, biome)) %>% 
  # I modified the line above, can change back to filtering these out if preferred.
#   Need to mention in methods though if filtering out.
  droplevels() %>% 
  reframe(n = n(),
          precip_maxquant = quantile(precip, 0.95), 
          precip_minquant = quantile(precip, 0.05),
          precip_mean = mean(precip),
          precip_median = median(precip),
          nitro_maxquant = quantile(nitrogen, 0.95),
          nitro_minquant = quantile(nitrogen, 0.05),
          nitro_mean = mean(nitrogen),
          nitro_median = median(nitrogen),
          temp_maxquant = quantile(temp, 0.95),
          temp_minquant = quantile(temp, 0.05),
          temp_mean = mean(temp),
          temp_median = median(temp),
          max_lat = max(Y),
          min_lat = min(Y),
          mean_lat = mean(Y),
          median_lat = median(Y),
          median_long = median(X),
          quant95 = quantile(Y, 0.95),
          quant005 = quantile(Y, 0.05),
          num_biome = length(unique(na.omit(biome))),
          biome = names(which.max(table(na.omit(biome))))
  )

summary(summary_df)

# add in our niche breadth measures
summary_df$precip_range <- summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range <- summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range <- summary_df$nitro_maxquant-summary_df$nitro_minquant

# Now read in traits

traits <- read.csv("data/legume_range_traits.csv") %>% 
  rename(species = Phy) %>% 
  select(species, genus, fixer, woody, annual, uses_num_uses, domY, efnY)

# Let us smoosh the traits together with the summary_df.

traits$species <- gsub(" ", "_", traits$species)
master_legume <- left_join(summary_df, traits, multiple="any")

summary(master_legume)

dropped_sp <- filter(master_legume, n<25)

master_thin <- master_legume %>% 
  filter(n >= 25)

master_thin$biome <- as.factor(master_thin$biome)

# write our new dataframe into a csv
write.csv(master_thin, "data/pgls_species_data.csv")


