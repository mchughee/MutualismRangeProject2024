# Making df with summarized niche breadth measures

library(tidyverse)
library(cowplot)
library(ggplot2)


# Read in occurrences with environmental data etc.
points <- read_csv("data_large/allocc_with_native_status.csv")
n_distinct(points$species)

# Read in lists of species with poor polygon overlap
lt50 = read_csv("species_lists/list_powo_pols_lessthan50.csv")
gt100 = read_csv("species_lists/list_powo_pols_greaterthan100.csv")

per_sp_count = points %>% 
  group_by(species) %>% 
  summarize(n = n())

# Drop species with fewer than 25 occurrences, this is described earlier in the methods text but done here. could move this step earlier in revisions.
points_0 <- points %>% 
  group_by(species) %>% 
  mutate(n = n()) %>% 
  filter(n>=25) %>% 
  # Also drop species with occurrences falling in <50 or >100% of polygons.
  filter(!(species %in% lt50$species)) %>% 
  filter(!(species %in% gt100$species)) 
         
n_distinct(points_0$species)

# Drop points that have NA values for the intrdcd status 
# Do this or not?
points_1 <- points_0 %>% 
  drop_na(intrdcd) %>%
  group_by(species) %>% 
  mutate(n1 = n()) %>% 
  filter(n1 >= 25)

n_distinct(points_0$species)
n_distinct(points_1$species)

n_distinct(points_0$species) - n_distinct(points_1$species)

per_sp_count_1 = points_1 %>% 
  group_by(species) %>% 
  summarize(n1 = n())

check = left_join(per_sp_count, per_sp_count_1) %>%
  mutate(pct = n1/n) %>% 
  filter(n1 >= 25)

median(check$pct)

# Drop points that have NA values for the niche axes 

points_2 <- points_1 %>% 
  drop_na(precip) %>% 
  drop_na(temp) %>% 
  drop_na(nitrogen) %>% 
  group_by(species) %>% 
  mutate(n2 = n()) %>% 
  filter(n2>=25)
n_distinct(points_1$species)
n_distinct(points_2$species)

hist(points_2$nitrogen)

n_distinct(points_1$species) - n_distinct(points_2$species)

# Check what percent of observations are retained on a species-by-species basis
per_sp_count_2 = points_2 %>% 
  group_by(species) %>% 
  summarize(n2 = n())

check2 = left_join(check, per_sp_count_2) %>% 
  mutate(pct2 = n2/n1) %>% 
  filter(n2 >= 25)

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
table((points_2 %>% filter(species=="Acacia_acuminata"))$biome)
table((points_2 %>% filter(species=="Abrus_precatorius"))$biome)

# common sense check-- species shouldn't be found in more than 16 biomes
# (this is the number of biome levels in the occurrence dataset-- remember,
# we have 98 and 99 in there)

# Group by species and get summarizing
summary_df <- points_2 %>% 
  group_by(species) %>% 
  mutate(biome = if_else(biome %in% c("98", "99"), NA, biome)) %>%
#   Drop the above instead?
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
  ) %>% 
  mutate(precip_range = precip_maxquant - precip_minquant,
         temp_range = temp_maxquant - temp_minquant,
         nitro_range = nitro_maxquant - nitro_minquant)

summary(summary_df)
# there are four species with a nitrogen range of 0. this is realy strange. must be due to issues with the soil data?
plot(points$Y, points$nitrogen)

# Now read in traits

traits <- read.csv("data/legume_range_traits.csv") %>% 
  rename(species = Phy) %>% 
  select(species, genus, fixer, woody, annual, uses_num_uses, Domatia, EFN)

# Let us smoosh the traits together with the summary_df.

traits$species <- gsub(" ", "_", traits$species)
master_legume <- left_join(summary_df, traits, multiple="any") %>% 
  filter(nitro_range >0)

summary(master_legume)

master_legume$biome <- as.factor(master_legume$biome)

# write our new dataframe into a csv
write_csv(master_legume, "data/pgls_species_data.csv")


