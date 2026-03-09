# Calculating fit of powo/kew polygons and classifying occurrence points

library(sf)
library(tidyverse)
library(ggplot2)
library(geojsonsf)


# Pull in the species distribution polygons
poly_sf = st_read("data_large/powo_polygons_sorted.shp")

# Read in occurrence data
points <- read_csv("data_large/allocc_with_env.csv")

# Put points into sf
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  filter(species %in% poly_sf$spcs_nm)


# Okay, now time to run through a big loop check how good the fit is for all the points and the polygons for each species 
# Turning geodesic geometry off
sf_use_s2(FALSE)

species_list = unique(points_sf$species) %>% sort()

all = list()

for (i in 1:length(species_list)) {
  this.species <- st_intersects(st_make_valid(poly_sf[poly_sf$spcs_nm == species_list[i],]), points_sf[points_sf$species == species_list[i],], sparse = FALSE)
  all[[i]] <- this.species
  }

names(all) = species_list

head(all)

# calculate number of trues across polygons, i.e. the number of occurrences falling within polygons.
results <- sapply(all, sum) 

results_df = tibble(species = names(results), num_in_polygons = results) 

# Next step: I'd like to see how this compares to the total number of occurrences we had post-thinning

# Get number of occurrences for each species
counts <- points_sf %>%
  filter(species %in% poly_sf$spcs_nm) %>%
  group_by(species) %>%
  summarize(num_total = n())

# bind with overlay results
final_df = left_join(counts, results_df) %>% 
  mutate(percent_in_polys = num_in_polygons/num_total*100)

# make histogram
hist(final_df$percent_in_polys)

# grab species with >100 percent cover, suggesting some overlapping polygons
greater100 <- final_df %>% filter(percent_in_polys > 100)
write_csv(greater100, "powo_polygons/list_powo_pols_greaterthan100.csv")

# Look at why these species have >100%
look = all$Anthyllis_montana %>% 
  t() %>% 
  tibble() %>% 
  mutate(total = rowSums(.))

look1 = rowSums(look)

which(look1 > 1)

look2 = poly_sf %>% filter(spcs_nm == "Anthyllis_montana")
plot(look2)

look3 = points_sf %>% 
  filter(species == "Anthyllis_montana") %>%
  bind_cols(., look) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  

world = map_data("world")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = poly_sf[poly_sf$spcs_nm=="Anthyllis_montana",], aes(fill = poly_sf[poly_sf$spcs_nm=="Anthyllis_montana",]$intrdcd), alpha = 0.2) +
  geom_sf(data = filter(look3, total == 2), aes(color = as.factor(total)), size = 0.5) +
  theme(legend.title=element_blank())

# I think there is just some overlap in polygon boundaries

# grab species with <50% cover, because that indicates the polygons are not fitting very well
less50 <- final_df %>% filter(percent_in_polys < 50)
write_csv(less50, "powo_polygons/list_powo_pols_lessthan50.csv")


# Now, let's drop species from our dataset that either 
# a) don't have polygons
# b) have greater than 100% overlap with polygons
# c) have less than 50% overlap with polygons

points_sf1 <- points_sf %>%
  filter(!(species %in% greater100$species)) %>% 
  filter(!(species %in% less50$species))

n_distinct(points_sf1$species)

# overlay points with polygons to grab native or invasive status

sf_use_s2(FALSE)
status <- NULL

for (i in (unique(points_sf1$species))){
  this.species <- st_join(points_sf1[points_sf1$species == i,], 
                         st_difference(poly_sf[poly_sf$spcs_nm == i,]),
                         join = st_intersects, left = TRUE, largest = FALSE)
  status <- rbind(status, this.species)
  print(i)
  }


status1 <- status %>% select(-c(plnt_n_, cntn__1, rgn_c_2, ar_cd_3, extinct, lctn_db, spcs_nm))


sf::st_write(status1, "data_large/allocc_with_native_status.csv", layer_options = "GEOMETRY=AS_XY")



# set introducd to be a factor
poly_sf$intrdcd<-as.factor(poly_sf$intrdcd)
status1$intrdcd<-as.factor(status1$intrdcd)

# check to make sure our spatial intersection in the last script worked

world = map_data("world")
sf_use_s2(FALSE)
for (i in unique(points_sf$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = poly_sf[poly_sf$spcs_nm==i,], aes(fill = poly_sf[poly_sf$spcs_nm==i,]$intrdcd), alpha = 0.2) +
    geom_sf(data = status1[status1$species==i,], aes(color = status1[status1$species==i,]$intrdcd), size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("planar_kew_polys/", species_j, ".pdf"), width = 14, height = 6)
  }

x = status1 %>% filter(species == "Albizia_forbesii")

# note that this prints pdfs for species excluded by our greater than 100, less than 50 filters (no points on these)
