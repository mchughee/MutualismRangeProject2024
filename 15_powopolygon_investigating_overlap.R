# Investigating why there ARE STILL SPECIES WITH >100% COVERAGE IN POWO POLYGONS

# Read in packages

library(ggplot2)
library(sf)
library(raster)
library(tidyverse)
library(rnaturalearthdata)

### For when working with powo polygons
poly_sf = st_read("powo_polygons/powo_polygons_sorted.shp")

# First, read in test df
points <- read.csv("thindat_climadd_soilgridsadd.csv") 

# Convert points to simplefeatures
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_")) %>% 
  filter(species %in% poly_sf$spcs_nm)

# Three species in this test set
table(points_sf$species)


# Code for when working with powo polygons
poly_sf = poly_sf %>% 
  # Convert each of these to sf
  #purrr::map(., st_as_sf) %>% 
  # Make valid
  #purrr::map(., st_make_valid) %>% 
  # Put the resulting list into a dataframe
  #tibble::enframe(name = NULL) %>% 
  # Then convert the nested list items into columns (code, status, geometry)
  #tidyr::unnest(cols = c(value)) %>% 
  # then bind the original dataframe back on to this one
  #dplyr::bind_cols(legume_pol, .) %>% 
  # drop the old polygon column which contains spatialPolygonsDataframes
  #dplyr:: select(-polygon) %>% 
  # Reconvert the whole thing to sf 
  #sf::st_as_sf() %>% 
  # Group by species and status
  #dplyr::group_by(spcs_nm, intrdcd) %>% 
  # Merge polygons within these groups
  #dplyr::summarize() %>% 
  dplyr::rename(species_polys = spcs_nm, status_polys = intrdcd)


# Make a list of species
species_list = unique(points_sf$species)

# Make an empty list to store results
status_list = list()

sf_use_s2(FALSE)
# Check each species points against polygons for that species
for (i in 1:length(species_list)) {
  # Select species i
  species_i = species_list[i]
  # Filter the points dataframe to only occurrences of that species
  points_i = points_sf %>% filter(species == species_i)
  # Filter the polygons object to only polygons for that species
  polygons_i = filter(poly_sf, species_polys == species_i)
  # Join these two features, which populates columns species_polys and status_polys with the species and status data of any polygon(s) that overlap a given point
  join_i = st_join(points_i, polygons_i, join = st_intersects) %>% 
    # Then select only that information (geometry tags along as well because this is a sf object)
    dplyr::select(species_polys, status_polys) %>% 
    # Add on a species identity column (because species_polys is NA if no polygon overlaps)
    mutate(species = species_i) %>% 
    # We then need to deal with the cases of multiple overlapping polygons and resulting multiple statuses 
    # Group by geometry (eg exact point location) and species identity
    group_by(geometry, species) %>% 
    # Concatenate all the statuses for each group
    summarize(status_polys = str_flatten(status_polys)) %>% 
    # Add on a species identity column (because species_polys is NA if no polygon overlaps)
    mutate(species = species_i)
  status_list[[i]] = join_i
}

status_df = status_list %>% 
  enframe(name = NULL) %>% 
  unnest(cols = c(value))

points_sf_with_statuses = left_join(points_sf, status_df)

#detach(package:plyr, unload=TRUE)
# Generate percentages of points with each status
status_counts = points_sf_with_statuses %>% 
  group_by(species) %>% 
  summarize(total = n(),
            pct_NA = sum(is.na(status_polys))/total,
            pct_0 = sum(status_polys %in% "0")/total,
            pct_1 = sum(status_polys %in% "1")/total,
            pct_mixed = sum(status_polys %in% c("10", "01", "11", "00"))/total) %>% 
  group_by(species) %>% 
  mutate(sum_pcts = rowSums(across(starts_with("pct"))))

problems<-subset(status_counts, pct_mixed>0.01)


# Generate plots to inspect
# Dunno if we'll want to do this for all species but good to do it for a bunch at first

world = map_data('world')

for (i in 1:length(species_list)){
  species_j = species_list[i]
  points_j = points_sf_with_statuses %>% filter(species == species_j)
  polygons_j = filter(poly_sf, species_polys == species_j)
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = polygons_j, aes(fill = status_polys), alpha = 0.2) +
    geom_sf(data = points_j, aes(color = status_polys), size = 0.5)
  ggsave(filename = str_c("polygon_plots_thinnedat/", species_j, ".pdf"), width = 14, height = 6)
}


