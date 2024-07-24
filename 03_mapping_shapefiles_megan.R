# Overlaying occurrence data with Pooja's shapefiles

# Read in packages

library(ggplot2)
library(sf)
library(raster)
library(tidyverse)
library(rnaturalearthdata)


# First, read in test df
points <- read.csv("twenty_sp.csv") %>% 
  # Getting rid of these junk columns, these arise from write.csv, good to use row.names = FALSE 
  dplyr::select(-X) %>% 
  # Seems like there are still duplicate points in here
  distinct(species, decimalLatitude, decimalLongitude, .keep_all = T)

# Quick vis of occurrences
ggplot(points, aes(y = decimalLatitude, x = decimalLongitude, color = species)) +
  geom_point()

# Convert points to simplefeatures
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("decimalLongitude", "decimalLatitude"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
                      # Replace spaces in species names
                      mutate(species = str_replace(species, " ", "_"))

# Three species in this test set
table(points_sf$species)


# Read in polygons
legume_pol <- readRDS("legume_range_polygons_data.rds") %>% 
  # And fix the species names to match points
  mutate(species = str_replace(species, " ", "_")) %>% 
  # Filter to test species
  filter(species %in% points_sf$species)


# Convert to sf 
# Use the column polygon, which is a list of spatial polygon dataframes
poly_sf = legume_pol$polygon %>% 
  # Convert each of these to sf
  purrr::map(., st_as_sf) %>% 
  # Make valid
  purrr::map(., st_make_valid) %>% 
  # Put the resulting list into a dataframe
  tibble::enframe(name = NULL) %>% 
  # Then convert the nested list items into columns (code, status, geometry)
  tidyr::unnest(cols = c(value)) %>% 
  # then bind the original dataframe back on to this one
  dplyr::bind_cols(legume_pol, .) %>% 
  # drop the old polygon column which contains spatialPolygonsDataframes
  dplyr:: select(-polygon) %>% 
  # Reconvert the whole thing to sf 
  sf::st_as_sf() %>% 
  # Group by species and status
  dplyr::group_by(species, status) %>% 
  # Merge polygons within these groups
  dplyr::summarize() %>% 
  dplyr::rename(species_polys = species, status_polys = status)

# I'm sure there's a better way to do this step but couldn't think of how
# This approach feels a bit clunky
# Tried a full join of points and polygons, then filtering so that species.x == species.y
# But that method drops rows that don't fall in a polygon because species = NA

# Make a list of species
species_list = unique(points_sf$species)

# Make an empty list to store results
status_list = list()

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
            pct_N = sum(status_polys %in% "N")/total,
            pct_U = sum(status_polys %in% "U")/total,
            pct_I = sum(status_polys %in% "I")/total,
            pct_mixed = sum(status_polys %in% c("UN", "NU", "IN", "NI", "UI", "IU"))/total) %>% 
  group_by(species) %>% 
  mutate(sum_pcts = rowSums(across(starts_with("pct"))))

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
 ggsave(filename = str_c("polygon_plots/", species_j, ".pdf"), width = 14, height = 6)
}


