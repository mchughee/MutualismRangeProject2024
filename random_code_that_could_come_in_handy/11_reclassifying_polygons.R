library(sf)
library(terra)
library(tidyverse)
library(rnaturalearthdata)
library(ggplot2)



# Get continents
continents<-st_read("World_Continents_316565891929102346/World_Continents.shp")
continents<-st_transform(continents, crs=4326)
plot(continents)

# Get points so we can find the species we want

points <- read.csv("thindat_climadd_soilgridsadd.csv") #%>% 
  # Getting rid of these junk columns, these arise from write.csv, good to use row.names = FALSE 
  #dplyr::select(-X) %>% 
  # Seems like there are still duplicate points in here
 # distinct(species, "X", "Y", .keep_all = T)


# Convert points to simplefeatures
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_"))

# Read in our polygons-- thanks Megan for the code
#legume_pol <- readRDS("legume_range_polygons_data.rds") %>% 
  # And fix the species names to match points
 # mutate(species = str_replace(species, " ", "_")) %>% 
  # Filter to test species
  #filter(species %in% points_sf$species)

poly_sf <- st_read("powo_polygons/powo_polygons_sorted.shp")


# Convert to sf 
# Use the column polygon, which is a list of spatial polygon dataframes
poly_sf = legume_pol$polygon %>% 
  # Convert each of these to sf
 # purrr::map(., st_as_sf) %>% 
  # Make valid
  purrr::map(., st_make_valid) %>% 
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
  dplyr::group_by(species, status) %>% 
  # Merge polygons within these groups
  dplyr::summarize() %>% 
  dplyr::rename(species_polys = species, status_polys = status)


### Okayyyyyy what to do next...let's use st_intersection to get the continents each polygon is on!

results<-NULL
for(i in (unique(poly_sf$spcs_nm))){
 this.species<- st_intersection(st_make_valid(poly_sf[poly_sf$spcs_nm==i,]), continents)
  results<-rbind(results, this.species)
  print(i)}

#for (i in (1:nrow(poly_sf))){
 # this.species<- st_intersection(st_make_valid(poly_sf[i,]), continents)
  #results<-rbind(results, this.species)
  #print(i)}
  
#sf::st_write(results1, "polygon_continents.csv", layer_options = "GEOMETRY=AS_XY")


results_unchanged<-results

results<-results_unchanged

# Write a little loop that tells R if a species has a native polygon on one continent, the entire continent
# should be classified as native. Thanks to Kevin for help writing this.
results$intrdcd<-as.factor(results$intrdcd)

for (i in (1:nrow(results))){
  if (results[i,]$intrdcd=="0"){
    results[results$CONTINENT==results[i,]$CONTINENT & results$spcs_nm==results[i,]$spcs_nm,]$intrdcd<-"0"
  }}
# Get those points!!

species_list = unique(points_sf$species)

status_list = list()

# Check each species points against polygons for that species
for (i in 1:length(species_list)) {
  # Select species i
  species_i = species_list[i]
  # Filter the points dataframe to only occurrences of that species
  points_i = points_sf %>% filter(species == species_i)
  # Filter the polygons object to only polygons for that species
  polygons_i = filter(results, species_polys == species_i)
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
  ggsave(filename = str_c("polygon_plots_reclass/", species_j, ".pdf"), width = 14, height = 6)
}


p<-st_collection_extract(results, "POLYGON")
st_cast()
st_write(p, "reclassified_polygons.shp")

class(results)

# Let's bring back in this forking shapefile and see if it worked!!
shape <- st_read("reclassified_polygons.shp")




