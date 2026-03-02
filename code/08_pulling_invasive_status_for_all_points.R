# getting native vs invasive status of occurrences for each species (and also dropping the species with <50 or >100 percent
# overlap with their polygons)
# plus, calculating niche breadth for native vs invasive points

library(tidyverse)
library(sf)

# read in occurrence data
points <- read_csv("data/thinnedoccs_soil_clim_biome.csv")
# The warning message is R being confused about the input format of a column
# that I don't care about

n_distinct(points$species)

# Convert points to simplefeatures
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_"))


# Pull in the powo polygons
poly_sf = st_read("powo_polygons/powo_polygons_sorted.shp")

# Read in the lists of species with <50 or >100 percent coverage
less50<-read.csv("powo_polygons/list_powo_pols_lessthan50.csv")
greater100<-read.csv("powo_polygons/list_powo_pols_greaterthan100.csv")

# Now, let's drop species from our dataset that either 
# a) don't have polygons
# b) have greater than 100% overlap with polygons
# c) have less than 50% overlap with polygons

points_sf1<-points_sf %>% filter(species %in% poly_sf$spcs_nm) %>% 
  filter(!(species %in% greater100$species)) %>% 
  filter(!(species %in% less50$species))

n_distinct(points_sf1$species)

# overlay points with polygons to grab native or invasive status

sf_use_s2(FALSE)
status<-NULL
for(i in (unique(points_sf1$species))){
  this.species<- st_join(points_sf1[points_sf1$species==i,], st_difference(poly_sf[poly_sf$spcs_nm==i,]),
                         join=st_intersects, left=TRUE, largest=FALSE)
  status<-rbind(status, this.species)
  print(i)}

# okay, now save classified points!

# But first, let's drop some of these ridiculous extra columns...

status1<-status %>% select(-c(mediaType, lastInterpreted, establishmentMeans,
                              typeStatus, rightsHolder, recordNumber, depth,
                              depthAccuracy, individualCount, infraspecificEpithet))


sf::st_write(status1, "data/invasiveclass_thindat_climadd_soilgridsadd.csv", layer_options = "GEOMETRY=AS_XY")
################################################################################
####### check that points fall into polygons
poly_sf = st_read("powo_polygons/powo_polygons_sorted.shp")

# set intrdcd to be a factor
poly_sf$intrdcd<-as.factor(poly_sf$intrdcd)
status1$intrdcd<-as.factor(status1$intrdcd)

#check to make sure our spatial intersection in the last script worked
sf_use_s2(FALSE)
for (i in unique(points_filtered$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = poly_sf[poly_sf$spcs_nm==i,], aes(fill = poly_sf[poly_sf$spcs_nm==i,]$intrdcd), alpha = 0.2) +
    geom_sf(data = status1[status1$species==i,], aes(color = status1[status1$species==i,]$intrdcd), size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("planar_kew_polys/", species_j, ".pdf"), width = 14, height = 6)}
