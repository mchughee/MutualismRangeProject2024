# getting native vs invasive status of occurrences for each species (and also dropping the species with <50 or >100 percent
# overlap with their polygons)
# plus, calculating niche breadth for native vs invasive points

library(tidyverse)
library

# read in occurrence data
points <- read.csv("thindat_climadd_soilgridsadd.csv")

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
less50<-read.csv("list_powo_pols_lessthan50.csv")
greater100<-read.csv("list_powo_pols_greaterthan100.csv")

# Now, let's drop species from our dataset that either 
# a) don't have polygons
# b) have greater than 100% overlap with polygons
# c) have less than 50% overlap with polygons

points_sf1<-points_sf %>% filter(species %in% poly_sf$spcs_nm) %>% 
  filter(!(species %in% greater100$species)) %>% 
  filter(!(species %in% less50$species))

# overlay points with polygons to grab native or invasive status

sf_use_s2(FALSE)
status<-NULL
for(i in (unique(points_sf1$species))){
  this.species<- st_join(points_sf1[points_sf1$species==i,], st_difference(poly_sf[poly_sf$spcs_nm==i,]),
                         join=st_intersects, left=TRUE, largest=FALSE)
  status<-rbind(status, this.species)
  print(i)}

# okay, now save classified points! haha!

# But first, let's drop some of these ridiculous extra columns...god gave us
# dplyr for a reason!

status1<-status %>% select(-c(mediaType, lastInterpreted, establishmentMeans,
                              typeStatus, rightsHolder, recordNumber, depth,
                              depthAccuracy, individualCount, infraspecificEpithet))


sf::st_write(status1, "invasiveclass_thindat_climadd_soilgridsadd.csv", layer_options = "GEOMETRY=AS_XY")


