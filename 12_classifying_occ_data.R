# Adding classification of native or not (?) to points

library(sf)
library(terra)
library(raster)
library(tidyverse)
library(ggplot2)

# First, read in data set (test for now!)
shape <- st_read("reclassified_polygons_aug2024/reclassified_polygons.shp")

# Read in occurrence data
points <- read.csv("thindat_climadd_soilsgridsadd.csv")

# Put points into sf (a la Megan B)
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("decimalLongitude", "decimalLatitude"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_"))
points_sf_subset<-subset(points_sf, species=="Abrus_fruticulosus"|species=="Lotus_pedunculatus"|species=="Ononis_spinosa")

#use st_intersection to get the status of each point
results<-NULL
for(i in (unique(points_sf_subset$species))){
  this.species<- st_join(points_sf_subset[points_sf_subset$species==i,], shape[shape$spcs_pl==i,], left = TRUE, largest=TRUE)
  results<-rbind(results, this.species)
  print(i)}


# Drop unnecessary columns

results1<-results %>% select(-c(X.1, X))

# write as st object
st_write(results1, "thinned_data_reclass.csv", layer_options = "GEOMETRY=AS_XY")

# What are some common-sense checks that can be done to make sure this function worked?

results<-read.csv("thinned_data_reclass.csv")

results <- st_as_sf(x = results,                         
                    coords = c("X", "Y"),
                    crs=4326)

# Replace NA values with I value
results$stts_pl<-as.character(results$stts_pl)
results$stts_pl[results$stts_pl==""] <- "I"
results$stts_pl<-as.factor(results$stts_pl)

world = map_data('world')

for (i in unique(results$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = shape[shape$spcs_pl==i,], aes(fill = shape[shape$spcs_pl==i,]$stts_pl), alpha = 0.2) +
    geom_sf(data = results[results$species==i,], aes(color = results[results$species==i,]$stts_pl), size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("point_reclass_check/", species_j, ".pdf"), width = 14, height = 6)}
  
  

