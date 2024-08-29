# Adding classification of native or not (?) to points

library(sf)
library(terra)
library(raster)
library(tidyverse)
library(ggplot2)

# First, read in data set (test for now!)
shape <- st_read("reclassified_polygons_aug2024/reclassified_polygons.shp")

# Read in occurrence data
points <- read.csv("thindat_climadd_soilgridsadd.csv")

# Put points into sf (a la Megan B)
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_"))
# points_sf_subset<-subset(points_sf, species=="Abrus_fruticulosus"|species=="Lotus_pedunculatus"|species=="Ononis_spinosa")

#use st_intersection to get the status of each point
results<-NULL
for(i in (unique(points_sf$species))){
  this.species<- st_join(points_sf[points_sf$species==i,], shape[shape$spcs_pl==i,], left = TRUE, largest=TRUE)
  results<-rbind(results, this.species)
  print(i)}


# Drop unnecessary columns

#results1<-results %>% select(-c(X.1, X))

# write as st object
st_write(results, "thinned_data_reclass_full.csv", layer_options = "GEOMETRY=AS_XY")

# What are some common-sense checks that can be done to make sure this function worked?

data<-read.csv("thinned_data_reclass_full.csv")

data <- st_as_sf(x = data,                         
                    coords = c("X", "Y"),
                    crs=4326)

data_sub<-filter(data, species=="Crudia_oblonga"|species=="Acacia_acinacea"|species=="Dalbergia_lactea"|
                   species=="Lupinus_nootkatensis"|species=="Ulex_europaeus"|species=="Genista_hispanica"|
                   species=="Psorothamnus_schottii"|species=="Abrus_precatorius"|species=="Abrus_fruticulosus"|
                   species=="Ornithopus_perpusillus"|species=="Lotus_pedunculatus"|species=="Vicia_cracca"|
                   species=="Trifolium_medium"|species=="Ononis_spinosa"|species=="Medicago_sativa")

# Replace NA values with I value
data_sub$stts_pl<-as.character(data_sub$stts_pl)
data_sub$stts_pl[data_sub$stts_pl==""] <- "I"
data_sub$stts_pl<-as.factor(data_sub$stts_pl)

world = map_data('world')

for (i in unique(data_sub$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = shape[shape$spcs_pl==i,], aes(fill = shape[shape$spcs_pl==i,]$stts_pl), alpha = 0.2) +
    geom_sf(data = data_sub[data_sub$species==i,], aes(color = data_sub[data_sub$species==i,]$stts_pl), size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("checking_if_pointreclass_worked_full/", species_j, ".pdf"), width = 14, height = 6)}
  
# plotting individual polygonzzzz

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = ono, aes(fill = ono$stts_pl), alpha = 0.2)

