# powo reclass draft 2

library(sf)
library(terra)
library(tidyverse)
library(rnaturalearthdata)
library(ggplot2)

# Get continents
continents<-st_read("World_Continents_316565891929102346/World_Continents.shp")
continents<-st_transform(continents, crs=4326)
st_crs(continents)
plot(continents)

# Get points so we can find the species we want

points <- read.csv("thindat_climadd_soilgridsadd.csv")

# Convert points to simplefeatures
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326) %>% 
  # Replace spaces in species names
  mutate(species = str_replace(species, " ", "_"))

# Read in polygons
poly_sf <- st_read("powo_polygons/powo_polygons_sorted.shp")

# subset data
points_sf<-subset(points_sf, species=="Abrus_fruticulosus" | species=="Acacia_acinacea" | species=="Ononis_spinosa")
poly_sf<-subset(poly_sf, spcs_nm=="Abrus_fruticulosus" | spcs_nm=="Acacia_acinacea" | spcs_nm=="Ononis_spinosa")

# Find the continent that each
sf_use_s2(FALSE)
results<-NULL
for(i in (unique(poly_sf$spcs_nm))){
  this.species<- st_intersection(continents, st_difference(st_make_valid(poly_sf[poly_sf$spcs_nm==i,])))
  results<-rbind(results, this.species)
  print(i)}

# Tell R that if there is a native polygon on that continent, all polys for that species on that continent
# should be classified as native as well
for (i in (1:nrow(results))){
  if (results[i,]$intrdcd=="0"){
    results[results$CONTINENT==results[i,]$CONTINENT & results$spcs_nm==results[i,]$spcs_nm,]$intrdcd<-"0"
  }}

# Get the native/introduced status of each point in the dataset
status<-NULL
for(i in (unique(points_sf$species))){
  this.species<- st_join(points_sf[points_sf$species==i,], st_difference(results[results$spcs_nm==i,]),
                         join=st_intersects, left=TRUE, largest=FALSE)
  status<-rbind(status, this.species)
  print(i)}

# rename the continent column so that later we know if the continent came from the point-continent or polygon-continent
# intersection
names(status)[names(status) == 'CONTINENT'] <- 'continent_from_polygon_overlay'

# Get continent for every point
new_points<-NULL
for(i in (unique(status$species))){
  this.species<- st_join(status[status$species==i,], continents, join=st_intersects, left=TRUE, largest=FALSE)
  new_points<-rbind(new_points, this.species)
  print(i)}

# Renaming the continent column!
names(new_points)[names(new_points) == 'CONTINENT'] <- 'continent_from_point_cont_overlay'

# Pare down the dataframe to just what we need!
new_points<-new_points %>% select(c(temp, precip, nitrogen, continent_from_point_cont_overlay,
                                     continent_from_polygon_overlay, intrdcd, species, spcs_nm,
                                     geometry, rgn_c_2, region, ar_cd_3, area))

# Make an extra df to compare output of this next loop against!

new_points1<-new_points
new_points

# Tell R that if an occurrence has an NA introduced status but falls on a continent with
# native occurrences, reclassify it as 0 (native)

new_points1$intrdcd<-as.factor(new_points1$intrdcd)

for (i in (1:nrow(new_points1))){
  native_points<-filter(new_points1, species==new_points1[1,]$species & intrdcd=="0")
  if (is.na(new_points1[i,]$intrdcd)){
    if(new_points1[i,]$continent_from_point_cont_overlay %in% native_points$continent_from_point_cont_overlay){
      new_points1[i,]$intrdcd<-"0"
    }
    else new_points1[i,]$intrdcd}}

# And vice versa
for (i in (1:nrow(new_points1))){
  invasive_points<-filter(new_points1, species==new_points1[1,]$species & intrdcd=="1")
  if (is.na(new_points1[i,]$intrdcd)){
    if(new_points1[i,]$continent_from_point_cont_overlay %in% invasive_points$continent_from_point_cont_overlay){
      new_points1[i,]$intrdcd<-"1"
    }
    else new_points1[i,]$intrdcd}}

new_points1$intrdcd<-as.factor(new_points1$intrdcd)

ggplot() +
  geom_sf(data = continents, aes(geometry=geometry), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = new_points1[new_points1$species=="Abrus_fruticulosus",], aes(geometry=geometry, color = intrdcd), alpha = 0.2)+
  geom_sf(data = poly_sf[poly_sf$spcs_nm=="Abrus_fruticulosus",], aes(geometry=geometry, fill = intrdcd), alpha = 0.2)


















