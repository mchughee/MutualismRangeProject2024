# Overlaying occurrence data with Pooja's shapefiles

# Read in packages

library(ggplot2)
library(sf)
library(raster)
library(tidyverse)
library(CoordinateCleaner)
library(rnaturalearthdata)

# setwd
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

# First, read in test df
test<-read.csv("dat_test_clean.csv")
test_df <- st_as_sf(x = test,                         
               coords = c("decimalLongitude", "decimalLatitude"))
# Tell R to read coordinates as WGS84
st_set_crs(df, 4326)->test_df_1

class(df_1)
st_crs(df_1)

# read in shapefiles and whip em into shape. All code taken from Pooja's repo.
legume_pol <- readRDS("legume_range_polygons_data.rds")

tmp <- data.frame(code = NULL, index = NULL)
for (x in 1:length(legume_pol$polygon)) {
  if(legume_pol$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pol$polygon[[x]]@data$Code, index = x)), tmp)
}

legume_pol$code<-NA
for (x in 1:length(legume_pol$polygon)) {
  legume_pol[x, "code"] <- legume_pol$polygon[[x]]@data$Code 
}

spatial_polygons <- do.call(rbind, legume_pol$`polygon`)
spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data = legume_pol, match.ID = F)
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons_df)

subset_polygon<-filter(spatial_polygons_sf, species=="Abrus fruticulosus"|species=="Abrus precatorius"|species=="Acacia acinacea")


# Tell R to not interpret data as being spherical
# Update: can only do this for small areas, not global ones!
# will have to find another solution
# sf_use_s2(FALSE)

# fix up the species names so they don't have a space inbetween
test_df_1$species<-gsub(" ", "_", test_df_1$species)
subset_polygon$species<-gsub(" ", "_", subset_polygon$species)

# p <- poly2nb(st_make_valid(subset_polygon))

# Try to merge polygons by species
#for(i in (unique(subset_polygon$species))){
 # this.pol<- st_union(st_make_valid(subset_polygon[subset_polygon$species==i,]), by_feature=FALSE)
  #assign(paste0("polygon", "_", i), this.pol, envir = .GlobalEnv)}

#plot(polygon_Abrus_fruticulosus)

# Suck it, R, I wrote a for loop AND it works for once

<<<<<<< HEAD
#polygonslist <- mget(ls(pattern="polygon_*"), envir = globalenv())

for(i in (unique(df_1$species))){
  this.species<- st_intersects(st_make_valid(subset_polygon[subset_polygon$species==i,]), df_1[df_1$species==i,], sparse=FALSE)
  assign(paste0("overlay", "_", i), this.species, envir = .GlobalEnv)}
=======
for(i in (unique(test_df_1$species))){
  this.species<- st_intersects(st_make_valid(subset_polygon[subset_polygon$species==i,]), test_df_1[test_df_1$species==i,], sparse=TRUE)
  assign(paste0("testoverlay", "_", i), this.species, envir = .GlobalEnv)}
>>>>>>> 5a680cbdae2b340eee15aef0e2824c4643717cc2

## to compare speed on two different functions, use system.time() to
## look at difference
## merge all polygons into one polygon? or one polygon for native,
# one for invasive
# function to merge them? Might make it faster to run!
# use st_union

# make list of files
all <- mget(ls(pattern="overlay_*"), envir = globalenv())

# calculate number of trues across polygons, i.e. the number of occurrences
# falling within polygons.
results <- lapply(all, sum)
# make dataframe to look at results
overlayresults<-as.data.frame(do.call(rbind, results))

# Next step: I'd like to see how this compares to the total number of occurrences
# that we pulled from gbif

# get number of occurrences for each species (we kind of already have this, but
# this method seems easier than trying to deal with logical matrices)
df_1 %>% 
  group_by(species) %>% 
  summarize(count=n())->counts

# bind with overlay results
cbind(overlayresults, counts)-> finaldf

# Add in percents
colnames(finaldf)[1] <- "no_occ_in_pol"
finaldf$percent<-(finaldf$no_occ_in_pol/finaldf$count)*100
mean(finaldf$percent)

# Using a method I already know works to validate that sums() is correctly
# grabbing true occurrences
overlay_Acacia_acinacea<-as.data.frame(overlay_Acacia_acinacea)
occ_true<-rowSums(overlay_Abrus_fruticulosus == "TRUE")
occ_false<-rowSums(overlay_Abrus_fruticulosus == "FALSE")

# Common-sense check time! I am going to try to map the points and polygons
# to do a visual check

# First, abrus fruticulosus

pol_frut<-subset(subset_polygon, species=="Abrus_fruticulosus")
occ_frut<-subset(df_1, species=="Abrus_fruticulosus")

plot(pol_frut)

world <- map_data('world')
world<-st_as_sf(world, coords = c("long", "lat"))
st_set_crs(world, 4326)->world


ggplot() +
  #geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_sf(data=subset(pol_frut))+
  geom_sf(data = occ_frut, color = "red", inherit.aes = T) 

# Next, acacia acinacea

pol_acacia<-subset(subset_polygon, species=="Acacia_acinacea")
occ_acacia<-subset(df_1, species=="Acacia_acinacea")

world <- map_data('world')
world<-st_as_sf(world, coords = c("long", "lat"))

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_sf(data=subset(pol_acacia), aes(fill = num_species))+
  geom_sf(data = occ_acacia, color = "red", inherit.aes = T) 


plot(subset_polygon)
