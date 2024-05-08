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
df <- st_as_sf(x = test,                         
               coords = c("decimalLongitude", "decimalLatitude"))
# Tell R to read coordinates as WGS84
st_set_crs(df, 4326)->df_1

class(df_1)
st_crs(df_1)

# read in shapefiles and whip em into shape. All code taken from Pooja's repo.
legume_pol <- readRDS("legume_range_polygons_data.rds")

tmp <- data.frame(code = NULL, index = NULL)
for (x in 1:length(legume_pol$polygon)) {
  if(legume_pol$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pol$polygon[[x]]@data$Code, index = x)), tmp)
}
unique_pols <- legume_pol[tmp$index, ]
unique_pols <- cbind(unique_pols, tmp)

legume_pol$code<-NA
for (x in 1:length(legume_pol$polygon)) {
  legume_pol[x, "code"] <- legume_pol$polygon[[x]]@data$Code 
}

lspecies_counts <- subset(legume_pol, introduced_status == "N") %>%
  group_by(code) %>%
  summarise(num_species = n_distinct(species))

# Merge with unique polygons
merge <- sp::merge(unique_pols, lspecies_counts, by.x="code", by.y="code")


spatial_polygons <- do.call(rbind, merge$`polygon`)
attribute_data <- merge[c("species", "num_species")]
spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data = attribute_data, match.ID = F)
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons_df)

subset_polygon<-filter(spatial_polygons_sf, species=="Abrus fruticulosus"|species=="Abrus precatorius"|species=="Acacia acinacea")

# Tell R to not interpret data as being spherical
sf_use_s2(FALSE)

# fix up the species names so they don't have a space inbetween
df_1$species<-gsub(" ", "_", df_1$species)
subset_polygon$species<-gsub(" ", "_", subset_polygon$species)


# Suck it, R, I wrote a for loop AND it works for once

for(i in (unique(df_1$species))){
  this.species<- st_intersects(subset_polygon[subset_polygon$species==i,], df_1[df_1$species==i,], sparse=FALSE)
  assign(paste0("overlay", "_", i), this.species, envir = .GlobalEnv)}


# make list of st_intersect output objects
all <- mget(ls(pattern="overlay_*"), envir = globalenv())

# calculate number of trues across polygons, i.e. the number of occurrences
# falling within polygons.
results <- lapply(all, sum)
# make dataframe to look at results
datanew<-as.data.frame(do.call(rbind, results))

# Next step: I'd like to see how this compares to the total number of occurrences
# that we pulled from gbif




# Okay, now to take the matrix output and make it *actually useful*
overlay_Acacia_acinacea<-as.data.frame(overlay_Acacia_acinacea)
occ_true<-rowSums(overlay_Abrus_fruticulosus == "TRUE")
occ_false<-rowSums(overlay_Abrus_fruticulosus == "FALSE")


