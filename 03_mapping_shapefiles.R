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
# Tell R 
overlap<-st_intersects(df_1, subset_polygon, sparse=FALSE)

