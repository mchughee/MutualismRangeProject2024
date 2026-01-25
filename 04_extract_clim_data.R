# Extracting climate data for thinned occurrence data

# read in packages
library(raster)
library(sf)
library(dplyr)
library(tidyverse)

# Read in thinned data
# Here, read_csv is better at handling the large amount of data, but it struggles
# with a column (TypeStatus) that we don't use...moving on as we don't use this in the analysis

thin<-read_csv("data_files/thinned_data.csv")
str(thin)


# Convert to sf object
thin <- st_as_sf(x = thin, coords = c("X", "Y")) %>% 
# Tell R to read coordinates as WGS84
st_set_crs(., 4326)

# Read in raster climate data

temp<-raster::raster("wc2.1_30s_bio_1.tif")
precip<-raster::raster("wc2.1_30s_bio_12.tif")

# Extract climate data

thin$temp <-raster::extract(temp, thin)
thin$precip <-raster::extract(precip, thin)

# write into st object
sf::st_write(thin, "thinned_data_climate.csv", layer_options = "GEOMETRY=AS_XY")



