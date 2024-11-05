# Extracting climate data for thinned occurrence data

# read in packages
library(raster)
library(sf)

# Read in thinned data
thin<-read.csv("thinned_data.csv")

# Convert to sf object
thin <- st_as_sf(x = thin, coords = c("X", "Y"))
# Tell R to read coordinates as WGS84
st_set_crs(thin, 4326)->thin

# Read in raster climate data

temp<-raster::raster("wc2.1_30s_bio_1.tif")
precip<-raster::raster("wc2.1_30s_bio_12.tif")

# Extract climate data

thin$temp <-raster::extract(temp, thin)
thin$precip <-raster::extract(precip, thin)

# write into st object
sf::st_write(thin, "thinned_data_climate.csv", layer_options = "GEOMETRY=AS_XY")



