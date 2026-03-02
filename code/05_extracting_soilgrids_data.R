# Extracting SoilGrids data

library(raster)
library(terra)
library(sf)
library(tidyverse)
library(geodata)
library(here)

# read in thinned occurrence data

dat<-read_csv("data_files/thinned_data_climate.csv")

# tell sf where the long and lat are 
dat <- st_as_sf(x = dat, coords = c("X", "Y"))
# Tell R to read coordinates as WGS84
dat<-st_set_crs(dat, 4326)

# pull nitrogen raster data
nitrogen<-soil_world_vsi("nitrogen", 15, stat="mean")
writeRaster(nitrogen, "nitrogen_5_15_mean_igh.tif")

# Read in nitrogen raster
nitrogen<-raster::raster("nitrogen_5_15_mean_igh.tif")
crs(nitrogen)<-(4326)
# plot nitrogen data to make sure it looks reasonable
raster::plot(nitrogen)
st_crs(nitrogen)


# Extract soil grids data
dat$nitrogen <-raster::extract(nitrogen, dat)

# write new data file
sf::st_write(dat, "thindat_climadd_soilgridsadd.csv", layer_options = "GEOMETRY=AS_XY")