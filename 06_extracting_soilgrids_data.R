# Extracting SoilGrids data

library(raster)
library(terra)
library(sf)
library(tidyverse)

# read in thinned occurrence data

dat<-read_csv("thinned_data_climate.csv")

# tell sf where the long and lat are 
dat <- st_as_sf(x = dat, coords = c("X", "Y"))
# Tell R to read coordinates as WGS84
dat<-st_set_crs(dat, 4326)

# Read in nitrogen raster
nitro<-raster::raster("nitrogen_5-15cm_mean.tif")
# plot nitrogen data to make sure it looks reasonable
raster::plot(nitro)

# Extract soil grids data
dat$nitrogen <-raster::extract(nitro, dat)

# write new data file
sf::st_write(dat, "thindat_climadd_soilgridsadd.csv", layer_options = "GEOMETRY=AS_XY")