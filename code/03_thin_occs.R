# Thinning points with spatsample

# First, read in packages and data
library(terra)
library(sf)
library(dplyr)

# Using the twenty species dataframe for right now, but replace with full data when the time comes
occ<-read.csv("data/allocc_clean.csv")

occ$species<-gsub(" ", "_", occ$species)

# Read in temperature raster to supply cells to sample
# 30s Bioclim data from here https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_bio.zip
temp<-raster::raster("wc2.1_30s_bio_1.tif")


# tell R where the long/lat is in the dataframe and the crs
# Code snippet from Tyler Smith at AAFC

occs_ls <- terra::vect(occ, geom = c("decimalLongitude",
                              "decimalLatitude"),
                crs = "+proj=longlat +datum=WGS84")

# use spatsample (terra) to thin data to one observation per cell BUT per species

results<-NULL

for (i in length(unique(occs_ls$species))) {
  this.species<- spatSample(occs_ls[occs_ls$species==i,], size=1, strata = temp)
  my_sf<-sf::st_as_sf(this.species)
  results<-rbind(results, my_sf)
  print(i)
    }

sf::st_write(results1, "data/thinned_data.csv", layer_options = "GEOMETRY=AS_XY")


# Nice and easy! Now we have a thinned dataframe
