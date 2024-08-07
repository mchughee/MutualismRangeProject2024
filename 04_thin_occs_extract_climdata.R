# Thinning points with spatsample instead of spthin

# First, read in packages and data
library(terra)
library(sf)
library(dplyr)

# Using the twenty species dataframe for right now, but replace with full data when the time comes
occ<-read.csv("allocc_clean.csv")
occ$species<-gsub(" ", "_", occ$species)

# Read in bioclim data as a spatraster for use with terra
temp<-rast("wc2.1_30s_bio_1.tif", crs("+proj=longlat +datum=WGS84"))
precip<-rast("wc2.1_30s_bio_12.tif", crs("+proj=longlat +datum=WGS84"))


# tell R where the long/lat is in the dataframe and the crs
# Code snippet from Tyler Smith at AAFC

occs_ls <- terra::vect(occ, geom = c("decimalLongitude",
                              "decimalLatitude"),
                crs = "+proj=longlat +datum=WGS84")

# use spatsample (terra) to thin data to one observation per cell BUT per species


results<-NULL
  for(i in (unique(occs_ls$species))){
    this.species<- spatSample(occs_ls[occs_ls$species==i,], size=1, strata=temp)
    my_sf<-sf::st_as_sf(this.species)
    results<-rbind(results, my_sf)
    print(i)}

    results1<-results %>% dplyr::select(-c(X.1, X))
    sf::st_write(results1, "thinned_data.csv", layer_options = "GEOMETRY=AS_XY")
# Why does st_write not include geometry points?? This is literally the only reason to write a
# dataframe as an sf object!!!!!!!!!!!!!!!!!!

# Write results object into csv file
write.csv(results, "thinned_with_geometry.csv")
thinned_with_geometry <- read_csv("thinned_with_geometry.csv")

# Extract climate data

thinned_with_geometry$temp <-terra::extract(temp, thinned_with_geometry)
thinned_with_geometry$precip <-terra::extract(precip, thinned_with_geometry)

# Nice and easy! Now we have a thinned dataframe with all species, with their associated climate data

