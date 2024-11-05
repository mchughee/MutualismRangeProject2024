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

    results1<-results %>% dplyr::select(-c(X))
    sf::st_write(results1, "thinned_data.csv", layer_options = "GEOMETRY=AS_XY")
# Why does st_write not include geometry points?? This is literally the only reason to write a
# dataframe as an sf object!!!!!!!!!!!!!!!!!!

# Check to ensure that data is actually in the right format:')
thinned_data<-read.csv("thinned_data.csv")

# Okay great, it's working and read in! Now, make sure it's in sf format

thinned <- st_as_sf(x = thinned_data,                         
                    coords = c("X", "Y"))
# Tell R to read coordinates as WGS84
st_set_crs(thinned, 4326)->thinned


# Extract climate data

thinned$temp <-raster::extract(temp, thinned)
thinned$precip <-raster::extract(precip, thinned)

# Nice and easy! Now we have a thinned dataframe

