# Trying to figure out what is making coordinate cleaner run so slowly

# Read in test data

test_data<-read.csv("test_df.csv")

library(CoordinateCleaner)

flags_one <- clean_coordinates(x = test_data, 
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    countries = "countryCode",
                                    species = "species",
                                    tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                    seas_ref=rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical', returnclass = "sf"))
                                    #seas_buffer=20)

### Took like 3 hours


### What if I try to coarsen the scale for seas_ref
flags_two <- clean_coordinates(x = test_data, 
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                               seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"))
### Okay like 10 minutes



### Add in buffer, and keep seas at 50

flags_three<- clean_coordinates(x = test_data, 
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                               seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                               seas_buffer = 20)
### 10 minutesish

### Add
flags_four<-clean_coordinates(x = test_data, 
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              countries = "countryCode",
                              species = "species",
                              tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                              seas_ref=rnaturalearth::ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf"))

summary(flags_one)
summary(flags_two)
summary(flags_three)
summary(flags_four)

# Capitals: removes records referenced to capital cities with a default buffer of 10 km
# Centroids: removes records within radius around country centroid
# Equal: removes records with equal lat/long ex 0, 0
# Institutions: removes records around botanical gardens, museums, universities w 100 m buffer
# Outliers: removes outliers using method one of -> quantile, mean absolute deviation, minimum distance
# Zeros: removes points at zero-zero lat-long
# Seas: restrict data to the land
#

