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

# Trying out different outlier methods
# first, mean absolute deviation
flags_five<-clean_coordinates(x = test_data, 
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              countries = "countryCode",
                              species = "species",
                              tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                              seas_ref=rnaturalearth::ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf"),
                              outliers_method = "mad")

# min distance to next record
flags_six<-clean_coordinates(x = test_data, 
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              countries = "countryCode",
                              species = "species",
                              tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                              seas_ref=rnaturalearth::ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf"),
                              outliers_method = "distance")

# trying it with tdi smaller
flags_seven<-clean_coordinates(x = test_data, 
                             lon = "decimalLongitude",
                             lat = "decimalLatitude",
                             countries = "countryCode",
                             species = "species",
                             tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                             seas_ref=rnaturalearth::ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf"),
                             outliers_method = "distance",
                             outliers_td=500)


summary(flags_one) # seas_ref at 10; buffer at 20; total flags=1197
# seas=863
plot(flags_one, lon = "decimalLongitude", lat = "decimalLatitude")

summary(flags_two) #seas_ref at 50; no buffer; total flags=1530
# seas=1288
plot(flags_two, lon = "decimalLongitude", lat = "decimalLatitude")

summary(flags_three) # seas_ref at 50; 20 m buffer; total flags=1526
# seas=1284
plot(flags_three, lon = "decimalLongitude", lat = "decimalLatitude")

summary(flags_four) # seas_ref at 110; no buffer; total flags=2452
# seas=2266
plot(flags_four, lon = "decimalLongitude", lat = "decimalLatitude")

# Capitals: removes records referenced to capital cities with a default buffer of 10 km
# Centroids: removes records within radius around country centroid
# Equal: removes records with equal lat/long ex 0, 0
# Institutions: removes records around botanical gardens, museums, universities w 100 m buffer
# Outliers: removes outliers using method one of -> quantile, mean absolute deviation, minimum distance
# Zeros: removes points at zero-zero lat-long
# Seas: restrict data to the land
#

