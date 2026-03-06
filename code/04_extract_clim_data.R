# Extracting environmental and biome data for thinned occurrences

# Load packages
library(raster)
library(sf)
library(dplyr)
library(tidyverse)
library(terra)
library(geodata)

# Read in thinned data

thin <- read_csv("data_large/allocc_thinned.csv")

# Convert to sf object
thin <- st_as_sf(x = thin, coords = c("X", "Y")) %>% 
# Tell R to read coordinates as WGS84
 st_set_crs(., 4326)

# 1. Read in raster climate data -----

temp <- raster::raster("data_large/wc2.1_30s_bio_1.tif")
precip <- raster::raster("data_large/wc2.1_30s_bio_12.tif")
crs(temp) <- 4326
crs(precip) <- 4326

# Extract climate data

thin$temp <- raster::extract(temp, thin)
thin$precip <- raster::extract(precip, thin)

rm(temp, precip)

# 2. Extracting SoilGrids data -----

# Pull nitrogen raster data
nitrogen <- soil_world_vsi("nitrogen", 15, stat = "mean")
st_crs(nitrogen)

thin_nit_proj = thin %>% 
  select(species, genus, countryCode, geometry) %>% 
  st_transform(crs = st_crs(nitrogen))

# plot(thin[1:10000,])
# plot(thin_nit_proj[1:10000,])

nit_extract <- terra::extract(nitrogen, thin_nit_proj)

thin$nitrogen = nit_extract$`nitrogen_5-15cm_mean`

summary(thin)

sum(is.na(thin$nitrogen))/nrow(thin)
# 0.1338635

sum(is.na(thin$temp))/nrow(thin)
# 0.009887009

sum(is.na(thin$precip))/nrow(thin)
# 0.009887009

# test = thin_nit_proj[1:500,]
# compare = thin[1:500,]
# 
# nitro_test <- terra::extract(nitrogen, test, method = "bilinear")
# nitro_test2 <- raster::extract(nitrogen, test)
# 
# compare = thin[1:500,]
# summary(nitro_test)
# summary(nitro_test2)
# summary(compare)


# write_csv(thin$nitrogen, "data_large/thin_w_nit.csv")
# saveRDS(thin, "data_large/thin_w_nit.rds")

rm(nitrogen, thin_nit_proj)


# 3. Extracting biome data for occurrence points -----

# Download wwf data
download.file("https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip", 
              destfile = "data_large/wwf_biome_data.zip")
# If the above doesn't work just copy-paste the URL into the browser, move file to data_large, modify next step as needed

# Unzip data file
unzip("data_large/6kcchn7e3u_official_teow", exdir = "data_large/wwf_biome_data")

# Read in wwf ecoregions shapefile
shapes <- st_read("data_large/wwf_biome_data/wwf_terr_ecos.shp")

ggplot(shapes[1:2000,]) +
  geom_sf(fill = "#69b3a2", color = "white") 

# make sure crs is the same for both datafiles
st_crs(thin)
st_crs(shapes)

biome_names = tibble(biome = 1:14, 
                     biome_name = c("Tropical and subtropical moist broadleaf forests",
                     "Tropical and subtropical dry broadleaf forests",
                     "Tropical and subtropical coniferous forests",
                     "Temperate broadleaf and mixed forests",
                     "Temperate coniferous forests",
                     "Taiga and Boreal forest",
                     "Tropical and subtropical grasslands savannas and shrublands",
                     "Temperate grasslands savannas and shrublands",
                     "Flooded grasslands and savannas",
                     "Montane grasslands and shrublands",
                     "Tundra",
                     "Mediterranean forests woodlands and scrub",
                     "Deserts and xeric shrublands",
                     "Mangroves"))

# Extract biome for lat and long of all occurrences
sf_use_s2(FALSE)
biome_join <- st_join(thin, shapes, join=st_intersects, left=TRUE, largest=FALSE) %>% 
  select(species, genus, countryCode, geometry, temp, precip, nitrogen, biome = BIOME) # %>% 
  # left_join(., biome_names) # can join these on later when needed

# Common-sense check: does the distribution of species across biomes know what
# we know to be true, i.e. most falling in temperate/tropical/mediterranean areas
table(biome_join$biome_name)/nrow(biome_join)


# We have just a few occurrences falling into "rock and ice" and "lake" (98 and 99)
# this problem will be resolved when we choose the biome that the most occurrences of each species falls in

# let's inspect a few species to see what the results of the join are
lupine <- filter(biome_join, species=="Lupinus_nootkatensis")
milkvetch <- filter(biome_join, species=="Astragalus_echinatus")

# write into a csv file
sf::st_write(biome_join, "data_large/allocc_with_env.csv", layer_options = "GEOMETRY=AS_XY")



