# extracting biome data for each species based on median lat and long

library(sf)
library(tidyverse)
library(ggplot2)

# read in datafile with our species and med lat/long
dat<-read_csv("data_files/invasiveclass_thindat_climadd_soilgridsadd.csv")

dat_sf<-st_as_sf(x = dat, coords = c("X", "Y"))
st_set_crs(dat_sf, 4326)->dat_sf

#download wwf data
download.file("https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip", 
              destfile = "data_files/wwf_biome_data.zip")

# unzip data file
unzip("data_files/wwf_biome_data.zip",exdir="data_files/wwf_biome_data") 

# read in wwf ecoregions shapefile

shapes<-st_read("data_files/wwf_biome_data/official/wwf_terr_ecos.shp")
plot(st_geometry(shapes))

ggplot(shapes) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

# make sure crs is the same for both datafiles
st_crs(dat_sf)

# Extract biome for median lat and median long for each species

sf_use_s2(FALSE)
join<-st_join(dat_sf, shapes, join=st_intersects, left=TRUE, largest=FALSE)


# Common-sense check: does the distribution of species across biomes know what
# we know to be true, i.e. most falling in temperate/tropical/mediterranean areas

hist(join$BIOME)
join$BIOME<-as.factor(join$BIOME)
levels(join$BIOME)

## we have just a few occurrences falling into "rock and ice" and "lake"
# prevents us from seeing real trends in histo

subset(join, BIOME=="98" | BIOME=="99")
# new histo to inspect
filtered<-join %>% filter(BIOME!="98" & BIOME!="99")
hist(as.numeric(filtered$BIOME))
# Okay, this problem will be resolved when we choose
# the biome that the most occurrences of each species falls in

# let's inspect a few species to see what the results of the join are
lupine<-filter(join, species=="Lupinus_nootkatensis")
milkvetch<-filter(join, species=="Astragalus_echinatus")


# Remove extraneous columns from the biome dataframe

join1<-join %>% select(-c(G200_REGIO, G200_NUM, G200_STAT, ECO_NUM,
                          ECO_ID, ECO_SYM, eco_code, PER_area, PER_area_1,
                          PER_area_2, AREA))

# write into a csv file
sf::st_write(join1, "invasiveclass_thinnedoccs_soil_clim_biome.csv", layer_options = "GEOMETRY=AS_XY")

duplicated(str(join1))


