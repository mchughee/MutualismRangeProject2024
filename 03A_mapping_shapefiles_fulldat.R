# Overlaying occurrence data with Pooja's shapefiles

# Read in packages

library(ggplot2)
library(sf)
library(raster)
library(tidyverse)
library(CoordinateCleaner)
library(rnaturalearthdata)

# setwd
# setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

# First, read in test df
datall<-read.csv("thindat_climadd_soilgridsadd.csv")
df <- st_as_sf(x = datall,                         
               coords = c("decimalLongitude", "decimalLatitude"))
# Tell R to read coordinates as WGS84
st_set_crs(df, 4326)->df_1

#class(df_1)
#st_crs(df_1)

# read in shapefiles and whip em into shape. All code taken from Pooja's repo.
legume_pol <- readRDS("legume_range_polygons_data.rds")

tmp <- data.frame(code = NULL, index = NULL)
for (x in 1:length(legume_pol$polygon)) {
  if(legume_pol$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pol$polygon[[x]]@data$Code, index = x)), tmp)
}

legume_pol$code<-NA
for (x in 1:length(legume_pol$polygon)) {
  legume_pol[x, "code"] <- legume_pol$polygon[[x]]@data$Code 
}

spatial_polygons <- do.call(rbind, legume_pol$`polygon`)
spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data = legume_pol, match.ID = F)
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons_df)


# fix up the species names so they don't have a space inbetween
df_1$species<-gsub(" ", "_", df_1$species)
spatial_polygons_sf$species<-gsub(" ", "_", spatial_polygons_sf$species)

# sf_use_s2(TRUE)

# Suck it, R, I wrote a for loop AND it works for once

for(i in (unique(df_1$species))){
  this.species<- st_intersects(st_make_valid(spatial_polygons_sf[spatial_polygons_sf$species==i,]), df_1[df_1$species==i,], sparse=FALSE)
  assign(paste0("overlay", "_", i), this.species, envir = .GlobalEnv)}


# make list of st_intersect output objects
all <- mget(ls(pattern="overlay_*"), envir = globalenv())

# calculate number of trues across polygons, i.e. the number of occurrences
# falling within polygons.
results <- lapply(all, sum)
# make dataframe to look at results
overlayresults<-as.data.frame(do.call(rbind, results))

# Next step: I'd like to see how this compares to the total number of occurrences
# that we pulled from gbif

# get number of occurrences for each species (we kind of already have this, but
# this method seems easier than trying to deal with logical matrices)
#df_1 %>% 
 # group_by(species) %>% 
  #summarize(count=n(), species=species)->counts
# above code does not work due to summarize being DEPRECATED (!)

counts<-points_sf %>% 
  filter(species %in% poly_sf$spcs_nm) %>% 
  group_by(species) %>% 
  tally()

# bind with overlay results
cbind(overlayresults, counts)-> finaldf

# rename columns and drop the geometry column that got preserved for god knows what reason
names(finaldf)[names(finaldf) == 'V1'] <- 'num_in_polygon'
names(finaldf)[names(finaldf) == 'n'] <- 'num_total'
finaldf <- subset(finaldf, select = -c(geometry))

# calculate percentages
finaldf$percent_cover<-(finaldf$num_in_polygon/finaldf$num_total)*100

write.csv(finaldf, "polygon_occ_coverage.csv")

# what is the mean coverage?
mean(finaldf$percent_cover)
# 90.78% cover! Pretty good

# How many have coverage under 80%?
count(subset(finaldf, percent_cover<80))
# 543

# How many have coverage under 75%?
count(subset(finaldf, percent_cover<75))
# 441

# make histogram
hist(finaldf$percent_cover)

# Time to inspect. Let's start with under 50% coverage

giant_headache<-subset(finaldf, percent_cover<50)
write.csv(giant_headache, "lessthan50percentcov.csv")

# Using a method I already know works to validate that sums() is correctly
# grabbing true occurrences
overlay_Acacia_acinacea<-as.data.frame(overlay_Acacia_acinacea)
occ_true_acin<-rowSums(overlay_Acacia_acinacea == "TRUE")
occ_false_acin<-rowSums(overlay_Acacia_acinacea == "FALSE")

occ_true<-rowSums(overlay_Abrus_fruticulosus == "TRUE")
occ_false<-rowSums(overlay_Abrus_fruticulosus == "FALSE")

# doing it for one with 200% coverage (yeeks)
rowSums(overlay_Afzelia_rhomboidea == "TRUE")
rowSums(overlay_Afzelia_rhomboidea == "FALSE")

afz_rhom<-subset(datall, species=="Afzelia rhomboidea")

# Common-sense check time! I am going to try to map the points and polygons
# to do a visual check

# First, abrus fruticulosus

pol_frut<-subset(subset_polygon, species=="Afzelia_rhomboidea")
occ_frut<-subset(df_1, species=="Afzelia_rhomboidea")

world <- map_data('world')
world<-st_as_sf(world, coords = c("long", "lat"))

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_sf(data=subset(pol_frut), aes(fill = num_species))+
  geom_sf(data = occ_frut, color = "red", inherit.aes = T) 

# Next, acacia acinacea

pol_acacia<-subset(subset_polygon, species=="Acacia_acinacea")
occ_acacia<-subset(df_1, species=="Acacia_acinacea")

world <- map_data('world')
world<-st_as_sf(world, coords = c("long", "lat"))

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_sf(data=subset(pol_acacia), aes(fill = num_species))+
  geom_sf(data = occ_acacia, color = "red", inherit.aes = T) 


abr_frut<-subset(datall, species=="Abrus fruticulosus")
abr_prec<-subset(datall, species=="Abrus precatorius")
aca_acin<-subset(datall, species=="Acacia acinacea")
