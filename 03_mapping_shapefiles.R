# Overlaying our occurrence data with Pooja's shapefiles
# Started 15 April 2024

# First, read in test df
test<-read.csv("test_df.csv")

# Map species occurrences using ggplot

library(ggplot2)
library(sf)
library(geodata)
library(raster)
library(tidyverse)
library(CoordinateCleaner)
library(rnaturalearthdata)
# install.packages("tmap")
library(tmap)

# Remove occurrences with no lat/long
occ_cleaned<-test %>% drop_na(decimalLatitude)

# Split data into a few different data frames to read into the function! Yayyy!

occ_reordered<-occ_cleaned %>% group_by(genus)

names(table(test$genus))->genus_table


reorder_data <- occ_reordered[order(occ_reordered$genus),]

flags_test<- clean_coordinates(x = reorder_data, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "iucn", "outliers", "seas", "zeros"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)

summary(flags_test)
summary(flags_test)

plot(flags_test, lon = "decimalLongitude", lat = "decimalLatitude")

# Exclude flags

dat_cl <- reorder_data[flags_test$.summary,]

### Trying this using ggplot and mapdata
# First, download clim data
worldclim_global("tavg", 10, "world", version="2.1")
world <- map_data("world")
world

### convert raster data to dataframe for ggplot
dat_cl <- raster::as.data.frame(dat_cl, xy=TRUE)  

dat_cl$species<-as.factor(dat_cl$species)

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  #geom_raster(data=climdat_df, aes(x=x, y=y,fill=world/wc2.1_10m_tavg_12))+
  geom_point(data=dat_cl, aes(x=decimalLongitude, y=decimalLatitude, colour=factor(species)), pch=20, size=2) #+
  #geom_sf(data=subset(spatial_polygons_sf, species == "Acacia adunca"), aes(fill = num_species))

### bring in climate data
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/world/wc2.1_10m")
climfiles<-list.files(pattern = "*.tif")


for (i in 1:length(climfiles)){
  climdat<-raster(climfiles[i])
}

tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(climdat, col=tempcol(100))

climdat_df <- raster::as.data.frame(climdat, xy=TRUE) 

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  geom_raster(data=climdat_df, aes(x=x, y=y,fill=wc2.1_10m_tavg_06))+
  geom_point(data=dat_cl, aes(x=decimalLongitude, y=decimalLatitude, colour=factor(species)), pch=20, size=2)#+
# okay, well the colours sure do suck, but we can come back to this

# Bring in range polygons
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")
legume_pol <- readRDS("legume_range_polygons_data.rds")

tmp <- data.frame(code = NULL, index = NULL)
for (x in 1:length(legume_pol$polygon)) {
  if(legume_pol$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pol$polygon[[x]]@data$Code, index = x)), tmp)
}
unique_pols <- legume_pol[tmp$index, ]
unique_pols <- cbind(unique_pols, tmp)

legume_pol$code<-NA
for (x in 1:length(legume_pol$polygon)) {
  legume_pol[x, "code"] <- legume_pol$polygon[[x]]@data$Code 
}

lspecies_counts <- subset(legume_pol, introduced_status == "N") %>%
  group_by(code) %>%
  summarise(num_species = n_distinct(species))

# Merge with unique polygons
merge <- sp::merge(unique_pols, lspecies_counts, by.x="code", by.y="code")


spatial_polygons <- do.call(rbind, merge$`polygon`)
attribute_data <- merge[c("species", "num_species")]
spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data = attribute_data, match.ID = F)
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons_df)

subset_polygon<-filter(spatial_polygons_sf, species=="Abrus fruticulosus"|species=="Abrus precatorius"|species=="Acacia acinacea")

# Try adding polygons on top of map

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  geom_raster(data=climdat_df, aes(x=x, y=y,fill=climdat_df$wc2.1_10m_tavg_12))+
  geom_sf(data=subset(subset_polygon), aes(fill = num_species))+
  geom_point(data=dat_cl, aes(x=decimalLongitude, y=decimalLatitude, colour=factor(species)), pch=20, size=2)+
  scale_color_manual(values=c('pink','salmon2', 'tomato2'))
# okay, well the colours sure do suck, but we can come back to this

### Okay, we've visualized the data-- but how to turn it into something usable?

coordinates(dat_cl) <- c("decimalLongitude", "decimalLatitude")
class(dat_cl)
extent(dat_cl)
crs(dat_cl)

plot(dat_cl)
plot(world, add=TRUE)

write.csv(dat_cl, "dat_test_clean.csv")
dat_sf <- st_read("dat_test_clean.csv")