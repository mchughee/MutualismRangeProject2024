### Bringing in bioclim data to extract temps and precips from occurrence points

#setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

library(sf)
library(terra)
library(tidyverse)
library(raster)
library(maps)

# read in csv folder containing cleaned data points
df<-read.csv("allocc_clean.csv")
# force into a simple features object
df_2<-st_as_sf(x = df,                         
               coords = c("decimalLongitude", "decimalLatitude"))
# kinda sketch here, but the occurrence points do not have an encoded crs. 
# I think gbif uses wgs84 (aka 4326) crs (most data does), and I was worried about
# it not lining up properly with wgs84 polygons, so I just made R read it as wgs84
df_1<-st_set_crs(df_2, 4326)


# Next, read in climate raster data
# don't need this step in symbiont
# setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/wc2.1_30s_bio")
# climfiles<-list.files(pattern = "*.tif")
temp<-raster("wc2.1_30s_bio_1.tif")
precip<-raster("wc2.1_30s_bio_12.tif")

# check crs to make sure it's all good
crs(temp)
crs(precip)

# plot data just to see what the tifs look like
plot(temp)
plot(precip)

# Now, reset working directory
# following step not needed on symbiont
# setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

# extracting climate data-- initially, I tried using raster::extract, but it 
# required me to pick a function like mean or max-- I don't need a function! 
# I've already got a whole layer of mean data. I found terra:: raster and decided
# it would be a better fit

# temp first
<<<<<<< HEAD
# temp_data<-terra::extract(temp, df_1)
df_1$temp <-terra::extract(temp, df_1)
#temp_df<-as.data.frame(temp_data)

# precip second
# precip_data<-terra::extract(precip, df_1)
df_1$precip <-terra::extract(precip, df_1)
#precip_df<-as.data.frame(precip_data)

# merge extracted data with the old dataframe containing points and attributes
clim_df<-cbind(precip_df, temp_df, df_1)
# cbind literally just puts columns together
# try using df_1$temp <-terra::extract(temp, df_1)
=======
#temp_data<-terra::extract(temp, df_1)
#temp_df<-as.data.frame(temp_data)

df_1$temp <-terra::extract(temp, df_1)

# precip second
#precip_data<-terra::extract(precip, df_1)
#precip_df<-as.data.frame(precip_data)

df_1$precip <-terra::extract(precip, df_1)
>>>>>>> 1481910 (symbiont push)

# remove unnecessary data

finaldf = dplyr::select(df_1, -c("kingdom", "phylum", "class", "order", "recordNumber",
                                 "identifiedBy", "dateIdentified", "rightsHolder", 
                                "typeStatus", "establishmentMeans", "depth", "depthAccuracy"))

# save new dataframe (write as csv)
write.csv(finaldf, "clim_data_june2024.csv")

# check levels
finaldf$species<-as.factor(finaldf$species)
levels(finaldf$species)

# write a smaller test dataset
testdata<-subset(finaldf, species=="Acacia acinacea" | species=="Crotalaria mitchellii" | species=="Vicia cracca" |
                   species== "Dalbgergia melanoxylon")


<<<<<<< HEAD
# Checking to make sure I didn't get the coordinates messed up. Looks like  I didn't
# still clustering around continents in the southern hemisphere
plot(precip)
plot(st_geometry(df_1$geometry), col = sf.colors(1), border = 'grey', 
     axes = TRUE, add=TRUE)

df_1$precip<-as.numeric(df_1$precip)
mean(df_1$precip, na.rm=TRUE)

df_1$temp<-as.numeric(df_1$temp)
mean(df_1$temp, na.rm=TRUE)

=======
# write into a csv file

#write.csv(testdata, "test_data_new_june2024.csv", quote=TRUE, row.names = FALSE)

st_write(testdata, "test_data_new_june2024_coords.csv", layer_options = "GEOMETRY=AS_XY")
>>>>>>> 1481910 (symbiont push)


<<<<<<< HEAD
world <- map_data('world')
world<-st_as_sf(world, coords = c("long", "lat"))
st_set_crs(world, 4326)->world


ggplot() +
  #geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_raster(data=precip)+
  geom_sf(data = df_1$geometry, color = "red") 

# Checking to see how points fall on top of climate data
# Don't run this, it nearly broke R
# precip <- raster::as.data.frame(precip, xy=TRUE)  
# ggplot()+
  # geom_raster(precip, mapping = aes(x=x, y=y))+
  # geom_point(data=test, aes(x=decimalLongitude, y=decimalLatitude))
=======
>>>>>>> 1481910 (symbiont push)

