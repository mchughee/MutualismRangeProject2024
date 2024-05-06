### Bringing in world clim data, layering data with sf, etc.


library(sf)
library(terra)
library(tidyverse)
library(raster)
#install.packages("shapefiles")
library(shapefiles)

str(test)


# Download bioclim variables
library(raster)

# turn df into an sf object
test<-read.csv("dat_test_clean.csv")
df <- st_as_sf(x = test,                         
               coords = c("decimalLongitude", "decimalLatitude")
)


### Never mind! I will try to overlay climate and occurrence data first because that's
# what we care about most

# First, read in climate raster data
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/wc2.1_30s_bio")
# climfiles<-list.files(pattern = "*.tif")
temp<-raster("wc2.1_30s_bio_1.tif")
precip<-raster("wc2.1_30s_bio_12.tif")

# check crs
crs(temp)
crs(precip)

# plot data
plot(temp)
plot(precip)

# Now, point data
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

climate_mean <- raster::extract(clim, # the raster that you wish to extract values from
                                df, # a point, or polygon spatial object
                                buffer=0.5,
                                fun = mean, # extract the MEAN value from each plot
                                sp = TRUE)

climate_df<-as.data.frame(climate_mean)

plot(clim)

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  geom_raster(data=clim, aes(x=x, y=y,fill=wc2.1_10m_tavg_06))+
  geom_point(data=df, aes(x=decimalLongitude, y=decimalLatitude, colour=factor(species)), pch=20, size=2)#+
# okay, well the colours sure do suck, but we can come back to this


