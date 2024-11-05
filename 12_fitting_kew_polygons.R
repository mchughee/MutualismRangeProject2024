# Calculating fit of powo/kew polygons. Do they fit our occurrence points well?

library(sf)
library(tidyverse)
library(ggplot2)
library(geojsonsf)


# Read in occurrence data
points <- read.csv("thindat_climadd_soilgridsadd.csv")

# Put points into sf (a la Megan B)
points_sf <- st_as_sf(x = points,
                      # Specify which columns are coordinates
                      coords = c("X", "Y"), 
                      # Tell R to read coordinates as WGS84
                      crs = 4326)


# Pull in the new polygons
poly_sf = st_read("powo_polygons/powo_polygons_sorted.shp")

# Filter my points down to only the species in the polygons (as there are fiften species in my dataset
# that are not in the polygons)

points_filtered<-points_sf %>% filter(species %in% poly_sf$spcs_nm)
points_filtered$species<-as.factor(points_filtered$species)
levels(points_filtered$species)


# Okay, now time to run through the big loop I made in 03 to check how good the fit is for
# all the points and the polygons for each species
# turning geodesic geometry off
sf_use_s2(FALSE)
for(i in (unique(points_filtered$species))){
  this.species<- st_intersects(st_make_valid(poly_sf[poly_sf$spcs_nm==i,]), points_filtered[points_filtered$species==i,], sparse=FALSE)
  assign(paste0("testoverlay", "_", i), this.species, envir = .GlobalEnv)}

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
#drops <- c("geometry")
#finaldf <- subset(finaldf, select = -c(geometry))

# calculate percent cover
finaldf$percent_cover<-(finaldf$num_in_polygon/finaldf$num_total)*100

# make histogram
hist(finaldf$percent_cover)

giant_headache<-subset(finaldf, percent_cover>100)
giant_headache<-giant_headache %>% select(-geometry)
write.csv(giant_headache, "list_powo_pols_greaterthan100.csv")


lesser_headache<-subset(finaldf, percent_cover<50)
lesser_headache<-lesser_headache %>% select(-geometry)
write.csv(lesser_headache, "list_powo_pols_lessthan100.csv")

# Running mapping code to check out if turning on planar (so turning OFF geodesic) is
# affecting the points
world = map_data('world')

giant_headache<-st_as_sf(giant_headache, sf_column_name="geometry")

rm(i)
i="Abrus_fruticulosus"
for (i in unique(giant_headache$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = poly_sf[poly_sf$species_name==i,], aes(fill = poly_sf[poly_sf$species_name==i,]$introduced), alpha = 0.2) +
    geom_sf(data = giant_headache[giant_headache$species==i,]$geometry, aes(geometry = geometry), size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("mapping_kew_polys/", species_j, ".pdf"), width = 14, height = 6)}

poly_sf$intrdcd<-as.factor(poly_sf$intrdcd)

sf_use_s2(FALSE)
for (i in unique(points_filtered$species)){
  species_j = i
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
    geom_sf(data = poly_sf[poly_sf$spcs_nm==i,], aes(fill = poly_sf[poly_sf$spcs_nm==i,]$intrdcd), alpha = 0.2) +
    geom_sf(data = points_filtered[points_filtered$species==i,], size = 0.5)+
    theme(legend.title=element_blank())
  ggsave(filename = str_c("planar_kew_polys/", species_j, ".pdf"), width = 14, height = 6)}

#aes(color = points_filtered[points_filtered$species==i,]$introduced),

