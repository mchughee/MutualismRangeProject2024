# Thinning points with spatsample instead of spthin

# First, read in packages and data
library(rworldmap)
library(terra)

# Using the twenty species dataframe for right now, but replace with full data when the time comes
occ<-read.csv("twenty_sp.csv")
occ$species<-gsub(" ", "_", occ$species)

# Read in bioclim data as a spatraster for use with terra
temp<-rast("wc2.1_30s_bio_1.tif", crs("+proj=longlat +datum=WGS84"))
precip<-rast("wc2.1_30s_bio_12.tif", crs("+proj=longlat +datum=WGS84"))


# tell R where the long/lat is in the dataframe and the crs
# Code snippet from Tyler Smith at AAFC

occs_ls <- vect(occ, geom = c("decimalLongitude",
                              "decimalLatitude"),
                crs = "+proj=longlat +datum=WGS84")

# use spatsample (terra) to thin data to one observation per cell BUT per species

for(i in (unique(occs_ls$species))){
  this.species<- spatSample(occs_ls[occs_ls$species==i,], size=1, strata=temp)
  assign(paste0("thin", "_", i), this.species, envir = .GlobalEnv)}

# For loop is long, but gets the job done


# Now, let's look at the spatvectors generated for one species
values(thin_Lotus_pedunculatus)
length(thin_Lotus_pedunculatus)
sum(occ$species=="Lotus_pedunculatus")


# Check that the function worked by calculating distances between points

thin_mat<-terra::nearby(thin_Lotus_pedunculatus, y=NULL, distance=1, symmetrical=TRUE)

# Like, 0.0something percent are less than a km apart; my guess is that this is due to thinning per km raster cell,
# which means that in some cases, they are just a little less than 1 km apart

# merge all spatvectors into one big list

files<-mget(ls(pattern="thin_*"), envir = globalenv())

# Tell R to apply the st_as_sf function to each item in the "files" list
my_sf<-lapply(names(files), function(x) sf::st_as_sf(files[[x]]))

# bind together each sf dataframe in the "files" list
my_df <- do.call(rbind, my_sf)


# Extract climate data

my_df$temp <-terra::extract(temp, my_df)
my_df$precip <-terra::extract(precip, my_df)

# Nice and easy! Now we have a thinned dataframe with all species, with their associated climate data


