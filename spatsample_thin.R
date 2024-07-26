# Thinning points with spatsample instead of spthin

# First, read in packages and data
library(rworldmap)
library(terra)

# Using the twenty species dataframe for right now
occ<-read.csv("twenty_sp.csv")
occ$species<-gsub(" ", "_", occ$species)

temp<-rast("wc2.1_30s_bio_1.tif")
precip<-rast("wc2.1_30s_bio_12.tif")



# tell R where the long/lat is in the dataframe and the crs
# Thank you, Tyler Smith of AAFC and Carlton Uni for making a helpful blog post about this

occs_ls <- vect(occ, geom = c("decimalLongitude",
                              "decimalLatitude"),
                crs = "+proj=longlat +datum=WGS84")

# Now, let's use spatsample (terra) to thin data to one observation per cell BUT per species



for(i in (unique(occs_ls$species))){
  this.species<- spatSample(occs_ls[occs_ls$species==i,], size=1, strata=temp)
  assign(paste0("thin", "_", i), this.species, envir = .GlobalEnv)}

# Now, let's look at the spatvectors generated
values(thin_Lotus_pedunculatus)
length(thin_Lotus_pedunculatus)

sum(occ$species=="Lotus_pedunculatus")

# Check that the function worked!

nearby(thin_Lotus_pedunculatus, y=NULL, distance=1)

# merge all spatvectors into one big vector
all <- mget(ls(pattern="thin_*"), envir = globalenv())

thinned_occ<-c(mget(grep(pattern = "thin_*", x = ls(), value = TRUE)))

# Okay, I mean, I can work with whatever this is haha...just lapply I think?

ext_func <- function(x) {
  climate <- terra::extract(c(temp, precip), x)
}


lapply(thinned_occ, function(x) lapply(x, ext_func))


thinned_occ1<-unlist(thinned_occ)




aca_clim <- cbind(aca_thin, terra::extract(c(temp, precip), aca_thin))


# trying it on one species

aca_acin<-subset(occ, species=="Acacia acinacea")

aca_acin<- vect(aca_acin, geom = c("decimalLongitude",
                                   "decimalLatitude"),
                crs = "+proj=longlat +datum=WGS84")

aca_thin<- spatSample(aca_acin, 1, strata=c(temp, precip))

# get weather records for one species
aca_clim <- cbind(aca_thin, terra::extract(c(temp, precip), aca_thin))
values(aca_clim)


# Get rid of occs with missing climate data
# Haha! Do NOT DO THAT 
aca_clim1 <- aca_clim[complete.cases(data.frame(aca_clim)), ]


thin_Lotus = terra::as.data.frame(thin_Lotus_pedunculatus,xy=TRUE)

