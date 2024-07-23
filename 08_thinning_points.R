#### Thinning data

# First, read in data
library(terra)
library(spThin)
library(rworldmap)
library(plyr)


occ<-read.csv("allocc_clean.csv")


# Make species names have the ole _
occ$species<-gsub(" ", "_", occ$species)

# Try spThin with one species
aca_acin<-subset(occ, species=="Acacia_acinacea") %>% droplevels()
thin(aca_acin, lat.col="decimalLatitude", long.col="decimalLongitude", spec.col="species",
     thin.par=1, reps=2, locs.thinned.list.return=FALSE, write.files = TRUE, max.files=1, out.dir="/symbiont/erin.mchugh/MutualismRangeProject2024",
     out.base="aca_1k.csv")

# Read in csv file with thinned data
aca_1k<-read.csv("aca.csv_thin1.csv") %>% droplevels()

# Bring in maps
world <- map_data('world')
australia<-map_data('world', region = "Australia")



## Calculate nearest neighbour distance for thinned and unthinned data
dist<-st_distance(aca_1k$geometry)
sum(count(dist<1))
sum(count(dist>=1))
unlist(dist)


# Okay, probs not going to see any differences but nice to know it's actually being thinned
# Let's try it on the test data

test_df<-read.csv("test_df.csv")

# gsub function
test_df$species<-gsub(" ", "_", test_df$species)

# Try spThin with multiple species

for(i in (unique(test_df$species))){
  thin(test_df[test_df$species==i,], lat.col="decimalLatitude", long.col="decimalLongitude", spec.col="species",
       thin.par=1, reps=2, locs.thinned.list.return=FALSE, write.files = TRUE, max.files=1, out.dir="/symbiont/erin.mchugh/MutualismRangeProject2024",
       out.base=paste("thin", i, sep = ""))}



abru_fru<-subset(test_df, species=="Abrus_fruticulosus") %>% droplevels()

abru_prec<-subset(test_df, species=="Abrus_precatorius") %>% droplevels()

aca_acin<-subset(test_df, species=="Acacia_acinacea") %>% droplevels()


# Calculate matrix
aca_thin <- st_as_sf(x = thinAcacia_acinacea_thin1_new,                         
                     coords = c("decimalLongitude", "decimalLatitude"))

dist<-st_distance(aca_thin$geometry)

as.data.frame(dist)

sum(count(dist<1))
sum(count(dist>=1))


