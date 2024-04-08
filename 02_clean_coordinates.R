# Cleaning coordinates from gbif occurrences

# Load packages
library(tidyverse)
library(CoordinateCleaner)
library(countrycode)

# Consider adding here a rationale for which tests from coordinate cleaner you're going to apply/not apply so you have it later if you need to remember
# Options are "capitals", "centroids", "countries", "duplicates", "equal", "gbif, "institutions", "outliers", "ranges", "seas", "urban", "validity", "zeros" 
# I think "urban" and "ranges" require additional information from us.

# Testing version of dataset
occ_data <- read.csv("test_df.csv") %>% 
  select(-1) 
  # Omitting the first column of index numbers (adding these is a write.csv behavior that can be prevented with row.names = FALSE unless you want them)
  # Could consider reducing to columns of interest per the coordinatecleaner vignette, but first should look through and see if there are any others we might need.
    # select(species, decimalLongitude,
    # decimalLatitude, countryCode, individualCount,
    # gbifID, family, taxonRank, coordinateUncertaintyInMeters,
    # year, basisOfRecord, institutionCode)
  # You could then have a smaller dataframe in your environment and that might be easier on the computer
  
names(occ_data)

# Full version
# occ_data <- read_delim("0008106-240229165702484.csv")

head(occ_data)
summary(occ_data)

# transforming country codes from iso2c to iso3c (apparently this is required for using )
# Skip this step for the test dataset because I think it was already done before it was written out
# occ_data$countryCode <- countrycode(occ_data$countryCode, 
#                                      origin =  'iso2c',
#                                      destination = 'iso3c')

head(occ_data$countryCode)

table(occ_data$species)


# Trying coordinate cleaner for each species separately
abrus <- occ_data %>% subset(species == "Abrus fruticulosus")

flags_abrus <- clean_coordinates(x = abrus, 
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))

acacia <- occ_data %>% subset(species == "Acacia acinacea")

flags_acacia <- clean_coordinates(x = acacia, 
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))

abrus_p <- occ_data %>% subset(species == "Abrus precatorius")

flags_abrus_p <- clean_coordinates(x = abrus_p, 
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))

# The "seas" test is slow
# So is "outliers"

# testing with basic switches
flags_occ_data <- clean_coordinates(x = occ_data, 
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    countries = "countryCode",
                                    species = "species",
                                    tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "gbif", "seas"))#,
                                    #country_ref=rnaturalearth:ne_countries('large', returnclass = "sf"))

# changing country ref to finer-scale map

flags_occ_data_ref <- clean_coordinates(x = occ_data, 
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    countries = "countryCode",
                                    species = "species",
                                    tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "gbif", "seas"),
                                    country_ref=rnaturalearth:ne_countries('110', returnclass = "sf"))

# Taking out gbif

flags_occ_data_ref <- clean_coordinates(x = occ_data, 
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    countries = "countryCode",
                                    species = "species",
                                    tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "gbif", "seas"),
                                    country_ref=rnaturalearth:ne_countries('large', returnclass = "sf"))
seas_flags<-cc_sea(
  occ_data,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  scale = 110,
  value = "flagged",
  buffer = 500)
  
# 7 min 20 sec on my old computer

# Remove species designations
occ_data_nospecies <- occ_data %>% 
  select(-species)

flags_occ_data_nospecies <- clean_coordinates(x = occ_data_nospecies, 
                                    lon = "decimalLongitude",
                                    lat = "decimalLatitude",
                                    species = NULL,
                                    countries = "countryCode",
                                    tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros")) 

# Without species indicated it won't run the outliers test, which indicates that it's doing it on a species-by-species basis

# Do we get the same number of records flagged if we do it for all 3 test species vs. each individually?

table(flags_abrus$.summary)
table(flags_abrus_p$.summary)
table(flags_acacia$.summary)
table(flags_occ_data$species, flags_occ_data$.summary)

summary(flags_occ_data)

# Same whether run together or separately

# Do these run faster with a map function?

# Here's the map() setup, but no, I don't think it's any faster, it's not parallel, just iterative so not really any different than running it on the full dataframe

flags_map = occ_data %>%
  # split into a list of dataframes
  group_split(species, .keep = TRUE) %>% 
  # apply fuction--that's the first argument after the ., then supply arguments to clean_coordinates
  map(., clean_coordinates, 
      species = "species",
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      countries = "countryCode",
      tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros")) %>%
  # bind back into columns
  list_rbind()
# 7 min 6 sec for the test dataset of 3 species and 17536 observations on my 2015 macbook air


# Explore these on maps

wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = flags_occ_data,
             aes(x = decimalLongitude, y = decimalLatitude, color = .summary, shape = species),
             size = 0.5) 

# Probably need to adjust the seas test parameters, looks like coastal occurrences are falling off. 


wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = flags_abrus,
             aes(x = decimalLongitude, y = decimalLatitude, color = .summary),
             size = 0.5) 


# Old code below



i=1

for (i in 1:length(occ_data$species)) {
  flags_[i] <- clean_coordinates(x = occ_data, 
                                 lon = "decimalLongitude"[i],
                                 lat = "decimalLatitude"[i],
                                 countries = "countryCode"[i],
                                 species = "species"[i])}


# trying out some stuff
# make test dataset
occ_data$species<-as.character(occ_data$species)
levels(occ_data$species)
test<-filter(occ_data, (species=="Abrus fruticulosus") | (species=="Abrus precatorius") | (species=="Acacia acinacea")) %>% droplevels()

# Remove NA values from test dataset
test1<-test %>% group_by(species) %>% filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
head(test1$decimalLatitude)
levels(test1$species)
write.csv(test1, "test_df.csv")


# Using for loops causes R to consider the entire dataset just as one group of occurrences--
# problematic, because coordinate cleaner looks at species ranges/polygons as a whole to find outliers!
# Using flag_[i] causes a problem, bc R does not recognize it


i=1
for(i in 1:length(unique(test1$species))){ 
  species_tmp <- unique(test1$species)[i] %>% droplevels()
  df.tmp <- filter(test1, species == as.character(species_tmp))
  flag_[i]<-clean_coordinates(x = df.tmp, 
                              lon = "decimalLongitude"[i],
                              lat = "decimalLatitude"[i],
                              countries = "countryCode"[i],
                              species = "species"[i],
                              tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros"))}
  
  # Using flag just as is also causes problems, throwing an NA column not found error
  for (i in 1:length(test1$species)) 
  {flag_[i]<-clean_coordinates(x = test1, 
                               lon = "decimalLongitude"[i],
                               lat = "decimalLatitude"[i],
                               countries = "countryCode"[i],
                               species = "species"[i],
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros"))}
  # Trying some more random stuff (representative of what I did, but not exhaustive):
  
  
  for (i in 1:unique(test1$species)){
    flag_[i]<-clean_coordinates(x = test1, 
                                lon = "decimalLongitude"[i],
                                lat = "decimalLatitude"[i],
                                countries = "countryCode"[i],
                                species = "species"[i],
                                tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros"))}
  
  
  for (i in 1:length(test2)) 
  {flag_[i]<-clean_coordinates(x = test2[i], 
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros"))}
  
  
  by(test2, flags <- clean_coordinates(x = test1, 
                                       lon = "decimalLongitude",
                                       lat = "decimalLatitude",
                                       countries = "countryCode",
                                       species = "species"))
  
  
  flags<- clean_coordinates(x = test1, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))
  
  
  
  
  # Overall, not working-- individual flag datasets for each species not being generated, and cleaner ends up flagging
  # every occurrence!
  
  
  # Trying to split test1 by species, which did not work when I tried to plug it into for loops and apply statements
  split(test1, test1$species, drop=FALSE)->test2
  str(test2)
  
  # sapply actually  works, but it only works when you don't use flags...also, it still treats 
  # the dataframe as one group of species! Yikes!
  sapply(split(test1,test1$species), 
         flags[""]<-clean_coordinates(x=test1),
         lon = "decimalLongitude",
         lat = "decimalLatitude",
         countries = "countryCode",
         species = "species",
         tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))
  
  # Tried splitting them into a bunch of different files to feed into a loop/apply statement
  # code modified from https://stackoverflow.com/questions/9713294/split-data-frame-based-on-levels-of-a-factor-into-new-data-frames
  
  for (i in unique(test1$species)) {
    len <- sum(test1$species == i)
    df <- data.frame(species = rep(i, len), 
                     decimalLatitude = test1[test1$species == i,"decimalLatitude"],
                     decimalLongitude = test1[test1$species == i,"decimalLongitude"],
                     countryCode = test1[test1$species == i,"countryCode"])
    assign(paste0("df_", i), df)
  }
  
  # making list of files
  my_dfs <- lapply(ls(pattern = "df_.*"), get)
  
  
  
  # Okay, this doesn't work even remotely...it flags everything
  sapply(my_dfs, 
         flags<-clean_coordinates(x=test1),
         lon = "decimalLongitude",
         lat = "decimalLatitude",
         countries = "countryCode",
         species = "species",
         tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas", "zeros"))
  
  ### Final thoughts: I think I need to move all of my newly generated single-species dataframes
  # into one single folder and create an apply function or for loop that will read them through. I 
  # ran out of time to do this, but will attempt next week
  