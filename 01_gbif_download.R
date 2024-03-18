getwd()
install.packages("rgbif")
install.packages("taxize")
library(rgbif)
library(taxize)
library(tidyverse)

legume <- read.csv("legume_data/legume_range_traits.csv")

species_names <- legume$Phy

gbif_taxon_keys <- 
  species_names %>% 
  # MB: Pass names of species to gbif to get ID numbers (keys) associated with them
  get_gbifid_(method="backbone") %>% 
  # MB: This returns a named list of data frames, one data frame per species with a number of rows equal to the number of possible matches
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  # MB: This step adds a new column to each dataframe (.x) in the list that contains the original scientific name queried (the index/name of the list item, .y). THis column is called original_sciname
  bind_rows()%>%
  filter(matchtype=="EXACT" & status=="ACCEPTED") %>% 
  
  
  # MB: get rid of fuzzy matches (these have different spellings and are mostly the wrong species, e.g., Crudia amazonica vs. Clusia amazonica)
  # MB: Important: do keys with the status "SYNONYM"give unique and legitimate occurrence points? Or will occurrences under synonymous names be returned by the accepted name? We should look this up or test it out. 
  # filter(kingdom == "Plantae") %>%
  # MB: Commented out the above because hopefully it's not necessary and reduces the generality of the code
  select(usagekey, original_sciname) %>%
  tibble()

# get occurrence numbers

for (i in 1:length(gbif_taxon_keys$usagekey)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$usagekey[i], hasCoordinate = TRUE)
}

# filter for species that have 50+ occurrences

gbif_taxon_keys_filtered<-gbif_taxon_keys%>% filter(occ_count>=50)

# ask gbif to pretty please prepare some downloads for me

# for (i in 1:length(gbif_taxon_keys_filtered$usagekey)){
#print(occ_download_queue(occ_download(pred_and(pred("taxonKey", gbif_taxon_keys_filtered$usagekey[i]), pred("hasCoordinate", TRUE)), user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com', format = "SIMPLE_CSV")))
# MB: Tried switching format from DWCA to SIMPLE_CSV which might be adequate for our purposes
#print(gbif_taxon_keys_filtered$usagekey[i])
#print(gbif_taxon_keys_filtered$original_sciname[i])
#print(i)
#}

# let's try downloading without using a for loop

# grab usagekeys from gbif_taxon_keys and make it into a little big vector

taxon_keys <- gbif_taxon_keys_filtered$usagekey

# occ_download_queue(
# occ_download(pred_and(pred("taxonKey", taxon_keys[1:500]), pred("hasCoordinate", TRUE)),
# format = "SIMPLE_CSV",
# user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com'))

occ_download(
  pred_in("taxonKey", taxon_keys),
  format = "SIMPLE_CSV",
  user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com'
)

# get downloads

occ_download_get(key="0008106-240229165702484", overwrite = FALSE)

# unzipping the file

# unzip files
# identify the folders
current.folder <- "/symbiont/erin.mchugh/Files"
new.folder <- "/symbiont/erin.mchugh/Files/legume_data/occurrence_data"

# find the files that you want
list.of.files <- list.files(pattern="*.zip")

# copy the files to the new folder
file.copy(list.of.files, new.folder)

file_names<- list.files("zip_files")

setwd("/symbiont/erin.mchugh/Files/legume_data/occurrence_data")
unzip("0008106-240229165702484.zip")

occ_data<-read_delim("0008106-240229165702484.csv")
head(occ_data)
# row names are probably species, causing problems
# use n_max in read_ series to read in just a few of the observations

# install coordinate cleaner
install.packages("CoordinateCleaner")
library(CoordinateCleaner)

# transforming country codes from iso2c to iso3c (apparently this is required for using )

# trying out clean coordinates wrapped function

install.packages("countrycode")
library(countrycode)

# change country cod efrom iso2 to iso3

occ_data$countryCode <-  countrycode(occ_data$countryCode, 
                                origin =  'iso2c',
                                destination = 'iso3c')

head(occ_data$countryCode)

i=1

for (i in 1:length(occ_data$species)) {
  flags_[i] <- clean_coordinates(x = occ_data, 
                                 lon = "decimalLongitude"[i],
                                 lat = "decimalLatitude"[i],
                                 countries = "countryCode"[i],
                                 species = "species"[i])}

# Trying coordiante cleaner for one species
vicia<-occ_data %>% subset(species=="Vicia villosa")

flags<-clean_coordinates(x = vicia, 
                         lon = "decimalLongitude",
                         lat = "decimalLatitude",
                         countries = "countryCode",
                         species = "species",
                         tests = c("capitals", "centroids", "equal", "institutions", "outliers", "seas","zeros"))



# trying out some stuff
# make test dataset
occ_data$species<-as.character(occ_data$species)
levels(occ_data$species)
test<-filter(occ_data, (species=="Abrus fruticulosus") | (species=="Abrus precatorius") | (species=="Acacia acinacea")) %>% droplevels()
write.csv(test, "test_df.csv")
# Remove NA values from test dataset
test1<-test %>% group_by(species) %>% filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
head(test1$decimalLatitude)
levels(test1$species)


# Using for loops causes R to consider the entire dataset just as one group of occurrences--
# problematic, because coordinate cleaner looks at species ranges/polygons as a whole to find outliers!
# Using flag_[i] causes a problem, bc R does not recognize it


i=1
for (i in 1:length(unique(test1$species)){ 
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




                  