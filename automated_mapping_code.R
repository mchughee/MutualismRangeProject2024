### Automating downloading and mapping occurrence data, mapping it with worldclim data and overlaying
### Anna's shapefiles
### Author: Erin McHugh
### Date: 13/02/2024

### Call libraries
library(tidyverse)
library(purrr)
library(filesstrings)
library(rgbif)
library(taxize)
library(readr)
library(here)
library(dplyr)
library(sp)
library(geodata)
library(raster)
library(ggplot2)

# Read in legume data and filter out species with less than fewer occurrences in the gbif file

legume<-read.csv("legume_range_traits.csv")
str(legume)
legume1 <-legume %>% filter(Count>=50)

### Filter species with no symbiotic relationship
# (I excluded fungi here for now, because the data is few and far between)
nonsym<-legume1 %>% subset(fixer==0 & Domatia == 0 & EFN==0)

### Subset species that ARE in a symbiotic relationship (fixer OR domatia OR EFN)
sym<-legume1 %>% subset(fixer==1 | Domatia == 1 | EFN==1)

### Randomly select 3 symbiotic and 3 nonsymbiotic species
sample_n(sym, 3)-> randomsym
sample_n(nonsym, 3)->randomnsym

### Combine into one dataframe
rbind(randomsym, randomnsym)->minidat

# Make list of taxonomic keys

keys <- minidat$Phy

# grabbing gbif data-- this entire code chunk (from gbif_taxon_keys to dup_names) 
# comes from Takuji, Amy, and Megan Oldfather

gbif_taxon_keys <- 
  keys %>% 
  get_gbifid_(method="backbone") %>% 
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  bind_rows() %>%
  write_tsv(path = "all_matches.tsv") %>% 
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>%
  filter(kingdom == "Plantae") %>%
  select(usagekey, canonicalname) %>%
  data.frame()

# write gbif taxon keys into a csv

write.csv(gbif_taxon_keys, ("gbif_taxon_keys.csv"))

# get actual occurrence data

gbif_taxon_keys <- read.csv("gbif_taxon_keys.csv")
colnames(gbif_taxon_keys) <- c("X", "keys", "taxon")

for (i in 1:length(gbif_taxon_keys$keys)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$keys[i], hasCoordinate = TRUE)
}
write.csv(gbif_taxon_keys, ("gbif_occ_count.csv"))

occ_data<-read.csv("gbif_occ_count.csv")
occ_data <- occ_data[ -c(1)]

# Okay, but now, let's download all the occurences for these species
# First print key names???

for (i in 1:length(gbif_taxon_keys$keys)){
  print(occ_download(pred_and(pred("taxonKey", gbif_taxon_keys$keys[i]), pred("hasCoordinate", TRUE)), user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com'))
  print(gbif_taxon_keys$keys[i])
  print(gbif_taxon_keys$taxon[i])
  Sys.sleep(120)}

# Okay, but now I want to count the number of occurrences in the gbif dataset
# Print download keys

occ_download_list(user = 'erin_m', pwd ='Dawson2023#')[[2]][1:5, 1:5]

# Make a list of download keys
dl_keys <- occ_download_list(user = 'erin_m', pwd ='Dawson2023#', limit = 1000)[[2]][1:5, 1:5]
length(unique(dl_keys$key))
write.csv(dl_keys, ("dl_keys.csv"))

# Download actual data

for (i in 1:length(dl_keys$key)){
  occ_download_get(key = paste(dl_keys$key[i]))
  print(i)
}

# change folder name to species name
dl_keys <- read.csv(here("dl_keys.csv")) # get download key
dl_keys$key <- as.character(dl_keys$key) # convert to character
setwd(here("")) %>% list.files(pattern="") # how many files?
dl_spname <- NA # create empty vector to store succesfully changed download species file names
dup_names <- NA 

# unzip files
# identify the folder you want to move things from, and the folder you want to move those things to
# This copying files code chunk is modified from https://amywhiteheadresearch.wordpress.com/2014/11/12/copying-files-with-r/
current.folder <- "C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024"
new.folder <- "C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/zip_files"

# find the files that you want
list.of.files <- list.files(pattern="*.zip")

# copy the files to the new folder
file.copy(list.of.files, new.folder)

file_names<- list.files("zip_files")

walk(file_names, ~ unzip(zipfile = str_c("zip_files/", .x), 
                         exdir = str_c("zip_out/", .x)))

# Get rid of the .zip part of the unzipped files
setwd("~/Mutualism_Range_Project_2024/zip_out")
file.rename(list.files(), sub(".zip", "",list.files()))

# reset working directory
setwd("~/Mutualism_Range_Project_2024")

### THIS freaking code chunk. This is Takuji's/Amy's/Megan's code, and it is supposed
# to rename downloaded occurrence files with their taxonomic names, but it keeps scrambling
# the data. Hence, it is a work in progress.

for(i in 1:length(dl_keys$key)){ # for all the keys in dl_keys
  occdata <- read.delim(paste0("zip_out/", dl_keys$key[i], "/occurrence.txt"))  # grab occurrence txt file
  spname <- gsub(" ", "_", unique(occdata$species)) # grab species name and adhere to end of existing file name
  spname <- strsplit(spname, split = " ")[[1]]
  if(identical(dir(pattern=spname), character(0))){ # if the directory does not have the species name already...
    dl_spname[i] <- spname # save species names as vector
    setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/zip_out")
    file.rename(list.files(pattern=dl_keys$key[i]), spname) # then rename to species name
  }else{ # otherwise...
    dup_names[i] <- spname # save species names as duplicate downloads to check later
    next # continue loop
  }
  
  print(paste(dl_keys$key[i], spname, i, sep = "_"))
  
}
write.csv(dl_spname, here("dl_spname.csv"))
write.csv(dup_names, here("duplicatenames.csv"))


### Bring in Anna's shapefiles and extract the actual polygons
# Sometimes working directory can get a little weird, so reset here
setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024")

# All of the code used to wrangle the polygons comes from Pooja's 2023 Proc B paper
# Read in legume polygons as a .rds file
legume_pol <- readRDS("legume_range_polygons_data.rds")

# Make a new empty dataframe
tmp <- data.frame(code = NULL, index = NULL)

# Make a loop-- for every polygon in the rds file, if the polygon has codes, put it into the
# tmp dataframe. Bind the tmp file with the actual polygon info
for (x in 1:length(legume_pol$polygon)) {
  if(legume_pol$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pol$polygon[[x]]@data$Code, index = x)), tmp)
}
# make an object caled unique_pols that includes the species code and the index
unique_pols <- legume_pol[tmp$index, ]
# cbind the two dataframes to make unique_pols
unique_pols <- cbind(unique_pols, tmp)

# make code full of NAs and replace with codes from polygons
legume_pol$code<-NA
for (x in 1:length(legume_pol$polygon)) {
  legume_pol[x, "code"] <- legume_pol$polygon[[x]]@data$Code 
}

# make a new dataframe that is a subset of legume_pol. Used to filter for only  species in their
# native range, but I axed that. Group by geographic code and summarise by num of distinct sp.
lspecies_counts <- subset(legume_pol) %>%
  group_by(code) %>%
  summarise(num_species = n_distinct(species))

# Merge with unique polygons
merge <- sp::merge(unique_pols, lspecies_counts, by.x="code", by.y="code")

spatial_polygons <- do.call(rbind, merge$`polygon`)
attribute_data <- merge[c("species", "num_species")]
spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data = attribute_data, match.ID = F)
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons_df)

# Download global climate data

# worldclim_global("tavg", 10, "world", version="2.1")

setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/world/wc2.1_10m")
climfiles<-list.files(pattern = "*.tif")

# read in climate files for each month

for (i in 1:length(climfiles)){
  raster(climfiles[i])->climdat
}


# Download world map
world <- map_data("world")

### convert raster data to dataframe for ggplot
climdat_df <- raster::as.data.frame(climdat, xy=TRUE)

# make loop to plot each species in zip_out files
for (i in 1:length("zip_out/")){
  read.delim(occurrence.txt)
  
}


