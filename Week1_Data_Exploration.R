### Exploring legume data in R

### Call libraries
library(tidyverse)
library(purrr)
library(filesstrings)
library(rgbif)
library(taxize)
library(readr)
library(here)

# MB: Goals:
# - get GBIF occurrence data: 
#   - requires getting gbif key associated with each species name
#   - checking number of occurrences
#   - querying gbif to prepare download
#   - downloading compressed file and slimming this down to coordinates


# MB: existing hurdles as I see them:
# - add a gbif data cleaning step, maybe with this https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html
# - see if taxonomic issues need further work (e.g., names that return both accepted and synonymous taxa, names that return ONLY synonymous taxa)



### First, read in legume data and filter out species with fewer than 50 occurrences
# Note: Sure, I can filter this stuff out, but the number of occurrences in Pooja's dataset and
# in GBIF are very different, so sometimes I end up randomly selecting species that have >50
# occurrences in P's data, but have <50 in gbif
# MB: interesting, we should ask Megan F about the source of Pooja's occurrence counts. A possible change to the workflow is to merge on occurence counts from gbif before filtering. 
# MB: this could also be caused by taxonomic discrepancies, e.g., if some keys are pulling up synonyms rather than accepted names

legume <- read.csv("legume_range_traits.csv") #%>% 
  #filter(Count >= 50)

### Create subset of species with no symbiotic relationship

# But what about fungi? It's in the dataset, but very spotty data.

nonsym <- legume %>% 
  filter(fixer==0 & Domatia == 0 & EFN==0)

### Subset species that ARE in a symbiotic relationship (fixer OR domatia OR EFN)

sym <- legume %>% 
  filter(fixer==1 | Domatia == 1 | EFN==1)

### Randomly select 3 symbiotic and 3 nonsymbiotic species

# randomsym <- sample_n(sym, 3)
# randomnsym <- sample_n(nonsym, 3)


### Combine into one dataframe
# minidat <- rbind(randomsym, randomnsym)

# MB: convention is to always put the object you are assigning to on the left eg. newdata <- filter(oldata, x)
# MB: Now that you have chosen 6, for reproducibility, replacing the above with a step that will always sample the same 6 species. (e.g., filter to the species names of the six you've been working with). That'll make it easier for us to jointly troubleshoot code. 


#minidat <- legume %>% 
  #filter(Phy %in% c("Inga splendens", 
                    "Macrolobium pendulum", 
                    "Phaseolus parvulus", 
                    "Saraca asoca", 
                    "Taralea cordata", 
                    "Crudia amazonica"))

#minidat<-legume
#### Now, I want to pull in gbif occurrence data 

# Make list of taxonomic keys

species_names <- legume$Phy

# grabbing gbif data-- this code mostly comes from Takuji's Range limits proj w/
# Amy and Megan Oldfather
# grab gbif data using "backbone"

gbif_taxon_keys <- 
  species_names %>% 
  # MB: Pass names of species to gbif to get ID numbers (keys) associated with them
  get_gbifid_(method="backbone") %>% 
  # MB: This returns a named list of data frames, one data frame per species with a number of rows equal to the number of possible matches
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  # MB: This step adds a new column to each dataframe (.x) in the list that contains the original scientific name queried (the index/name of the list item, .y). THis column is called original_sciname
  bind_rows() #%>%
  # MB: This step combines all the listed dataframes into one big one. 
  # write_tsv(path = "all_matches.tsv") %>% 
  # MB: Omitting this writeout step until it becomes clear we need it. It's probably for a visual check of whether keys are good matches?
  
 
  gbif_taxon_keys<-filter(gbif_taxon_keys, matchtype=="EXACT" & status=="ACCEPTED") %>% 

 
  # MB: get rid of fuzzy matches (these have different spellings and are mostly the wrong species, e.g., Crudia amazonica vs. Clusia amazonica)
  # MB: Important: do keys with the status "SYNONYM"give unique and legitimate occurrence points? Or will occurrences under synonymous names be returned by the accepted name? We should look this up or test it out. 
  # filter(kingdom == "Plantae") %>%
  # MB: Commented out the above because hopefully it's not necessary and reduces the generality of the code
  select(usagekey, original_sciname) %>%
  tibble()


# Write gbif taxon keys into a csv
# MB: Omitting this step because it's redundant with the table that could be written out above and not clear what it's for, can add it back in if needed later
# write.csv(gbif_taxon_keys, ("gbif_taxon_keys.csv"))

# gbif_taxon_keys <- read.csv("gbif_taxon_keys.csv")
# MB: what's the purpose of writing out then reading in? 
# colnames(gbif_taxon_keys) <- c("X", "keys", "taxon")


# Get occurrence counts for each species

for (i in 1:length(gbif_taxon_keys$usagekey)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$usagekey[i], hasCoordinate = TRUE)
}

# filter out species with less than 50 occurrences
  
gbif_taxon_keys_filtered<-gbif_taxon_keys%>% filter(occ_count>=50)
  
# MB: Deleted another write out/read in step


# Okay, but now, let's download all the occurrences for these species
# First print key names???

# Request that gbif prepare the downloads

for (i in 1:length(gbif_taxon_keys_filtered$usagekey)){
  print(occ_download_queue(occ_download(pred_and(pred("taxonKey", gbif_taxon_keys_filtered$usagekey[i]), pred("hasCoordinate", TRUE)), user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com', format = "SIMPLE_CSV")))
  # MB: Tried switching format from DWCA to SIMPLE_CSV which might be adequate for our purposes
  print(gbif_taxon_keys_filtered$usagekey[i])
  print(gbif_taxon_keys_filtered$original_sciname[i])
  print(i)
}

# MB: this step sends a request to gbif to prepare occurrences for download. the print() wrappers make the output print so you can watch progress, this is useful if something goes wrong and you need to start up where you left off
# MB: found the function occ_download_queue which makes sys.sleep unnecessary
# MB: There may be some improvements that could be made on this step:
# 1. Bundling many taxa into one gbif download might be efficient
# 2. Adding some other preds that omit records that will get removed anyways when you QC geographic data would be efficient

# MB: The thing I'm stuck on here is getting the correct download keys for actually downloading the files. The list of keys returned in the step below will not necessarily be in the order that you requested them--the order could be altered if you have made the same request before or if some request are a lot larger than others. I'm sure we can figure this out in a makeshift way but there is probably an elegant solution too. 

# View all your download requests
View(occ_download_list(user = 'erin_m', pwd ='Dawson2023#')[[2]])
# MB: indices above are to use the second item in a list (first item is just metadata) and then select the first x rows (number of rows should correspond to the number of species you're downloading occurrences for, so I replaced it). selects first 5 columns, are these ones we need?

# Make a list of download keys
# MB: in my current situation, the 6 most recent requests are the six I want, but this might not always be the case!
dl_keys <- occ_download_list(user = 'erin_m', pwd ='Dawson2023#', limit = 1000)[[2]][1:6, 1]

for (i in 1:length(dl_keys$key)){
  occ_download_get(key = paste(dl_keys$key[i]), path = "zip_files")
  # MB: just download to zip_files and save the step of moving later. 
  print(i)
}

# Unzip files
file_names<- list.files("zip_files")
walk(file_names, ~ unzip(zipfile = str_c("zip_files/", .x), 
                         exdir = str_c("zip_out/", .x), overwrite = TRUE))

# Extract occurrence files from each folder and rename them to match the species they are

for(i in 1:length(dl_keys$key)){
  # occdata <- read.delim(paste0("zip_out/", dl_keys$key[i], "/occurrence.txt")), na.strings="", encoding = "UTF-8", header=T)  # grab occurrence txt file
  occdata <- read.delim(paste0("zip_out/", dl_keys$key[i], ".zip", "/", dl_keys$key[i], ".csv"), na.strings="", encoding = "UTF-8", header=T)  # grab occurrence txt file
  spname <- gsub(" ", "_", unique(occdata$species)) # grab species name
  spname <- strsplit(spname, split = " ")[[1]] # MB: necessary?
  
  # MB: Left off here
  if(identical(dir(pattern=spname), character(0))){ # if the directory does not have the species name already...
    dl_spname[i] <- spname # save species names as vector
    setwd(here("~/Mutualism_Range_Project_2024/zip_out/"))
    file.rename(list.files(pattern=dl_keys$key[i]), spname) # then rename to species name
  }else{ # otherwise...
    dup_names[i] <- spname # save species names as duplicate downloads to check later
    next # continue loop
  }
  
  print(paste(dl_keys$key[i], spname, i, sep = "_"))
  
}
write.csv(dl_spname, here("~/Mutualism_Range_Project_2024/dl_spname.csv"))
write.csv(dup_names, here("~/Mutualism_Range_Project_2024/duplicatenames.csv"))

# This code still doesn't rename the occurrence files themselves, but DOES rename the folders,
# without scrambling the encoding. So it's a step up

# Might need to move all renamed occurrence files into their own folder at this step


### map occurrence data
### here, I am trialling some things
library(dplyr)

occurrence_dat<-list.files(path="~/Mutualism_Range_Project_2024/zip_out/", pattern="occurrence.txt", recursive=TRUE)

#for (i in occurrence_dat){
  
#}


read.delim("occurrence.txt",header=T)->Crudia_ama

write.csv(Crudia_ama, "C_amazonica_occ.csv")


### Adding in Pooja's polygon data
# read in shapefiles
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

crudia_polygon<-susbet(spatial_polygons_sf, species=="Crudia amazonica")

### Adding in climate data
install.packages("geodata")
install.packages("raster")
library(geodata)
library(raster)

# Try downloading average temp for just one continent that Crudia is found in (because like, it should
# be found in one continent??)
# worldclim_global("tavg", 10, "world", version="2.1")


setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/South America/wc2.1_10m")
climfiles<-list.files(pattern = "*.tif")

str(wc2.1_10m_tavg_01.tif)

for (i in 1:length(climfiles)){
  climdat<-raster(climfiles[i])
}

tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(climdat, col=tempcol(100))

k <- leaflet(Crudia_ama) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~decimalLongitude, ~decimalLatitude, popup=Crudia_ama$species)
k %>% 
  addMarkers(crudia_pol)->k_shapefilecomp

library(ggplot2)
# install.packages("sf")
library(sf)


### Trying this using ggplot and mapdata

world <- map_data("world")

### convert raster data to dataframe for ggplot
climdat_df <- raster::as.data.frame(climdat, xy=TRUE)  

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  geom_raster(data=climdat_df, aes(x=x, y=y,fill=climdat_df$wc2.1_10m_tavg_12))+
  geom_point(data=Crudia_ama, aes(x=decimalLongitude, y=decimalLatitude, colour="Crudia amazonica"), pch=20, size=2)+
  geom_sf(data=subset(spatial_polygons_sf, species == "Acacia adunca"), aes(fill = num_species))






=======
### Exploring legume data in R
### Install packages
# install.packages("filesstrings")

### Call libraries
library(tidyverse)
library(purrr)
library(filesstrings)

### First, read in legume data and filter out species with fewer than 50 occurrences
# Note: Sure, I can filter this stuff out, but the number of occurrences in Pooja's dataset and
# in GBIF are very different, so sometimes I end up randomly selecting species that have >50
# occurrences in P's data, but have <50 in gbif

legume<-read.csv("legume_range_traits.csv")
str(legume)
legume1 <-legume %>% filter(Count>=50)

### Filter species with no symbiotic relationship
# But what about fungi? It's in the dataset, but very spotty data.
nonsym<-legume1 %>% subset(fixer==0 & Domatia == 0 & EFN==0)

### Subset species that ARE in a symbiotic relationship (fixer OR domatia OR EFN)
sym<-legume1 %>% subset(fixer==1 | Domatia == 1 | EFN==1)

### Randomly select 3 symbiotic and 3 nonsymbiotic species
sample_n(sym, 3)-> randomsym
sample_n(nonsym, 3)->randomnsym

### Combine into one dataframe
rbind(randomsym, randomnsym)->minidat


#### Now, I want to pull in gbif occurrence data 

## I will need rgbif, taxize, reader, and here

# install.packages("rgbif")
# install.packages("taxize")
# install.packages("readr")
# install.packages("here")
library(rgbif)
library(taxize)
library(readr)
library(here)


# Make list of taxonomic keys

keys <- minidat$Phy

# grabbing gbif data-- this code mostly comes from Takuji's Range limits proj w/
# Amy and Megan Oldfather
# grab gbif data using "backbone"

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
# identify the folders
current.folder <- "C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024"
new.folder <- "C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/zip_files"

# find the files that you want
list.of.files <- list.files(pattern="*.zip")

# copy the files to the new folder
file.copy(list.of.files, new.folder)
>>>>>>> aafa2407a5dfb79ebfc97cd3a32564be4603c7eb

file_names<- list.files("zip_files")

walk(file_names, ~ unzip(zipfile = str_c("zip_files/", .x), 
                         exdir = str_c("zip_out/", .x)))

# Get rid of the .zip part of the unzipped files

# setwd("~/Mutualism_Range_Project_2024/zip_out")
# file.rename(list.files(), sub(".zip", "",list.files()))
# MB: this seems like a bad idea--why get rid of a file extension? 

# reset working directory
# setwd("~/Mutualism_Range_Project_2024")

setwd("~/Mutualism_Range_Project_2024/zip_out")
file.rename(list.files(), sub(".zip", "",list.files()))

# reset working directory
setwd("~/Mutualism_Range_Project_2024")


# rename files with species names
# try i=1 or i=10, comment out the brackets so it's not a loop, and try that
# for occ data, does each step do the right thing, or is it getting messed up?
# if kept: dataframe with key and species name, make loop and go through, for i in length, for each
### occurrence file, grab taxon name, make dataframe where i append key and spname, which key is which 
### name. close loop and create second loop where for all keys in dataframe, and then for each key, load
### in occurrence and write it to a new file with taxonomic name, rename using write.csv(spname, occdata)
### okay, so in better words:
### I want this loop to take the taxonomic names for each species from the occurrence file, and I want
### to link this information to dl_keys so that I have a file with each key and it's taxon
### I then want to create a second loop in which I add the taxonomic name onto the end of each
### occurrence file and put it in a new folder called "occurrence data"



for(i in 1:length(dl_keys$key)){ # for all the keys in dl_keys

  occdata <- read.delim(here(paste0("zip_out/", paste0(dl_keys$key[i], ".zip"), "/occurrence.txt")))  # grab occurrence txt file

  spname <- gsub(" ", "_", unique(occdata$species)) # grab species name and adhere to end of existing file name
  spname <- strsplit(spname, split = " ")[[1]]
  if(identical(dir(pattern=spname), character(0))){ # if the directory does not have the species name already...
    dl_spname[i] <- spname # save species names as vector
    setwd(here())
    file.rename(list.files(pattern=dl_keys$key[i]), spname) # then rename to species name
  }
  
  print(paste(dl_keys$key[i], spname, i, sep = "_"))
  
}

rbind(dl_keys, spname)->write.csv(here("dl_spname.csv"))

write.csv(dl_spname, here("dl_spname.csv"))



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

# Might need to move all renamed occurrence files into their own folder at this step


### map occurrence data
### here, I am trialling some things
# Call leaflet
# install.packages("leaflet")
# install.packages("rgdal")
library(leaflet)
library(dplyr)
library(rgdal)

# Trying to map just one file

setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/zip_out/0004908-240202131308920")

read.delim("occurrence.txt",header=T)->Crudia_ama

write.csv(Crudia_ama, "C_amazonica_occ.csv")


### Adding in Pooja's polygon data
# read in shapefiles
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

crudia_polygon<-susbet(spatial_polygons_sf, species=="Crudia amazonica")

### Adding in climate data
install.packages("geodata")
install.packages("raster")
library(geodata)
library(raster)

# Try downloading average temp for just one continent that Crudia is found in (because like, it should
# be found in one continent??)
# worldclim_global("tavg", 10, "world", version="2.1")


setwd("C:/Users/erinm/OneDrive/Documents/Mutualism_Range_Project_2024/South America/wc2.1_10m")
climfiles<-list.files(pattern = "*.tif")

str(wc2.1_10m_tavg_01.tif)

for (i in 1:length(climfiles)){
  climdat<-raster(climfiles[i])
}

tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(climdat, col=tempcol(100))

k <- leaflet(Crudia_ama) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~decimalLongitude, ~decimalLatitude, popup=Crudia_ama$species)
k %>% 
  addMarkers(crudia_pol)->k_shapefilecomp

library(ggplot2)
# install.packages("sf")
library(sf)


### Trying this using ggplot and mapdata

world <- map_data("world")

### convert raster data to dataframe for ggplot
climdat_df <- raster::as.data.frame(climdat, xy=TRUE)  

ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  #coord_sf(xlim = c(50,160), ylim=c(-70,-30))+
  geom_raster(data=climdat_df, aes(x=x, y=y,fill=climdat_df$wc2.1_10m_tavg_12))+
  geom_point(data=Crudia_ama, aes(x=decimalLongitude, y=decimalLatitude, colour="Crudia amazonica"), pch=20, size=2)+
  geom_sf(data=subset(spatial_polygons_sf, species == "Acacia adunca"), aes(fill = num_species))



