# Preparing to download GBIF data

# Read in the packages we need
library(rgbif)

# taxize got kicked off of CRAN and the problem is still getting resolved
# Until then, we're using this method of installing taxize
install.packages("taxize", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
library(taxize)
library(tidyverse)

# Read in the data file that contains the names of hte species we're trying
# to get occurrences for
legume <- read.csv("legume_range_traits.csv")

# make list of the species names
species_names <- legume$Phy

gbif_taxon_keys <- 
  species_names %>% 
  # MB: Pass names of species to gbif to get ID numbers (keys) associated with them
  get_gbifid_(method="backbone") %>% 
  # MB: This returns a named list of data frames, one data frame per species with
  #a number of rows equal to the number of possible matches
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  # MB: This step adds a new column to each dataframe (.x) in the list that contains
  # the original scientific name queried (the index/name of the list item, .y). THis column is called original_sciname
  bind_rows()%>%
  filter(matchtype=="EXACT" & status=="ACCEPTED") %>% 
  # MB: get rid of fuzzy matches (these have different spellings and are mostly 
  #the wrong species, e.g., Crudia amazonica vs. Clusia amazonica)
  # MB: Important: do keys with the status "SYNONYM" give unique and legitimate 
  # occurrence points? Or will occurrences under synonymous names be returned by
  # the accepted name? We should look this up or test it out. 
  filter(kingdom == "Plantae") %>%
  select(usagekey, original_sciname) %>%
  tibble()

# get occurrence numbers

for (i in 1:length(gbif_taxon_keys$usagekey)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$usagekey[i], hasCoordinate = TRUE)
}

# filter for species that have 50+ occurrences

gbif_taxon_keys_filtered <- gbif_taxon_keys %>% filter(occ_count>=50)

# ask gbif to pretty please prepare some downloads

# grab usagekeys from gbif_taxon_keys and make it into a vector

taxon_keys <- gbif_taxon_keys_filtered$usagekey

# Prepare download request-- this spins it up on the gbif website

occ_download(
  pred_in("taxonKey", taxon_keys),
  format = "SIMPLE_CSV",
  user = "erin_m", pwd ='Dawson2023#', email = 'erinmchugh94@gmail.com'
)

# Now, we can actually download the data from gbif

occ_download_get(key="0008106-240229165702484", overwrite = FALSE)


# unzip files
# identify the folders you want to move the zipped gbif download to
# our current folder
current.folder <- "/symbiont/erin.mchugh/Files"
# and our new folder!
new.folder <- "/symbiont/erin.mchugh/Files/legume_data/occurrence_data"


# find the files that you want-- there should be just one big zipped file
list.of.files <- list.files(pattern="*.zip")

# copy the file to the new folder
file.copy(list.of.files, new.folder)

# old code
#file_names<- list.files("zip_files")

# set our working directory to the folder with our zipped file
setwd("/symbiont/erin.mchugh/Files/legume_data/occurrence_data")
# unzip the file!
unzip("0008106-240229165702484.zip")

# Done! We now have a giant datase with all the occurrences we need, and
# it's unzipped and ready to work with
