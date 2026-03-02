# Preparing to download GBIF data

# Read in the packages we need
library(rgbif)
library(tidyverse)
library(here)

# taxize was removed from CRAN and the problem is still getting resolved
# Until then, we're using this method of installing taxize
install.packages("taxize", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
library(taxize)

# Read in the data file that contains the names of the species we want occurrences for
legume <- read.csv("data/legume_range_traits.csv")

gbif_taxon_keys <- 
  legume$Phy %>% 
  # Pass names of species to gbif to get ID numbers (keys) associated with them
  get_gbifid_(method="backbone") %>% 
  # This returns a named list of data frames, one data frame per species with
  #a number of rows equal to the number of possible matches
  imap(~ .x %>% mutate(original_sciname = .y)) %>%
  # This step adds a new column to each dataframe (.x) in the list that contains
  # the original scientific name queried (the index/name of the list item, .y). THis column is called original_sciname
  bind_rows()%>%
  filter(matchtype=="EXACT" & status=="ACCEPTED") %>% 
  # get rid of fuzzy matches (these have different spellings and are mostly 
  #the wrong species, e.g., Crudia amazonica vs. Clusia amazonica)
  filter(kingdom == "Plantae") %>%
  select(usagekey, original_sciname) %>%
  tibble()

# get occurrence numbers
for (i in 1:length(gbif_taxon_keys$usagekey)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$usagekey[i], hasCoordinate = TRUE)
}

# filter for species that have 50+ occurrences
gbif_taxon_keys_filtered <- gbif_taxon_keys %>% filter(occ_count>=50)

###### ask gbif to prepare some downloads

# Prepare download request-- this spins it up on the gbif website
occ_download(
  pred_in("taxonKey", gbif_taxon_keys_filtered$usagekey),
  format = "SIMPLE_CSV",
  #fill in your own GBIF username, password, and email here
  user = "", pwd ='', email = ''
)

# Now, we can actually download the data from gbif
# You need to wait a little while for gbif to actually process the download
# R may finish running the previous function, but if you log into your gbif
# account it will still be running

#The GBIF download request should generate a unique key 
occ_download_get(key="0008106-240229165702484", overwrite = FALSE)
# Again, wait for the download

# unzip the file!
unzip("0008106-240229165702484.zip")

# Done! We now have a giant database with all the occurrences we need, and
# it's unzipped and ready to work with
