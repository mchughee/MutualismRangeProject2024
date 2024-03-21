library(rgbif)
library(taxize)
library(tidyverse)


legume <- read.csv("legume_range_traits.csv")

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
  # MB: Important: do keys with the status "SYNONYM" give unique and legitimate occurrence points? Or will occurrences under synonymous names be returned by the accepted name? We should look this up or test it out. 
  filter(kingdom == "Plantae") %>%

  select(usagekey, original_sciname) %>%
  tibble()

# get occurrence numbers

for (i in 1:length(gbif_taxon_keys$usagekey)) {
  gbif_taxon_keys$occ_count[i] <- occ_count(taxonKey = gbif_taxon_keys$usagekey[i], hasCoordinate = TRUE)
}

# filter for species that have 50+ occurrences

gbif_taxon_keys_filtered <- gbif_taxon_keys %>% filter(occ_count>=50)

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

# Full version
# occ_data <- read_delim("0008106-240229165702484.csv")

head(occ_data)
summary(occ_data)

# transforming country codes from iso2c to iso3c (apparently this is required for using )
# Skip this step for the test dataset because I think it was already done before it was written out
occ_data$countryCode <- countrycode(occ_data$countryCode, 
                                    origin =  'iso2c',
                                    destination = 'iso3c')

head(occ_data$countryCode)

table(occ_data$species)


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


                  