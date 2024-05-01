# Run clean coordinates function on all data
library(tidyverse)
library(readr)

# First, read in the 10.6 gigabyte data file
occ_data <- read_delim("0008106-240229165702484.csv")
occ_data$species<-as.factor(occ_data$species)
levels(occ_data$species)

objectclasses = unique(factor(occ_data$species))

# Remove occurrences with no lat/long
occ_cleaned<-occ_data %>% drop_na(decimalLatitude)

# Split data into a few different data frames to read into the function! Yayyy!

occ_reordered<-occ_cleaned %>% group_by(genus)

names(table(occ_data$genus))->genus_table


reorder_data <- occ_reordered[order(occ_reordered$genus),]

part_a<-reorder_data %>% filter(grepl("^A", genus)) %>% droplevels()
part_b<-reorder_data %>% filter(grepl("^B", genus)) %>% droplevels()
part_c<-reorder_data %>% filter(grepl("^C", genus)) %>% droplevels()
part_d<-reorder_data %>% filter(grepl("^D", genus)) %>% droplevels()


part_four_fg<-reorder_data %>% filter(grepl("^F|^G", genus)) %>% droplevels()


part_two_<-reorder_data %>% filter(grepl("^F|^G|^H|^I|^J", genus)) %>% droplevels()
part_three_klmno<-reorder_data %>% filter(grepl("^K|^L|^M|^N|^O", genus)) %>% droplevels()
part_four_pqrst<-reorder_data %>% filter(grepl("^P|^Q|^R|^S|^T", genus)) %>% droplevels()
part_five_uvw<-reorder_data %>% filter(grepl("^U|^V|^W", genus)) %>% droplevels()
part_six_xyz<-reorder_data %>% filter(grepl("^X|^Y|^Z", genus)) %>% droplevels()

# Check to make sure R has filtered for the right letters and dropped filtered-out values
unique(part_a$genus)
unique(part_one_bcde$genus)
unique(part_two_fghij$genus)
unique(part_three_klmno$genus)
unique(part_four_pqrst$genus)
unique(part_five_uvwxyz$genus)

# Feed each dataset into the coordinate cleaner
# removing the gbif test before I run because it doesn't really matter, and also
# we have species in Denmark

flags_part_one <- clean_coordinates(x = part_a, 
                  lon = "decimalLongitude",
                  lat = "decimalLatitude",
                  countries = "countryCode",
                  species = "species",
                  tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                  seas_ref=rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical', returnclass = "sf"),
                  seas_buffer=20)

# Okay well this took a VERY long time to run
# I am going to try with a revised function that uses 50 for seas_ref and gets rid of the 
#seas buffer

flags_two <- clean_coordinates(x = part_a, 
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                               seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"))
# This took 3 hours to run on 1624100 points
# So 3 hours to run about 10% of the data
# 3 hours x 10=30 hours of running total