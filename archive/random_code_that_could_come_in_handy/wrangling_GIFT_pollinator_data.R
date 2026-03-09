# Trying to wrangle pollinator data using Ms. GIFT

install.packages("GIFT")
library(GIFT)

# Find the trait that I want-3.6.1
traits<-GIFT_traits_meta()

# Get the dataframe for the pollination syndrome data
# Using default settings used in GIFT tutorial because they all seem fine??
pollination_syndrome<-
  GIFT_traits(
  trait_IDs = "3.6.1",
  agreement = 0.66,
  bias_ref = TRUE,
  bias_deriv = TRUE,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)

# Read in dataset with my species
sp<-read.csv("summary_df_august2024.csv")

# put a little _ in between genus and species epithet for GIFT data so it matches the species summary df
pollination_syndrome$work_species<-gsub(" ", "_", pollination_syndrome$work_species)

# Figure out how many species are overlapping between GIFT and my data
overlap<-intersect(sp$species, pollination_syndrome$work_species)
# 391 species that are in my dataset are in the GIFT pollination dataset

# Filter pollination dataset by sp in sp
pollination_syndrome<-pollination_syndrome[pollination_syndrome$work_species %in% sp$species,]

# Change polllination_syndrom$work_species to just $species
names(pollination_syndrome)[names(pollination_syndrome) == 'work_species'] <- 'species'
head(pollination_syndrome)

# Append data from pollination syndrome to sp
master<-merge(pollination_syndrome, sp, all.y=TRUE, by="species") %>% 
  dplyr::select(-c(work_author, work_ID, n_3.6.1, agreement_3.6.1, references_3.6.1, X))

# Change pollination data column name to something more intuitive
names(master)[names(master) == 'trait_value_3.6.1'] <- 'poll_type'

# Write new csv file
write.csv(master, "summary_df_with_pollination.csv")





