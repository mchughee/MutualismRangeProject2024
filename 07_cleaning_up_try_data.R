# Trying TRY (times are DESPERATE)

# Read in packages
library(terra)
library(sf)
library(tidyverse)

setwd("C:/Users/erinm/Downloads")

# Read in this freaking data
try<-read.delim("TRY_29_Data/35503.txt")
head(try)

pollination_syndrome<-subset(try, DataName=="Pollination syndrome (pollen vector)"|DataName=="Pollination syndrom 2"|
                               DataName=="Pollination syndrom: abiotic"|DataName=="Pollination syndrom: insects"|
                               DataName=="Pollination syndrom: vertebrates")
pollination_syndrome<-as.data.frame(pollination_syndrome)

# Read in dataset with my species
sp<-read.csv("summary_df_august2024.csv")

# put a little _ in between genus and species epithet for GIFT data so it matches the species summary df
pollination_syndrome$AccSpeciesName<-gsub(" ", "_", pollination_syndrome$AccSpeciesName)

# Figure out how many species are overlapping between GIFT and my data
overlap<-intersect(sp$species, pollination_syndrome$AccSpeciesName)
# 391 species that are in my dataset are in the GIFT pollination dataset

# Filter pollination dataset by sp in sp
pollination_syndrome1<-pollination_syndrome[pollination_syndrome$AccSpeciesName %in% sp$species,]

# Okay let's figure out what we're dealing with here. Will it be worth it to make this data usable?
pollination_syndrome1$OriglName<-as.factor(pollination_syndrome1$OriglName)
levels(pollination_syndrome1$OriglName)

pollination_syndrome1$OrigValueStr<-as.factor(pollination_syndrome1$OrigValueStr)
levels(pollination_syndrome1$OrigValueStr)

# Okay, this dataset is a mess. Time to clean 'er up
# cleaning pipeline: streamline OrigValueStr. Remove duplicate values and go with more conservative poll syn

# Change polllination_syndrome$work_species to just $species
names(pollination_syndrome1)[names(pollination_syndrome1) == 'AccSpeciesName'] <- 'species'
head(pollination_syndrome1)

# Find levels for OrigValueStr
levels(pollination_syndrome1$OrigValueStr)


pollclean <- pollination_syndrome1 %>%
  mutate(OrigValueStr = case_when( 
    OrigValueStr=="insect pollinated"|OrigValueStr=="bee"|
      OrigValueStr=="beetles"|OrigValueStr=="bumblebees"|
      OrigValueStr=="entomogamous"|OrigValueStr=="flies"|
      OrigValueStr=="hymenopteres"|OrigValueStr=="insects always"|
      OrigValueStr=="insects the rule"|OrigValueStr=="pollination animals"|
      OrigValueStr=="wasps"|OrigValueStr=="Animals"|
      OrigValueStr=="bees"|OrigValueStr=="bumblebees"|
      OrigValueStr=="butterflies"|
      OrigValueStr=="Entomophily"|OrigValueStr=="general insect"|
      OrigValueStr=="insect"|OrigValueStr=="insects often"|
      OrigValueStr=="syrphids"~ "biotic",
    OrigValueStr=="cleistogamy often"|OrigValueStr=="selfed"|OrigValueStr=="selfing often"|
      OrigValueStr=="selfing always"| OrigValueStr=="self"|OrigValueStr=="selfing the rule"~"abiotic",
    OrigValueStr=="anemogamous/entomogamous"|OrigValueStr=="selfing at failure of outcrossing"|
      OrigValueStr=="selfing rare"|OrigValueStr=="selfing unknown"|
      OrigValueStr=="3"|OrigValueStr=="cleistogamy rare"|
      OrigValueStr=="insects rare"|OrigValueStr=="insects uknown"|
      OrigValueStr=="selfing never"|OrigValueStr=="selfing possible"~"NA",
    DataName=="Pollination syndrom: vertebrates" & OrigValueStr=="yes"~"biotic",
    DataName=="Pollination syndrom: vertebrates" & OrigValueStr=="no"~"NA",
    DataName=="Pollination syndrom: insects" & OrigValueStr=="yes"~"biotic",
    DataName=="Pollination syndrom: insects" & OrigValueStr=="no"~"NA",
    DataName=="Pollination syndrom: abiotic" & OrigValueStr=="yes"~"abiotic",
    DataName=="Pollination syndrom: abiotic" & OrigValueStr=="no"~"biotic")) %>% 
  filter(OrigValueStr=="biotic"|OrigValueStr=="abiotic") %>% 
  droplevels()

# Check that I've dropped the NA values
pollclean$OrigValueStr<-as.factor(pollclean$OrigValueStr)

levels(pollclean$OrigValueStr)

# Remove duplicates!

# how many duplicated species are there?
pollclean$species<-as.factor(pollclean$species)
sum(duplicated(pollclean$species))
# 604 duplicates!!

# k, fork, let's look at which species are abiotically reproducing

pollclean$species<-as.factor(pollclean$species)

abiotic<-pollclean %>% subset(OrigValueStr=="abiotic") %>% droplevels()
biotic<-pollclean %>% subset(OrigValueStr=="biotic") %>% droplevels()



unique(abiotic$species)


intersect(abiotic$species, biotic$species)
setdiff(abiotic$species, biotic$species)

# there are a couple to several observations of each species, with conflicting reports of them being
# biotically vs abiotically pollinated hahaha yikes

# write some code that will conditionally choose one pollination mode to keep

for (i in unique(pollclean$species)){
  if(sum(pollclean[pollclean$species==i,]$OrigValueStr=="biotic")>sum(pollclean[pollclean$species==i,]$OrigValueStr=="abiotic")){
    pollclean[pollclean$species==i,]$OrigValueStr<-"biotic"
  }
  if(sum(pollclean[pollclean$species==i,]$OrigValueStr=="biotic")<sum(pollclean[pollclean$species==i,]$OrigValueStr=="abiotic")){
    pollclean[pollclean$species==i,]$OrigValueStr<-"abiotic"
  }
  if(sum(pollclean[pollclean$species==i,]$OrigValueStr=="biotic")==sum(pollclean[pollclean$species==i,]$OrigValueStr=="abiotic")){
    pollclean[pollclean$species==i,]$OrigValueStr<-"conflicted"
  }}


pollclean1<-pollclean %>%
  #arrange(species) %>%
  group_by(species) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(species)


duplicated(pollclean1$species)

subset(pollclean1, OrigValueStr=="abiotic")

subset(pollclean1, OrigValueStr=="conflicted")

subset(pollclean1, OrigValueStr=="biotic")

setdiff(pollclean1$species, pollination_syndrome1$species)
