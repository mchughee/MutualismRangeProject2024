### Exploring climate data and smooshing into one dataframe

library(ggplot2)
library(dplyr)
library(tidyverse)

# First, read in climate data
test<-read.csv("test_data_new_june2024_coords.csv", header=TRUE)

# Make sure all the species are there
test$species<-as.factor(test$species)
levels(test$species)

# visualize data
ggplot(test, aes(x=precip))+
  geom_histogram(bins=50)+
  facet_wrap(~species)

ggplot(test, aes(x=temp))+
  geom_histogram(binwidth=10)+
  facet_wrap(~species)
# okay well I don't care to mess around with binwidth right now so

# subsetting dataset to keep individual species for sanity checks
aca_acin<-subset(test, species=="Acacia acinacea")
cro_mitc<-subset(test, species=="Crotalaria mitchellii")
vic_crac<-subset(test, species=="Vicia cracca")


# Smooshing part 1: get 95% quantile and 5% quantile

test1<-test %>% drop_na(precip) %>% drop_na(temp)

testingpr<-test1 %>% 
  group_by(species) %>% 
  reframe(max_precip=quantile(precip, 0.95), 
          min_precip=quantile(precip, 0.05),
          max_temp=quantile(temp, 0.95),
          min_temp=quantile(temp, 0.05))

# Is this command giving the correct values?

vic_crac_1<-vic_crac %>% drop_na(precip) %>% drop_na(temp)

quantile(vic_crac_1$precip, 0.95)
quantile(vic_crac_1$precip, 0.05)

quantile(vic_crac_1$temp, 0.95)
