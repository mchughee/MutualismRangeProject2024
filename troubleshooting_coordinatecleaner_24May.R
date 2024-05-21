# trying to resubset the full dataset and replicate coordinate cleaner to see where
# my errors are

# subset data
data<-read_delim("0008106-240229165702484.csv")
data$species=="Abrus fruticulosus"

head(data)
str(data)
data$species<-as.factor(data$species)
levels(data$species)

sum(data$species == "Abrus fruticulosus")
sum(data$species == "Abrus precatorius")

test_data<-filter(data, species=="Abrus fruticulosus" | species=="Abrus precatorius" | species=="Acacia acinacea")
sum(test_data$species == "Abrus fruticulosus")
# Okay, 1559 occurrences for Abrus fruticulosus


test_subset<-subset(data, species=="Abrus fruticulosus" | species=="Abrus precatorius" | species=="Acacia acinacea")

test_cleaned<-test_data %>% drop_na(decimalLatitude)
sum(test_cleaned$species == "Abrus fruticulosus")
# 895 once we remove occurrences with no associated coordinates-- maybe look into this later? 
# That's a lot of occurrences with no associated coordinates

# Update: I'm back and ready to look at why this is
Abr_frut<-subset(data, species=="Abrus fruticulosus")


# what happens if we clean the test dataset?
flags_test<- clean_coordinates(x = test_cleaned, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)

dat_fl_test <- test_cleaned[flags_test$.summary,]
summary(flags_test)

sum(dat_fl_test$species == "Abrus fruticulosus")
# 854, which checks out and is what I got for the test originally
sum(dat_fl_test$species == "Acacia acinacea")
# 8442
sum(dat_fl_test$species == "Abrus precatorius")
# 6715

# This all checks out and is what I have in the test dataset (-2 occurrences? I think
# I used an older coord cleaner combo to clean the test dataset and never fixed it because
# it doesn't really matter


### Now, what if we used the grep function we used on the big dataset to try
# and replicate our test dataset

grep_test_abrfrut<-data %>% filter(grepl("Abrus fruticulosus", species)) %>% droplevels()
# 1559 again
grep_test_abrprec<-data %>% filter(grepl("Abrus precatorius", species)) %>% droplevels()
grep_test_acaacin<-data %>% filter(grepl("Acacia acinacea", species)) %>% droplevels()

test_grepl<-rbind(grep_test_abrfrut, grep_test_abrprec, grep_test_acaacin)

# Checking whether the full grepl function I used in 02_cleaning_coordinates isn't dropping stuff

abr_occ_data<-subset(data, species=="Abrus fruticulosus")
# 1559

occ_cleaned<-data %>% drop_na(decimalLatitude)

occ_reordered<-occ_cleaned %>% group_by(genus)
abr_occ_reordered<-subset(occ_reordered, species=="Abrus fruticulosus")
# 895 now

reorder_data <- occ_reordered[order(occ_reordered$genus),]
sum(reorder_data$species == "Abrus fruticulosus")
# okayyyy 895, once again

part_a<-reorder_data %>% filter(grepl("^A", genus)) %>% droplevels()
sum(part_a$species == "Abrus fruticulosus")
# 895 once again

# Okay, I didn't grep too close to the sun...but then what is happening? I am going to run all the A genera
# through coordinate cleaner again

flags_a<- clean_coordinates(x = part_a, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)

# Okay, let's see how the math on this works out

summary(flags_a)
dat_a_test <- part_a[flags_a$.summary,]
sum(dat_a_test$species == "Abrus fruticulosus")
sum(dat_a_test$species == "Acacia acinacea")
sum(dat_a_test$species == "Abrus precatorius")

# SCREAMING! Crying! Throwing up! It is 389 for Abrus frut! So when I run coordinate cleaner on all the A species,
# I get 389 occurrences left over; when I run just a few species, I get 854
# 8470  for Acacia acinaeca
# 2858 for Abrus prec
# Too many people using symbiont right now, BUT I do have the old output

dat_clean_a <- read_csv("dat_clean_a.csv")
sum(dat_clean_a$species == "Abrus fruticulosus")
# Okay, when I clean all the As at once, I get 389 occurrences at the end.
# When I clean just the three species, I get 800 something at the end.
# So the problem is a coordinate cleaner issue. Frick!

# If I run it on a slightly larger set than the test set, do I get a different number at the end?

part_a_six_sp<-part_a %>% filter(species=="Abrus fruticulosus" | species=="Abrus precatorius" | species=="Acacia acinacea" |
                          species=="Acacia acradenia" | species=="Acacia acuminata" | species=="Acacia adsurgens") %>% droplevels()


flags_a_six_sp<- clean_coordinates(x = part_a_six_sp, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)

dat_six_test <- part_a_six_sp[flags_a_six_sp$.summary,]
summary(flags_a_six_sp)

sum(dat_fl_test$species == "Abrus fruticulosus")
# 854 once again

# Let's try it on a larger subset of A values. I'm going to run it on 
part_a_large<-reorder_data %>% filter(grepl("^Ab|^Ac", genus)) %>% droplevels()
part_a_large$species<-as.factor(part_a_large$species)
levels(part_a_large$species)

sum(part_a_large$species == "Abrus fruticulosus")

flags_a_largeset<- clean_coordinates(x = part_a_large, 
                                   lon = "decimalLongitude",
                                   lat = "decimalLatitude",
                                   countries = "countryCode",
                                   species = "species",
                                   tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                   seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                                   seas_buffer = 25)

dat_a_largeset <- part_a_large[flags_a_largeset$.summary,]
summary(flags_a_largeset)
sum(dat_a_largeset$species == "Abrus fruticulosus")
# now, we get 533 occurrences leftover
# feck, I think it's time to work on a loop:/

sum(dat_a_largeset$species == "Acacia acinacea")
# 8470! same as a

sum(dat_a_largeset$species == "Abrus precatorius")
# 4021