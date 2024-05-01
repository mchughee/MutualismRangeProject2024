# Finally running coordinate cleaner lol
# 15 April 2024

# read in full dataset
occ_data <- read_delim("0008106-240229165702484.csv")

# Remove occurrences with no lat/long
occ_cleaned<-occ_data %>% drop_na(decimalLatitude)

# Split data into a few different data frames to read into the function! Yayyy!

occ_reordered<-occ_cleaned %>% group_by(genus)

names(table(occ_data$genus))->genus_table


reorder_data <- occ_reordered[order(occ_reordered$genus),]

part_a<-reorder_data %>% filter(grepl("^A", genus)) %>% droplevels()
part_b<-reorder_data %>% filter(grepl("^B", genus)) %>% droplevels()
part_cdef<-reorder_data %>% filter(grepl("^C|^D|^E|^F", genus)) %>% droplevels()
part_ghijk<-reorder_data %>% filter(grepl("^G|^H|^I|^J|^K", genus)) %>% droplevels()
part_lmnop<-reorder_data %>% filter(grepl("^L|^M|^N|^O|^P", genus)) %>% droplevels()
part_qrstuv<-reorder_data %>% filter(grepl("^Q|^R|^S|^T|^U|^V", genus)) %>% droplevels()
part_wxyz<-reorder_data %>% filter(grepl("^W|^X|^Y|^Z", genus)) %>% droplevels()



# seas buffer at 25 m
# running part a
flags_a<- clean_coordinates(x = part_a, 
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                                seas_buffer = 25)
# Look at summary
summary(flags_a)

plot(flags_a, lon = "decimalLongitude", lat = "decimalLatitude")

# get flags and clean dataset
dat_fl_a <- part_a[!flags_a$.summary,]

dat_cl_a <- part_a[flags_a$.summary,]

# bring in world map
world <- map_data("world")
world
dat_cl_a$species<-as.factor(dat_cl_a$species)
quality_a<-filter(dat_cl_a, species=="Aganope gabonica"| species=="Alexa imperatricis"|species=="Amorpha nana"|species=="Arachis diogoi"|species=="Acmispon americanus"|species=="Aeschynomene indica"|species=="Alysicarpus ovalifolius")


# plot data
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1)+
  geom_point(data=quality_a, aes(x=decimalLongitude, y=decimalLatitude, colour=factor(species)), pch=20, size=2)



# running part b
flags_b<- clean_coordinates(x = part_b, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)
# Look at summary
summary(flags_b)



# running part c
flags_cdef<- clean_coordinates(x = part_cdef, 
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                            seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                            seas_buffer = 25)
# Look at summary
summary(flags_cdef)



# running part ghijk
flags_ghijk<- clean_coordinates(x = part_ghijk, 
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                               seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                               seas_buffer = 25)
# Look at summary
summary(flags_ghijk)


# running part lmnop
flags_lmnop<- clean_coordinates(x = part_lmnop, 
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                                seas_buffer = 25)
# Look at summary
summary(flags_lmnop)


# running part qrstuv
flags_qrstuv<- clean_coordinates(x = part_qrstuv, 
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                                seas_buffer = 25)
# Look at summary
summary(flags_qrstuv)


# running part wxyz
flags_wxyz<- clean_coordinates(x = part_wxyz, 
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 tests = c("capitals", "centroids", "equal", "institutions", "outliers", "zeros", "seas"),
                                 seas_ref=rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf"),
                                 seas_buffer = 25)
# Look at summary
summary(flags_wxyz)