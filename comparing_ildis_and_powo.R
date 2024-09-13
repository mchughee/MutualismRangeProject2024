# compare coverage of powo polygons (yay) and ildis polygons (BOOOOOOOO)

# Read in list of ildis polygons with less than 50% coverage
ildis_50<-read.csv("lessthan50percentcov.csv")

# Read in list of powo polygons with less than 50% coverage
powo_50<-read.csv("list_powo_pols_lessthan50.csv")

# find overlapping species from the two datasets
overlap<-setdiff(ildis_50$species, powo_50$species)

unique(ildis_50$species)
unique(powo_50$species)

powo_50$in_ildis<-powo_50$species %in% ildis_50$species
overlap<-subset(powo_50, in_ildis=="TRUE")

overlap$species

poly_sf = st_read("powo_polygons/powo_polygons_sorted.shp")

overlap$reason_for_fit<-NA
overlap[1,]$reason_for_fit<-"used in landscaping in Cali/southwest US"
overlap[2,]$reason_for_fit<-"lot of obsv in china for some reason"
overlap[3,]$reason_for_fit<-"so rare that powo polygon is tiny"
overlap[4,]$reason_for_fit<-"ornamental is best guess. very popular bonsai tree"
overlap[5,]$reason_for_fit<-"endangered plant, but cultivated"
overlap[6,]$reason_for_fit<-"ornamental!"
overlap[7,]$reason_for_fit<-"just looks like bad depiction of range to me"
overlap[8,]$reason_for_fit<-"fit of north american introduced polygons is bad...in florida now"
overlap[9,]$reason_for_fit<-"just a bad fit...native to west africa, some points falling just outside pols"
overlap[10,]$reason_for_fit<-



  