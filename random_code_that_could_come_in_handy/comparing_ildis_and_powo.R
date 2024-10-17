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
overlap[10,]$reason_for_fit<-"extra points in mongolia"
overlap[11,]$reason_for_fit<-"occs falling into ocean, i fear"
overlap[12,]$reason_for_fit<-"native to s. europe as well, but polygons only cover north africa"
overlap[13,]$reason_for_fit<-"expansion/introduction into northern europe?"
overlap[14,]$reason_for_fit<-"shitty polygons"
overlap[15,]$reason_for_fit<-"yikes! basically all points in canada not in polygons, even though part of known range"
overlap[16,]$reason_for_fit<-"used as an ornamental in africa"
overlap[17,]$reason_for_fit<-"native to india, but bunch of occs in australia"
overlap[18,]$reason_for_fit<-"lots of occs in us, has usda page, looks like maybe recent introduction from se asia"
overlap[19,]$reason_for_fit<-"okay, this is a hybrid ornamental. why is it in the dataset?"
overlap[20,]$reason_for_fit<-"teeny polygon for species native to se australia and nz"
overlap[21,]$reason_for_fit<-"native to phillipines, but used widely as an ornamental, I suspect"
overlap[22,]$reason_for_fit<-"shitty polygons"
overlap[23,]$reason_for_fit<-"native to middle east, widely cultivated as forage crop"
overlap[24,]$reason_for_fit<-"native to middle east, widely cultivated as forage crop"
overlap[25,]$reason_for_fit<-"aka blue fenugreek, suspect it is cultivated for use in europe"
overlap[26,]$reason_for_fit<-"shitty polygons...lots of occs in northern europe, polygons cover southern europe"
overlap[27,]$reason_for_fit<-"native to se mediterranean, lots of occs in northern europe"
overlap[28,]$reason_for_fit<-"not many occs and many are falling outside of pols"

write.csv(overlap, "poor_fit_ildis_powo_both.csv")  
