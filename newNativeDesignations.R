library(tidyverse)
library(geojsonsf)
library(sf)

# Got list of native and introduced localities from
# https://powo.science.kew.org/ -> data -> Download WCVP data
# https://sftp.kew.org/pub/data-repositories/WCVP/?_gl=1*i3n1l7*_ga*NzIxMDU4MTQ5LjE3MjI3ODA5OTE.*_ga_ZVV2HHW7P6*MTcyMjc4MDk5MS4xLjEuMTcyMjc4MTExNi4wLjAuMA..
# wcvp_dwca.zip

# Got polygons from here 
# https://github.com/tdwg/wgsrpd/tree/master

# Read in the list of plant names, this will let us match up the plant name with the id number used in these data
plants = read_delim("wcvp_dwca/wcvp_taxon.csv", delim = "|") %>% 
  # Filter to legumes
  filter(family == "Fabaceae") %>% 
  # Select columns we might want
  select(taxonid, genus, specificepithet, infraspecificepithet)

# plants = plants %>% 
  # filter(genus == "Abrus" & specificepithet == "fruticulosus")

# Read in the information about what gegraphic areas these species occupy
ranges = read_delim("wcvp_dwca/wcvp_distribution.csv", delim = "|") %>% 
  # Filter to legumes by filtering to ids in the first dataframe
  filter(coreid %in% plants$taxonid) %>% 
  # Remove additional string from location codes
  mutate(locationid = str_remove(locationid, "TDWG:"))

# Does each locality have a distinct locationid?
check = ranges %>% 
  distinct(locality, locationid) %>% 
  group_by(locationid) %>% 
  summarize(n = n())
# yes

# Just want to check locality names against codes at different levels

# Read in all the polygons at different levels of organization

poly_sf1 = geojson_sf("wgsrpd-master/geojson/level1.geojson") %>% 
  mutate(LEVEL1_COD = as.character(LEVEL1_COD))
poly_sf2 = geojson_sf("wgsrpd-master/geojson/level2.geojson") %>% 
  mutate(LEVEL2_COD = as.character(LEVEL2_COD))
poly_sf3 = geojson_sf("wgsrpd-master/geojson/level3.geojson")
poly_sf4 = geojson_sf("wgsrpd-master/geojson/level4.geojson")

# Make simple df versions for checking and exploring
poly_sf1_info = poly_sf1 %>% 
  select(LEVEL1_COD, LEVEL1_NAM) %>% 
  st_drop_geometry()
poly_sf2_info = geojson_sf("wgsrpd-master/geojson/level2.geojson") %>% 
  mutate(LEVEL2_COD = as.character(LEVEL2_COD))
poly_sf3_info = geojson_sf("wgsrpd-master/geojson/level3.geojson")
poly_sf4_info = geojson_sf("wgsrpd-master/geojson/level4.geojson")

ranges$locality[ranges$locality %in% poly_sf1$LEVEL1_NAM]
ranges$locationid[ranges$locationid %in% poly_sf1$LEVEL1_COD]
# one location matches at level 1

ranges$locality[ranges$locality %in% poly_sf2$LEVEL2_NAM]
ranges$locationid[ranges$locationid %in% poly_sf2$LEVEL2_COD]
# many match by name at level 2, but fewer match by code

ranges$locality[ranges$locality %in% poly_sf3$LEVEL3_NAM]
ranges$locationid[ranges$locationid %in% poly_sf3$LEVEL3_COD]
# most match by both identifiers at level 3

ranges$locality[ranges$locality %in% poly_sf4$Level_4_Na]
ranges$locationid[ranges$locationid %in% poly_sf4$Level4_cod]
ranges$locationid[ranges$locationid %in% poly_sf4$Level3_cod]
# names match at level 4 but codes are different

ranges2 = ranges %>% 
  left_join(., select(poly_sf1, LEVEL1_COD), by = c("locationid" = "LEVEL1_COD")) %>% 
  left_join(., select(poly_sf1, LEVEL1_NAM), by = c("locality" = "LEVEL1_NAM")) %>% 
  left_join(., select(poly_sf2, LEVEL2_COD), by = c("locationid" = "LEVEL2_COD")) %>% 
  left_join(., select(poly_sf2, LEVEL2_NAM), by = c("locality" = "LEVEL2_NAM"))

# Which level of geographic designation do these codes use?

table(ranges$locality)
table(ranges$locationid)
table(poly_sf4$Level3_cod)

#test

range_codes = unique(ranges$locationid)

level3_codes = unique(poly_sf4$Level3_cod)

setdiff(level3_codes, range_codes)
setdiff(range_codes, level3_codes)

ranges2 = ranges %>% 
  mutate(establishmentmeans = if_else(is.na(establishmentmeans), "not_specified", establishmentmeans)) %>% 
  group_by(coreid, establishmentmeans) %>%
  summarize(loc_list = list(locationid))

poly_sf1 = geojson_sf("wgsrpd-master/geojson/level1.geojson")
poly_sf4 = geojson_sf("wgsrpd-master/geojson/level4.geojson")

plot(poly_sf4)


