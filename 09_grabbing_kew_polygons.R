## Grabbing polygons for my species from Plants of the World Online

library(tidyverse)
library(geojsonsf)
library(sf)
#install.packages("shapefiles")
library(shapefiles)

# read in taxon and filter to fabaceae
plants = read_delim("wcvp_dwca/wcvp_taxon.csv", delim = "|") %>% 
  # Filter to legumes
  filter(family == "Fabaceae") %>% 
  # Select columns we might want
  select(taxonid, genus, specificepithet, infraspecificepithet)

# read in range polygons
ranges = read_delim("wcvp_dwca/wcvp_distribution.csv", delim = "|") %>% 
  # Filter to legumes by filtering to ids in the first dataframe
  filter(coreid %in% plants$taxonid) %>% 
  # Remove additional string from location codes
  mutate(locationid = str_remove(locationid, "TDWG:"))


# read in special issue data from plants of the world
plant_dist = read_delim("wcvp_names_and_distribution_special_issue_28_feb_2022/wcvp_distribution.txt", delim = "|")

# Read in species names
names = read_delim("wcvp_names_and_distribution_special_issue_28_feb_2022/wcvp_names.txt", delim = "|") %>% 
  filter(family == "Fabaceae", taxon_rank=="Species", taxon_status=="Accepted")

# Read in a dataset with my species!
my_sp<-read.csv("summary_df_august2024.csv")

# Make plant names in powo "names" file the same format as I have, with the _ between sp and genus

names$species_names<-ifelse(names$species=="NA", "NA", paste(names$genus,names$species))

names$species_names<-gsub(" ", "_", names$species_names)

# filter to only the species I have in my dataset
names<-names %>% filter(names$species_names %in% my_sp$species)

2910-2895
# 15 species do not have polygons!

# Okay, let's try to get these associated plant distributions up and running, first 
# by filtering to just the species that are in my dataset
plant_dist1<-plant_dist %>% filter(plant_name_id %in% names$plant_name_id)

# pull species names from "names" dataset to make plant distributions easier to work with
# basically, using the unique species id in both datasets to match the species name to its
# distributions
plant_dist1$species_name <- names$species_names[match(plant_dist1$plant_name_id, names$plant_name_id)]

# Read in polys for level 3 geometry, as I've never looked at these before
poly_sf3 = geojson_sf("wgsrpd-master/geojson/level3.geojson")
st_crs(poly_sf3)

# Make sure that the geometries in plant_dist1 and level3.geojson match up!
plant_dist1$area_code_l3[plant_dist1$area_code_l3 %in% poly_sf3$LEVEL3_COD]

# Okay, I feel convinced enough to do a little merge of the geometries with the species!
plant_dist1$geometry <- poly_sf3$geometry[match(plant_dist1$area_code_l3, poly_sf3$LEVEL3_COD)]

# Okay, what if I mapped some of the polygons? Would they match up with what I know to be true?
world = map_data('world')

plant_dist1$introduced<-as.factor(plant_dist1$introduced)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = plant_dist1[plant_dist1$species_name=="Abrus_fruticulosus",], aes(geometry=geometry, fill = introduced), alpha = 0.2)
# Niiice! It's making a distribution that I know is correct for Abrus fruticulosus

# What about Lupinus nootkaensis

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = plant_dist1[plant_dist1$species_name=="Lupinus_nootkatensis",], aes(geometry=geometry, fill = introduced), alpha = 0.2)
# Okay wowwww, way to invalidate Iceland's problems with Lupinus! But still, this is better than ILDIS

# Let's try another species...Lotus pedunculatus
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = plant_dist1[plant_dist1$species_name=="Lotus_pedunculatus",], aes(geometry=geometry, fill = introduced), alpha = 0.2)
# What is going on with Czechia and it's missing data? Anyways, at least these points work

# Let's try trifolium medium

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = plant_dist1[plant_dist1$species_name=="Trifolium_medium",], aes(geometry=geometry, fill = introduced), alpha = 0.2)
# Wow, we're cooking with fire now! Compared to the old polygons, this map is more accurate

# Finally, Ononis spinosa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour="darkgrey", fill = NA, alpha = 0.2) +
  geom_sf(data = plant_dist1[plant_dist1$species_name=="Ononis_spinosa",], aes(geometry=geometry, fill = introduced), alpha = 0.2)

# writing the distribution polygons into a df

powo_polygons<-st_as_sf(plant_dist1, sf_column_name="geometry")

#sf::st_write(powo_polygons,"powo_polygons_sorted.shp")

p<-st_collection_extract(powo_polygons, "POLYGON")

sf::st_write(p,"powo_polygons_sorted.shp")
# These are under powo_polygons (I think I renamed to make it more straightfoward to find these...
# had the opposite effect, I fear!)


