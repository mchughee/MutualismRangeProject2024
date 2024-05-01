### setwd, read in libraries

library(ggplot2)
library(sf)
library(sp)
library(terra) #don't think I need this but ya never know
library(tidyverse)

### Read in files
occ<-read.csv("dat_test_clean.csv")
legume_pol<-readRDS("legume_range_polygons_data.rds")

# Subset for the three species in the test dataset
legume_pols<-subset(legume_pol, species == "Abrus fruticulosus" | species == "Abrus precatorius" |
                     species == "Acacia acinacea")

# merge the legumessssss. This does not work!!!!!! It creates a dataframe with
# nearly double 91000 observations which is insane. Plus I don't need it.
#merged_legume <- sp::merge(legume_pols, occ, by.x="species", all.x=TRUE, all.y=TRUE,duplicateGeoms = TRUE)
# merged_legume <- as.data.frame(merged_legume)

# Extract unique polygons. Definitely do this step!
tmp <- data.frame(code = NULL, index = NULL)
for (x in 1:length(legume_pols$polygon)) {
  if(legume_pols$polygon[[x]]@data$Code %in% tmp$code) next()
  tmp <- rbind((data.frame(code = legume_pols$polygon[[x]]@data$Code, index = x)), tmp)
}
unique_pols <- legume_pols[tmp$index, ]
unique_pols <- cbind(unique_pols, tmp)



# Merge with unique polygons. Erin's note: boo, not helpful.
# merge_EFN <- sp::merge(unique_pols, lspecies_counts, by.x="code", by.y="code")

class(legume_pol$species)
class(legume_pol$introduced_status)
legume_pol$species<-as.factor(legume_pol$species)
# Run the below line to get an S4 object
# This is where I'm stuck-- I need to get the species names into the polygons
# but I don't know how to. For some reason, do.call naturally takes invasion.status
# when it rbinds, but not species!
spatial_polygons <- do.call(rbind, legume_pols$`polygon`)

#legume_pols %>% select(species, polygon) -> test

#attribute_data <- merge_fix[c("fixer", "num_species")] boooooo
# spatial_polygons_df <- SpatialPolygonsDataFrame(spatial_polygons, data=merged_legume, match.ID = F)
# Run the last line
spatial_polygons_sf <- sf::st_as_sf(spatial_polygons)