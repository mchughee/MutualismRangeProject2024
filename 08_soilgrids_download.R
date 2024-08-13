# Please note: symbiont throws a tantrum any time you try to load gdalUtilities! Just download
# the script and run it locally
devtools:::install_github("gearslaboratory/gdalUtils")
library(gdalUtilities)


gdalUtilities::gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
                        co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
                        tr=c(250, 250), # Desired output resolution
                        #verbose=T,
                        "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_5-15cm_mean.vrt", # Input VRT
                        "nitrogen_5-15cm_mean.tif")