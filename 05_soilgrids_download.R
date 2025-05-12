### Downloading SoilGrids data for soil nitrogen

# Please note: the web version of R on the remote server does not like gdalUtilities! Just download
# the script and run it locally

library(devtools)
devtools::install_github("JoshOBrien/gdalUtilities")
library(gdalUtilities)

### Actual download
gdalUtilities::gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
                        co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
                        tr=c(250, 250), # Desired output resolution
                        #verbose=T,
                        "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_5-15cm_mean.vrt", # Input VRT
                        "nitrogen_5-15cm_mean.tif")

## check the warnings
warnings()
sessionInfo()
