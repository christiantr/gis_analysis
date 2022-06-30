library(sf)
library(curl)
library(tidyverse)
library(units)
library(terra)


#### Weblinks to dem files
# https://data.bev.gv.at/geonetwork/srv/metadata/cced2282-cfc5-42eb-9acb-b1c799522fcd
page1 <- "http://quicklook.bev.gv.at/als/DSM/CRS3035RES50000mN2600000E4550000.tif"
page2 <- "http://quicklook.bev.gv.at/als/DSM/CRS3035RES50000mN2600000E4600000.tif"

#### Directoy and Files
dir.create("./dem")
store.page1 <- "./dem/page1.tif"
store.page2 <- "./dem/page2.tif"

#### Download
curl_download(page1, store.page1, quiet = FALSE)
curl_download(page2, store.page2, quiet = FALSE)

### Read raster data
rast1.3035 <- terra::rast(store.page1)
rast2.3035 <- terra::rast(store.page2)

### Plot raster data
plot(rast1.3035)
plot(rast2.3035)

########## Get Bounding Box
bounds <- st_read("./data/bounds.gpkg")
bbox.WGS <- bounds %>%
  filter(area == "Unteres Drautal") %>%
  pull()
bbox.3035 <- st_transform(bbox.WGS, crs = 3035)

#### Merge and crop rasters
wgs84 <- "+proj=longlat +datum=WGS84"
raster.3035 <- terra::merge(rast1.3035, rast2.3035)
raster.3035 <- terra::crop(raster.3035, as_Spatial(bbox.3035))

plot(raster.3035)

### Convert to WGS84 and save to file
raster.wgs <- terra::project(raster.3035, wgs84)
terra::writeRaster(raster.3035, "./dem/dem_ud_1m_3035.tif")
terra::writeRaster(raster.wgs, "./dem/dem_ud_1m_wgs84.tif")
