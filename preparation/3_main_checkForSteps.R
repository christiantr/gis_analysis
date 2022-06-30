

library(Rcpp)
library(osmdata)
library(leaflet)
library(sf)
library(tmap)
library(dplyr)
# source("gemeindenUD.R")
source("analyse_loadFiles.R")

library(mapview)
library(htmlwidgets)


bounds <- st_read("./data/bounds.shp")
bbox <- bounds[bounds$area == "Unteres Drautal", ]

query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = "steps")
features <- osmdata_sf(query)
features <- trim_osmdata(features, bb_poly = bb <- as(bbox, "Spatial"))
steps <- features$osm_lines

b <- readBuildingsDistAndElev()


bo <- b %>%
  filter(st_intersects(geometry, steps, sparse = FALSE))

bo <- b %>%
  filter(st_overlaps(geometry, steps, sparse = FALSE))

f <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addPolygons(data = bbox, fillOpacity = 0.0, weight = 1.2, color = "#444444", smoothFactor = 0.5) %>%
  addMeasure() %>%
  addScaleBar() %>%
  addPolylines(data = bo, color = "green", group = "paths") %>%
  addPolylines(data = steps, color = "red", group = "steps") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("steps", "paths"),
    options = layersControlOptions(collapsed = TRUE)
  )
saveWidget(f, file = "steps.html")
