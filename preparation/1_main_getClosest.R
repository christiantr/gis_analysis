source("utility_closestPoint.R")
library(htmlwidgets)
library(mapview)
library(htmlwidgets)
library(osrm)
library(tibble)
library(tidyverse)
library(sf)
library(sfheaders)
# https://rdrr.io/cran/osrm/man/osrmRoute.html

## Start Up OSRM docker
# cd C:\
# cd OSM
# docker run -t -i -p 5000:5000 -v "${PWD}:/data" osrm/osrm-backend osrm-routed --algorithm mld /data/austria-latest.osrm
# Local server
u <- "http://127.0.0.1:5000/"

bounds <- st_read("./data/bounds.gpkg")
bbox <- bounds[bounds$area == "Unteres Drautal", ]

### Read stop locations
stops <- st_read("./data/stops.gpkg")
stops$type <- as_factor(stops$type)
stops$county <- as_factor(stops$county)

### Overview of the stops
stops %>%
  st_drop_geometry() %>%
  group_by(type, county) %>%
  count()


### Read origin locations
origins <- st_read("./data/origins.gpkg")
origins$county <- as_factor(origins$county)
origins$type <- as_factor(origins$type)
origins <- origins %>% select(osm_id, county, type, name, addr.city, addr.housenumber, addr.place, addr.postcode, addr.street, building, tourism)


### Read origin destination locations
destinations <- st_read("./data/destinations.gpkg")
destinations$county <- as_factor(destinations$county)
destinations$category <- as_factor(destinations$category)
destinations <- destinations %>% select(osm_id, county, category, name, addr.city, addr.housenumber, addr.street, addr.place, addr.postcode, amenity, tourism, historic, craft, shop, office, leisure, sport, landuse)


## Get Routes and Save to File
st_write(getAllClosestLS(stops %>% filter(type == "Train"), destinations, server = u), "./data/closest_stops_Train_destinations.gpkg")
st_write(getAllClosestLS(stops %>% filter(type == "Bus"), destinations, server = u), "./data/closest_stops_Bus_destinations.gpkg")
st_write(getAllClosestLS(stops %>% filter(type == "MT"), destinations, server = u), "./data/closest_stops_MT_destinations.gpkg")

st_write(getAllClosestLS(stops %>% filter(type == "Train"), origins, server = u, invert = TRUE), "./data/closest_Train_origins_stops.gpkg")
st_write(getAllClosestLS(stops %>% filter(type == "Bus"), origins, server = u, invert = TRUE), "./data/closest_Bus_origin_stops.gpkg")
st_write(getAllClosestLS(stops %>% filter(type == "MT"), origins, server = u, invert = TRUE), "./data/closest_MT_origins_stops.gpkg")
