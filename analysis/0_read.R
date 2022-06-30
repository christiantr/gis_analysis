library(tidyverse)
library(tibble)
library(sf)

bounds <- st_read("./data/bounds.gpkg")

roads<- st_read("./data/roads.gpkg")
rails<- st_read("./data/rails.gpkg")


stops <- st_read("./data/stops.gpkg")
stops$type <- as_factor(stops$type)
stops$county <- as_factor(stops$county)


## stops Overview
stops %>%
  st_drop_geometry() %>%
  group_by(type, county) %>%
  count()

origins <- st_read("./data/origins.gpkg")
origins$county <- as_factor(origins$county)
origins$type <- as_factor(origins$type)

## Origins Overview
origins %>%
  st_drop_geometry() %>%
  filter(type == "Tourist") %>%
  group_by(county, tourism) %>%
  count()
origins %>%
  st_drop_geometry() %>%
  filter(type == "Residential") %>%
  group_by(county, building) %>%
  count()


destinations <- st_read("./data/destinations.gpkg")
destinations$county <- as_factor(destinations$county)
destinations$category <- as_factor(destinations$category)




###############
destination_stop_Train <- st_read("./data/closest_stops_Train_destinations.gpkg") %>% add_column(closest = "Train")
destination_stop_Bus <- st_read("./data/closest_elevation_stops_Bus_destinations.gpkg") %>% add_column(closest = "Bus")
destination_stop_MT <- st_read("./data/closest_elevation_stops_MT_destinations.gpkg") %>% add_column(closest = "Mt")
origin_stop_Train <- st_read("./data/closest_Train_origins_stops.gpkg") %>% add_column(closest = "Train")
origin_stop_Bus <- st_read("./data/closest_elevation_Bus_origins_stops.gpkg") %>% add_column(closest = "Bus")
origin_stop_MT <- st_read("./data/closest_elevation_MT_origins_stops.gpkg") %>% add_column(closest = "Mt")

### Combine Rows

destination_stop <- destination_stop_Train %>%
  bind_rows(., destination_stop_Bus) %>%
  bind_rows(., destination_stop_MT) 


origin_stop <- origin_stop_Train %>%
  bind_rows(., origin_stop_Bus) %>%
  bind_rows(., origin_stop_MT) 


origin_stop <- left_join(origin_stop, st_drop_geometry(origins), by = c("srcID" = "osm_id"))
destination_stop <- left_join(destination_stop, st_drop_geometry(destinations), by = c("dstID" = "osm_id"))


##############

max(origin_stop$slope_max, na.rm = TRUE)
max(destination_stop$slope_max, na.rm = TRUE)


### Check count###########

st_drop_geometry(destination_stop_Train) %>% count()
st_drop_geometry(destination_stop_Bus) %>% count()
st_drop_geometry(destination_stop_MT) %>% count()
st_drop_geometry(origin_stop_Train) %>% count()
st_drop_geometry(origin_stop_Bus) %>% count()
st_drop_geometry(origin_stop_MT) %>% count()
