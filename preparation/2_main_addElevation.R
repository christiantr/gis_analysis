library(sf)
library(dplyr)
library(terra)
source("./preparation/utility_elevation.R")

raster <- terra::rast("./dem/dem_ud_1m_wgs84.tif")

stop_destination_bus <- st_read("./data/closest_stops_Bus_destinations.gpkg")
stop_destination_mt <- st_read("./data/closest_stops_MT_destinations.gpkg")

origin_stop_bus <- st_read("./data/closest_Bus_origins_stops.gpkg")
origin_stop_mt <- st_read("./data/closest_MT_origins_stops.gpkg")


st_write(addElevationData(stop_destination_bus, dem = raster, minDist=100, minWalkingDist=200), "./data/closest_elevation_stops_Bus_destinations.gpkg")
st_write(addElevationData(stop_destination_mt, dem = raster, minDist=100, minWalkingDist=200), "./data/closest_elevation_stops_MT_destinations.gpkg")

st_write(addElevationData(origin_stop_bus, dem = raster, minDist=100, minWalkingDist=200), "./data/closest_elevation_Bus_origins_stops.gpkg")
st_write(addElevationData(origin_stop_mt, dem = raster, minDist=100, minWalkingDist=200), "./data/closest_elevation_MT_origins_stops.gpkg")


