library(tidyverse)
library(htmlwidgets)
library(ggmap)
library(tibble)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(cowplot)
library(fontawesome)
# devtools::install_github("rstudio/fontawesome")
library(RColorBrewer)
library(colorRamps)


source("./analysis/0_read.R")

##### Get Closest Stop #####
origin_stop <- st_drop_geometry(origin_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(srcID) %>%
  slice(which.min(distance))

destination_stop <- st_drop_geometry(destination_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(dstID) %>%
  slice(which.min(distance))





##### Join to get Location points
origin_stop <-
  origin_stop %>%
  select(srcID, dstID, distance, duration, elev_cum, slope_max, closest, name, county, type) %>%
  left_join(., origins, by = c("srcID" = "osm_id")) 




destination_stop <-
  destination_stop %>%
  select(srcID, dstID, distance, duration, elev_cum, slope_max, closest, name, county, category) %>%
  left_join(., destinations, by = c("dstID" = "osm_id")) 


all <- bind_rows(origin_stop, destination_stop)
all <- st_as_sf(all)






### Define Icons
icoLst <- awesomeIconList(
  car = makeAwesomeIcon(text = fa("car"), markerColor = "green", squareMarker = TRUE),
  bus = makeAwesomeIcon(text = fa("bus"), markerColor = "blue", squareMarker = TRUE),
  train = makeAwesomeIcon(text = fa("train"), markerColor = "red", squareMarker = TRUE)
)



### Filter Data if wanted.
# filtered<-  all %>% filter(distance>0.75)  %>% filter(elev_cum>=50)
filtered <- all


### Define Color Paletes for Elevation and Distance
pal<-colorRamps::blue2green2red(100)

color_pal_elevation <- colorNumeric(palette = pal, domain = filtered$elev_cum, reverse = F)
color_pal_distance <- colorNumeric(palette = pal , domain = filtered$distance, reverse = F)

previewColors(pal,  sort(rexp(16)))


### Create Pop-Up Text for Points on Map
popupText <- function(point) {
  paste(
    "src ID:",
    point$srcID,
    "dst ID:",
    point$dstID,
    "distance",
    point$distance,
    "elevation",
    point$elev_cum,
    "max Slope",
    point$slope_max
  )
}


### Create Leaflet Map
center <- st_coordinates(st_centroid(bounds[bounds$area == "Unteres Drautal", ]))
f <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen") %>%
  addTiles(group = "OSM") %>%
  addMeasure() %>%
  addScaleBar() %>%
  setView(lng = center[1], lat = center[2], zoom = 11) %>%
  addPolygons(data = bounds[bounds$area == "Weissenstein", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
  addPolygons(data = bounds[bounds$area == "Fresach", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
  addPolygons(data = bounds[bounds$area == "Ferndorf", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
  addPolygons(data = bounds[bounds$area == "Paternion", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
  addPolygons(data = bounds[bounds$area == "Stockenboi", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
  addLabelOnlyMarkers(
    data = st_centroid(bounds)[1:5, ],
    label = ~area,
    labelOptions = labelOptions(textsize = "16px", noHide = TRUE, direction = "bottom", textOnly = TRUE), group = "Municipality_Names"
  ) %>%
  addCircleMarkers(data = origins, color = "black", label = ~ as.character(origins$type), popup = ~ as.character(osm_id), stroke = FALSE, fillOpacity = 1.0, radius = 2, group = "Origins") %>%
  addCircleMarkers(data = destinations, color = "darkgreen", label = ~ as.character(destinations$category), popup = ~ as.character(osm_id), stroke = FALSE, fillOpacity = 1.0, radius = 3, group = "Destinations") %>%
  addAwesomeMarkers(data = stops %>% filter(type == "MT"), icon = icoLst$car, popup = ~ as.character(id), group = "MT", options = markerOptions(opacity = 0.8)) %>%
  addAwesomeMarkers(data = stops %>% filter(type == "Bus"), icon = icoLst$bus, popup = ~ as.character(id), group = "Bus", options = markerOptions(opacity = 0.8)) %>%
  addAwesomeMarkers(data = stops %>% filter(type == "Train"), icon = icoLst$train, popup = ~ as.character(id), group = "Train", options = markerOptions(opacity = 0.8)) %>%
  addCircleMarkers(
    data = filtered, color = ~ color_pal_elevation(filtered$elev_cum), popup =
      popupText(filtered), stroke = FALSE, fillOpacity = 3, radius = 3, group = "Elevation_Coloring"
  ) %>%
  addCircleMarkers(
    data = filtered, color = ~color_pal_distance(filtered$distance), popup =
      popupText(filtered), stroke = FALSE, fillOpacity = 3, radius = 3, group = "Distance_Coloring"
  )%>%
  addLayersControl(
    baseGroups = c("Stamen", "OSM"),
    overlayGroups = c("Origins", "Destinations", "Municipalities", "Train", "Bus", "MT", "Elevation_Coloring", "Distance_Coloring", "Municipality_Names"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addMouseCoordinates()%>% 
  hideGroup("Elevation_Coloring")


saveWidget(f, file = "blindspots_colored.html")

