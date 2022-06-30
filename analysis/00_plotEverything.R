library(tidyverse)
library(htmlwidgets)
library(tibble)
library(sf)
library(mapview)
library(leaflet)
library(fontawesome)
library(leaflegend)

# devtools::install_github("rstudio/fontawesome")


source("./analysis/0_read.R")


icoLst <- awesomeIconList(
  car = makeAwesomeIcon(text = fa("car"), markerColor = "green", squareMarker = TRUE, iconColor ="darkgray"),
  bus = makeAwesomeIcon(text = fa("bus"), markerColor = "blue", squareMarker = TRUE,  iconColor ="darkgray"),
  train = makeAwesomeIcon(text = fa("train"), markerColor = "red", squareMarker = TRUE,  iconColor ="darkgray")
  # house = makeAwesomeIcon(text = fa("house-user"), markerColor = "black", squareMarker = FALSE)
  
)

bbox<- bounds[bounds$area == "Unteres Drautal", ]

icons <- iconList(
  circle <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                     iconWidth = 15, iconHeight = 15),
  triangle <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                       iconWidth = 15, iconHeight = 15)
)


## Crop rail and road to area
roads <- st_intersection(roads, bbox )
rails <- st_intersection(rails, bbox )


center<- st_coordinates(st_centroid( bounds[bounds$area == "Unteres Drautal", ]))

f <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner, group = "Stamen") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "StamenLite") %>%
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
    labelOptions = labelOptions(textsize = "20px", noHide = TRUE, direction = "bottom", textOnly = FALSE, opacity =0.7), group = "Municipality_Names"
  ) %>%
  addPolylines(data=rails, color="darkred", smoothFactor = 0.5, opacity=1, group="rails", dashArray='5,10', weight=8) %>%
  addPolylines(data=roads %>%filter(highway %in% c("motorway") ), color="darkblue", smoothFactor = 0.5, opacity=0.8, group="autobahn", weight=5) %>%
  addMarkers(data = origins, icon = ~ icons[1], group = "origins")%>%
  # addAwesomeMarkers(data = origins, icon = icoLst$house,   group = "origins", options = markerOptions(opacity = 0.6)) %>%
  addMarkers(data = destinations, icon = ~ icons[2], group = "destinations")%>%
    # addCircleMarkers(data = origins, color = "black", label = ~ as.character(origins$type), popup = ~ as.character(osm_id), stroke = FALSE, fillOpacity = 1.0, radius = 2, group = "origins") %>%
  # addCircleMarkers(data = destinations, color = "darkgreen", label = ~ as.character(destinations$category), popup = ~ as.character(osm_id), stroke = FALSE, fillOpacity = 1.0, radius = 3, group = "destinations") %>%
  addAwesomeMarkers(data = stops %>% filter(type == "MT"), icon = icoLst$car, popup = ~ as.character(id), group = "MT", options = markerOptions(opacity = 0.7)) %>%
  addAwesomeMarkers(data = stops %>% filter(type == "Bus"), icon = icoLst$bus, popup = ~ as.character(id), group = "Bus", options = markerOptions(opacity = 0.9)) %>%
  addAwesomeMarkers(data = stops %>% filter(type == "Train"), icon = icoLst$train, popup = ~ as.character(id), group = "Train", options = markerOptions(opacity = 0.8)) %>%
   # addPolylines(data=destination_stop_Train, color="gray", smoothFactor = 0.5, opacity=0.2, group="destinations_Train" ) %>%
  # addPolylines(data=destination_stop_Bus, color="gray", smoothFactor = 0.5, opacity=0.2, group="destinations_Bus" ) %>%
  # addPolylines(data=destination_stop_MT, color="gray", smoothFactor = 0.5, opacity=0.2, group="destinations_MT" ) %>%
  # addPolylines(data=origin_stop_Train, color="gray", smoothFactor = 0.5, opacity=0.2, group="origins_Train" ) %>%
  # addPolylines(data=origin_stop_Bus, color="gray", smoothFactor = 0.5, opacity=02, group="origins_Bus" ) %>%
  # addPolylines(data=origin_stop_MT, color="gray", smoothFactor = 0.5, opacity=0.7, group="origins_MT" ) %>%

  
  
  addPolylines(data=roads %>%filter(highway %in% c( "trunk", "primary", "secondary", "tertiary", "unclassified", "motorway_link","trunk_link",
                                    "primary_link", "secondary_link", "tertiary_link") ), color="black", smoothFactor = 0.5, opacity=0.8, group="roads", weight=2) %>%
  
  addLayersControl(
    baseGroups = c("Stamen", "StamenLite","OSM"),
    overlayGroups = c("origins", "destinations", "Municipalities", "Train", "Bus", "MT", "destinations_Train", "destinations_Bus", "destinations_MT", "origins_Train", "origins_Bus", "origins_MT", "Municipality_Names",
                      "rails", "roads","autobahn"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup("destinations_Train") %>%
  hideGroup("destinations_Bus") %>%
  hideGroup("destinations_MT") %>%
  hideGroup("origins_Train") %>%
  hideGroup("origins_Bus") %>%
  hideGroup("origins_MT") %>%
  hideGroup("MT") 





saveWidget(f, file = "overview.html")


# mapshot(f, file = "overview.jpg",  remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                                                         # "drawToolbar", "easyButton"), debug=T)

# webshot::webshot(url = paste0(getwd(), "/m.html"), file = paste0(getwd(),"/m.jpg"), debug=T)

