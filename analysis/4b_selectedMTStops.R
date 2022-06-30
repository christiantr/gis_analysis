library(tidyverse)
library(htmlwidgets)
library(ggmap)
library(tibble)
library(sf)
library(purrr)
library(mapview)
# remotes::install_github("r-spatial/mapview")

# 
# install.packages("webshot",dependencies = TRUE)
# library(webshot)
# webshot::install_phantomjs()

library(leaflet)
library(leaflegend)

library(leafem)
library(cowplot)
library(fontawesome)
# devtools::install_github("rstudio/fontawesome")
library(RColorBrewer)
library(colorRamps)
library(htmltools)
library(webshot)

source("./analysis/0_read.R")

##### Get Closest Stop #####
origin_stop <- st_drop_geometry(origin_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(srcID) %>%
  slice(which.min(distance)) %>%
  add_column(leg = "1_first")

destination_stop <- st_drop_geometry(destination_stop) %>%
  filter(closest %in% c("Bus", "Mt")) %>%
  group_by(dstID) %>%
  slice(which.min(distance)) %>%
  add_column(leg = "2_last")


##### Join to get Location points
origin_stop <-
  origin_stop %>%
  filter(dstID %in% ids) %>%
  select(srcID, dstID, distance, duration, elev_cum, slope_max, closest, name, county, type) %>%
  left_join(., origins, by = c("srcID" = "osm_id"))



destination_stop <-
  destination_stop %>%
  filter(srcID %in% ids) %>%
  select(srcID, dstID, distance, duration, elev_cum, slope_max, closest, name, county, type) %>%
  left_join(., destinations, by = c("dstID" = "osm_id"))


all <- bind_rows(origin_stop, destination_stop)
all <- st_as_sf(all)





########### Map Creation

icoLst <- awesomeIconList(
  car = makeAwesomeIcon(text = fa("car"), markerColor = "green", squareMarker = TRUE, iconColor ="darkgray"),
  bus = makeAwesomeIcon(text = fa("bus"), markerColor = "blue", squareMarker = TRUE,  iconColor ="darkgray"),
  train = makeAwesomeIcon(text = fa("train"), markerColor = "red", squareMarker = TRUE,  iconColor ="darkgray")
  # house = makeAwesomeIcon(text = fa("house-user"), markerColor = "black", squareMarker = FALSE)
  
)





popupText <- function(point) {
  paste(
    "src ID:",
    point$srcID,
    "dst ID:",
    point$dstID,
    "distance",
    round(1000 * point$distance, 0),
    round(point$elev_cum, 0),
    round(100 * point$slope_max, 1)
  )
}

shortText <- function(distance, elev_cum, slope_max) {
  if (is.na(elev_cum) && is.na(slope_max)) {
    text <- paste0(round(1000 * distance, 0), "m")
  }
  else if (is.na(slope_max)) {
    text <- paste0(
      round(1000 * distance, 0), "m ",
      round(elev_cum, 0), "hm"
    )
  }

  else {
    text <-
      paste0(
        round(1000 * distance, 0), "m ",
        round(elev_cum, 0), "hm ",
        round(100 * slope_max, 1), "%"
      )
  }
  return(text)
}

shortTextHtml <- function(distance, elev_cum, slope_max) {
  if (is.na(elev_cum) && is.na(slope_max)) {
    text <- paste(
      "<p>",
      round(1000 * distance, 0), "m",
      "</p>"
    )
  }
  else if (is.na(slope_max)) {
    text <- paste(
      "<p>", round(1000 * distance, 0), "m", "<p></p>",
      round(elev_cum, 0), "hm", "</p>"
    )
  }

  else {
    text <-
      paste(
        "<p>",
        round(1000 * distance, 0), "m", "<p></p>",
        round(elev_cum, 0), "hm", "<p></p>",
        round(100 * slope_max, 1), "%",
        "</p>"
      )
  }
  return(text)
}




makeMap <- function(center, points, mtstops, zoom = 11) {

  # labels<- lapply(filtered, shortText)
  filtered <- filtered %>%
    rowwise() %>%
    mutate(shortTextHtml = shortTextHtml(distance = distance, elev_cum = elev_cum, slope_max = slope_max))
  filtered <- filtered %>%
    rowwise() %>%
    mutate(shortText = shortText(distance = distance, elev_cum = elev_cum, slope_max = slope_max))


  color_pal_elevation <-  colorNumeric(    palette = "YlGnBu",
    domain = filtered$elev_cum
  )

  ### Create Leaflet Map
  f <- leaflet() %>%
    addProviderTiles(providers$Stamen, group = "Stamen") %>%
    addProviderTiles(providers$BasemapAT, group = "BaseMap") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addMeasure() %>%
    addScaleBar() %>%
    setView(lng = center[1], lat = center[2], zoom = zoom) %>%
    addPolygons(data = bounds[bounds$area == "Weissenstein", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
    addPolygons(data = bounds[bounds$area == "Fresach", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
    addPolygons(data = bounds[bounds$area == "Ferndorf", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
    addPolygons(data = bounds[bounds$area == "Paternion", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
    addPolygons(data = bounds[bounds$area == "Stockenboi", ], fillOpacity = 0.1, weight = 1.2, color = "#444444", smoothFactor = 0.5, group = "Municipalities") %>%
    addLabelOnlyMarkers(
      data = st_centroid(bounds)[1:5, ],
      label = ~area,
      labelOptions = labelOptions(textsize = "16px", noHide = TRUE, direction = "bottom", textOnly = TRUE, zIndexOffset = -100000), group = "Municipality_Names"
    ) %>%
    addLabelOnlyMarkers(
      data = filtered,
      # label = lapply(filtered$shortText, htmltools::HTML),
      label = filtered$shortText,
      labelOptions = labelOptions(textsize = "11px", permanent = TRUE, direction = "bottom", textOnly = FALSE, opacity = 0.7),
      group = "Summary"
    ) %>%
    addCircleMarkers(
      data = filtered, color = ~ color_pal_elevation(filtered$elev_cum), popup =
        popupText(filtered), stroke = TRUE, fillOpacity = 1, radius =15 , group = "Elevation_Coloring"
    ) %>%
    # addCircleMarkers(
    #   data = filtered, color = ~ color_pal_distance(filtered$distance), popup =
    #     popupText(filtered), stroke = FALSE, fillOpacity = 1, radius = 5, group = "Distance_Coloring"
    # ) %>%
    addLayersControl(
      baseGroups = c("Stamen", "OSM", "BaseMap"),
      overlayGroups = c("Origins", "Destinations", "Municipalities", "Train", "Bus", "MT", "Elevation_Coloring",  "Municipality_Names", "Summary"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addAwesomeMarkers(data = mtstops, icon = icoLst$car, popup = ~ as.character(id), group = "MT", options = markerOptions(opacity = 1.0, zIndexOffset = 100000)) %>%
    addMouseCoordinates() %>%
    hideGroup("Origins") %>%
    hideGroup("Destinations") %>%
    addLegend("topleft", pal = color_pal_elevation, values = filtered$elev_cum,
              title = "Cumulative elevation",
              labFormat = labelFormat(suffix = " hm"),
              opacity = 1
    )

  return(f)
}


########## Create Map for some selected locations

# ids <- c("MT_7", "MT_29", "MT_34", "MT_96", "MT_98", "MT_44", "MT_68")
ids <- c("MT_29", "MT_96") 
# ids <- c("MT_96") 

center <- st_coordinates(mtstops <- stops %>% filter(id == "MT_29"))
# center <- st_coordinates(mtstops <- stops %>% filter(id == "MT_96"))
# center <- st_coordinates(st_centroid(bounds[bounds$area == "Unteres Drautal", ]))

filtered <- all %>% filter(dstID %in% ids | srcID %in% ids)
mtstops <- stops %>%
  filter(type == "MT") %>%
  filter(id %in% ids)



f <- makeMap(center = center, points = filtered, mtstops = mtstops, zoom = 17)
saveWidget(f, file = "blindspots_selected.html")

# mapshot(f, file = "fig_MT_29.png", remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))
# mapshot(f, file = "fig_MT_96.png", remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))
