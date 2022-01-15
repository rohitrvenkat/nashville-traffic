library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinycssloaders)

accidents_by_road <- readRDS("data/accidents_by_road.rds")
map_variables <- c("accidents",
                   "AADT",
                   "injury", 
                   "injury_pct", 
                   "multi_injury", 
                   "multi_injury_pct", 
                   "hit_and_run",
                   "hit_and_run_pct")

accidents_by_road$label <-
  paste0("<b>", accidents_by_road$RTE_NME, "</b> (", 
         accidents_by_road$ID_NUMBER, ")<br>",
         "<b>AADT:</b> ", accidents_by_road$AADT, "<br>",
         "<b>Accidents:</b> ", accidents_by_road$accidents, "<br>",
         "<b>Injury Accidents:</b> ", accidents_by_road$injury, " (", 
         scales::percent(accidents_by_road$injury_pct, accuracy = 0.1), ")<br>",
         "<b>Multi-Injury Accidents:</b> ", accidents_by_road$multi_injury, " (",
         scales::percent(accidents_by_road$multi_injury_pct, accuracy = 0.1), ")<br>",
         "<b>Hit-and-Runs:</b> ", accidents_by_road$hit_and_run, " (",
         scales::percent(accidents_by_road$hit_and_run_pct, accuracy = 0.1), ")") %>%
  lapply(htmltools::HTML)

RdYlBu_pal <- colorNumeric(palette = "RdYlBu", domain = NULL, na.color = "#E5E5E5", reverse = T)
RdBu_pal <- colorNumeric(palette = "RdBu", domain = NULL, na.color = "#E5E5E5", reverse = T)
Purples_pal <- colorNumeric(palette = "Purples", domain = NULL, na.color = "#E5E5E5", reverse = F)
Reds_pal <- colorNumeric(palette = "Reds", domain = NULL, na.color = "#E5E5E5", reverse = F)

aadt_pal <- colorQuantile(palette = "RdYlBu", 
                          probs = c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.945, 0.961, 1), 
                          domain = NULL, 
                          na.color = "#E5E5E5", 
                          reverse = T)

injury_pct_pal <- colorQuantile(palette = "RdYlBu", 
                                probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1), 
                                domain = NULL, 
                                na.color = "#E5E5E5", 
                                reverse = T)

hit_and_run_pct_pal <- colorQuantile(palette = "RdYlBu", 
                                     probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                     domain = NULL, 
                                     na.color = "#E5E5E5", 
                                     reverse = T)

accidents_by_road$accidents_hex <- RdYlBu_pal(logb(accidents_by_road$accidents + 1))
accidents_by_road$AADT_hex <- aadt_pal(accidents_by_road$AADT)
accidents_by_road$injury_hex <- RdYlBu_pal(logb(accidents_by_road$injury + 1))
accidents_by_road$injury_pct_hex <- injury_pct_pal(accidents_by_road$injury_pct)
accidents_by_road$multi_injury_hex <- RdYlBu_pal(logb(accidents_by_road$multi_injury + 1))
accidents_by_road$multi_injury_pct_hex <- RdYlBu_pal(logb(accidents_by_road$multi_injury_pct + 1))
accidents_by_road$hit_and_run_hex <- RdYlBu_pal(logb(accidents_by_road$hit_and_run + 1))
accidents_by_road$hit_and_run_pct_hex <- hit_and_run_pct_pal(accidents_by_road$hit_and_run_pct)

interstates <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / INTERSTATE",
                           "U / INTERSTATE",
                           "U / FWY OR EXP"))

arterial_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / MIN ART",
                           "U / MIN ART",
                           "R / OTH PRIN ART",
                           "U / MIN ART",
                           "U OTH PRIN  ART"))

collector_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / MAJ COL",
                           "R / MIN COL",
                           "U / MAJ COL"))

local_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / LOCAL",
                           "U / LOCAL"))

draw_base_map <- function() {
  
  leaflet(options = leafletOptions(minZoom = 12, maxZoom = 18, dragging = T)) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    setView(lng = -86.7816, lat = 36.1627, zoom = 12) %>%
    setMaxBounds(lng1 = -86.7816 + 1, 
                 lat1 = 36.1627 + 1, 
                 lng2 = -86.7816 - 1, 
                 lat2 = 36.1627 - 1) %>%
    addResetMapButton() %>%
    addLayersControl(overlayGroups = c("Interstates", "Arterial Roads", "Collector Roads", "Local Roads"), 
                     position = "bottomleft")
    
}

update_map <- function(mymap, scope, map_input) {

  if(scope == "zoom_12") {

    leafletProxy(mymap) %>%
      removeShape(layerId = paste(local_roads$ID_NUMBER, local_roads$BLM, local_roads$ELM, sep = "_")) %>%
      addPolylines(data = collector_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Collector Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 0.5,
                   highlight = highlightOptions(
                     weight = 1,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = arterial_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Arterial Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 2,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = interstates,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Interstates",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 3,
                   highlight = highlightOptions(
                     weight = 4.5,
                     fillOpacity = 1,
                     bringToFront = F))

  } else if(scope == "zoom_13") {

    leafletProxy(mymap) %>%
      addPolylines(data = local_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Local Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 0.25,
                   highlight = highlightOptions(
                     weight = 0.5,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = collector_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Collector Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 0.75,
                   highlight = highlightOptions(
                     weight = 1.5,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = arterial_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Arterial Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1.5,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = interstates,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Interstates",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 3.5,
                   highlight = highlightOptions(
                     weight = 5.25,
                     fillOpacity = 1,
                     bringToFront = F))

  } else if(scope == "zoom_14") {

    leafletProxy(mymap) %>%
      addPolylines(data = local_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Local Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 0.5,
                   highlight = highlightOptions(
                     weight = 1,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = collector_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Collector Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 2,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = arterial_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Arterial Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 2,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = interstates,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Interstates",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 4,
                   highlight = highlightOptions(
                     weight = 6,
                     fillOpacity = 1,
                     bringToFront = F))

  } else if(scope == "zoom_15") {

    leafletProxy(mymap) %>%
      addPolylines(data = local_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Local Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 2,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = collector_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Collector Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1.5,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = arterial_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Arterial Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 2.5,
                   highlight = highlightOptions(
                     weight = 3.75,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = interstates,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Interstates",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 5,
                   highlight = highlightOptions(
                     weight = 7.5,
                     fillOpacity = 1,
                     bringToFront = F))

    } else if(scope == "zoom_16") {

    leafletProxy(mymap) %>%
      addPolylines(data = local_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Local Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 1.5,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = collector_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Collector Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 2,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = arterial_roads,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Arterial Roads",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 3,
                   highlight = highlightOptions(
                     weight = 4.5,
                     fillOpacity = 1,
                     bringToFront = F)) %>%
      addPolylines(data = interstates,
                   layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                   group = "Interstates",
                   label = ~label,
                   color = ~eval(parse(text = paste0(map_input, "_hex"))),
                   opacity = 0.8,
                   weight = 6,
                   highlight = highlightOptions(
                     weight = 9,
                     fillOpacity = 1,
                     bringToFront = F))
  }
}

check_zoom <- function(zoom) {
  case_when(
    zoom == 12 ~ "zoom_12",
    zoom == 13 ~ "zoom_13",
    zoom == 14 ~ "zoom_14",
    zoom == 15 ~ "zoom_15",
    TRUE ~ "zoom_16"
  )
}