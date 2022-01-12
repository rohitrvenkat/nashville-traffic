library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinycssloaders)

roads <- readRDS("data/roads.rds")
accidents_to_roads <- readRDS("data/accidents_to_roads.rds")

accidents_by_road <- accidents_to_roads %>% 
  group_by(transport_edge_id) %>%
  summarize(accidents = n(), injury = sum(injury), hit_and_run = sum(hit_and_run)) %>%
  right_join(roads, by = "transport_edge_id") %>%
  mutate(accidents = replace_na(accidents, 0)) %>%
  mutate(injury = replace_na(injury, 0)) %>%
  mutate(hit_and_run = replace_na(hit_and_run, 0)) %>%
  st_sf(sf_column_name = 'geometry')

draw_base_map <- function() {
  leaflet(options = leafletOptions(minZoom = 11, maxZoom = 18, dragging = TRUE)) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    setView(lng = -86.7816, lat = 36.1627, zoom = 11) %>%
    setMaxBounds(lng1 = -86.7816 + 1, 
                 lat1 = 36.1627 + 1, 
                 lng2 = -86.7816 - 1, 
                 lat2 = 36.1627 - 1) %>%
    addResetMapButton()
}

update_map <- function(mymap, scope) {
  
  accidents_by_road$label <-
    paste0("<b>", accidents_by_road$full_name, "</b> (", 
           accidents_by_road$transport_edge_id, ")<br>",
           "<b>Accidents:</b> ", accidents_by_road$accidents, "<br>",
           "<b>Injury Accidents:</b> ", accidents_by_road$injury, "<br>",
           "<b>Hit-and-Runs:</b> ", accidents_by_road$hit_and_run) %>%
    lapply(htmltools::HTML)
  
  interstates <- accidents_by_road %>% filter(class %in% c(1,2))
  highways <- accidents_by_road %>% filter(class %in% c(3,4))
  main_roads <- accidents_by_road %>% filter(class %in% c(5))
  small_roads <- accidents_by_road %>% filter(class %in% c(6,7))
  
  pal <- colorNumeric(palette = "Purples", domain = NULL, reverse = F)
  
  if(scope == "low") {
    
    leafletProxy(mymap) %>% 
      clearShapes() %>% 
      addPolylines(data = highways, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 1.5,
                     fillOpacity = 0.95,
                     bringToFront = FALSE)) %>%
      addPolylines(data = interstates, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 2,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 0.95,
                     bringToFront = FALSE))
  
  } else if(scope == "med") {
    
    leafletProxy(mymap) %>% 
      clearShapes() %>% 
      addPolylines(data = main_roads, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 1.5,
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addPolylines(data = highways,
                   label = ~label,
                   color = ~pal(log10(accidents + 1)),
                   opacity = 0.8,
                   weight = 2,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addPolylines(data = interstates,
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 3,
                   highlight = highlightOptions(
                     weight = 4.5,
                     fillOpacity = 1,
                     bringToFront = FALSE))
    
  } else if(scope == "high") {
    
    leafletProxy(mymap) %>% 
      clearShapes() %>% 
      addPolylines(data = small_roads, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 1,
                   highlight = highlightOptions(
                     weight = 1.5,
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addPolylines(data = main_roads, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 2,
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addPolylines(data = highways, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 3,
                   highlight = highlightOptions(
                     weight = 4.5,
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addPolylines(data = interstates, 
                   label = ~label, 
                   color = ~pal(log10(accidents + 1)), 
                   opacity = 0.8, 
                   weight = 4,
                   highlight = highlightOptions(
                     weight = 6,
                     fillOpacity = 1,
                     bringToFront = FALSE))
  }
}

check_zoom <- function(zoom) {
  case_when(
    zoom <= 12 ~ "low",
    zoom <= 14 ~ "med",
    TRUE ~ "high"
  )
}