library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinycssloaders)

shinyServer(function(input, output) {
  
  # Render leaflet map
  output$mymap <- renderLeaflet({
    draw_base_map()
  })
  
  # Create scope reactive value
  rv <- reactiveValues(scope = "low")
  
  # Update scope based on zoom level 
  observeEvent(input$mymap_zoom, {
    rv$scope <- check_zoom(input$mymap_zoom)
  })
  
  # Update map based on scope
  observeEvent(rv$scope, {
    update_map("mymap", rv$scope)
  })
  
})
