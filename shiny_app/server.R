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
  rv <- reactiveValues(scope = "zoom_12")
  
  # Add zoom delay
  zoom_delay <- debounce(function(){input$mymap_zoom}, 1000)
  
  # Update scope based on zoom level 
  observeEvent(zoom_delay(), {
    rv$scope <- check_zoom(zoom_delay())
  })
  
  # Update map based on scope
  observeEvent(c(rv$scope, input$map_input), {
    update_map("mymap", rv$scope, input$map_input)
  })
  
})
