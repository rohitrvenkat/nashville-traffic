library(shiny)
library(tidyverse)
library(scales)
library(rrapply)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shinyBS)
library(shinyglide)
library(plotly)
library(DT)

shinyServer(function(input, output, session) {
  
  # Render leaflet map
  output$mymap <- renderLeaflet({
    draw_base_map()
  })
  
  # Create reactive values for zoom and info modal title
  rv <- reactiveValues(zoom = "zoom_12", title = "")
  
  # Add 1000 ms delay to map zoom level
  zoom_delay <- debounce(function(){input$mymap_zoom}, 1000)
  
  # Update zoom reactive value based on map zoom level
  observeEvent(
    eventExpr = zoom_delay(),
    handlerExpr = {
      rv$zoom <- check_zoom(zoom_delay())
    }
  )
  
  # Update map based on zoom reactive value and map input
  observeEvent(
    eventExpr = c(rv$zoom, input$map_input),
    handlerExpr = update_map("mymap", rv$zoom, parse_map_input(input$map_input))
  )
  
  # Update map legend and sidebar plot based on map input
  observeEvent(
    eventExpr = input$map_input, 
    handlerExpr = {
      
      # Update map legend
      update_map_legend("mymap", parse_map_input(input$map_input), input$map_input)
      
      # Update sidebar plot
      output$sidebar_plot <- renderPlotly(
        plots[[parse_map_input(input$map_input)]] %>%
          ggplotly(tooltip = "text") %>%
          config(displayModeBar = F) %>%
          layout(xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
      )
    }
  )
  
  # Show data table modal with button click
  observeEvent(
    eventExpr = input$table_button,
    handlerExpr = toggleModal(session, modalId = "table_modal", toggle = "toggle")
  )
  
  # Render data table
  output$data_table <- renderDT(
    server = T, {
      datatable(
        data = accidents_by_road_table,
        class = c("compact", "stripe"),
        rownames = F,
        filter = "top",
        escape = F,
        style = 'bootstrap',
        selection = "none",
        options = list(
          pageLength = 10,
          autoWidth = T,
          scrollX = T,
          dom = 'rtip',
          columnDefs = list(
            list(searchable = F, targets = 0),
            list(visible = F, targets = 12)
          )
        )
      ) %>%
        formatPercentage(columns = c("Injury %", "Hit-and-Run %"), digits = 1) %>%
        formatStyle(columns = c(1:12), "vertical-align" = "middle")
    })
  
  # Update map view with button click
  observeEvent(
    eventExpr = input$locate_button,
    handlerExpr = {
      index <- as.numeric(strsplit(input$locate_button, "_")[[1]][2])
      coordinates <- unlist(accidents_by_road_table$geometry[index])
      update_map_view("mymap", coordinates)
      toggleModal(session, modalId = "table_modal", toggle = "hide")
    }
  )
  
  # Show info modal with button click
  observeEvent(
    eventExpr = input$info_button,
    handlerExpr = showModal(info_modal)
  )
  
  # Update info modal title based on glide index
  observeEvent(
    eventExpr = input$shinyglide_index_myglide,
    handlerExpr = {
      rv$title <- check_glide_index(input$shinyglide_index_myglide)
    }
  )
  
  # Render info modal title
  output$title <- renderText(paste0("<b>", rv$title, "</b>"))
  
  # Render info modal total accidents by year plot 
  output$total_accidents_by_year <- renderPlot(plots[["total_accidents_by_year"]])
  
  # Render info modal injury percentage by year plot
  output$injury_pct_by_year <- renderPlot(plots[["injury_pct_by_year"]])
})