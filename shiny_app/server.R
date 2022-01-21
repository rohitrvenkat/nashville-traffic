library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinycssloaders)
library(scales)
library(htmltools)
library(shinyBS)
library(shinyglide)
library(wesanderson)
library(plotly)
library(rrapply)

shinyServer(function(input, output, session) {
  
  # Render leaflet map
  output$mymap <- renderLeaflet({
    draw_base_map()
  })
  
  # Show 'Statistics' modal window with button click
  observeEvent(input$statsButton, {
    # toggleModal(session, modalId = "statsModal", toggle = "toggle")
    showModal(
      modalDialog(
        title = htmlOutput("title"),
        footer = NULL,
        size = "l",
        easyClose = T,
        fade = T,
        glide(
          id = "myglide",
          screen(
            column(
              width = 12, 
              tags$span(br(), br(), br(),
                        "Davidson County Population: 715,884 (as of April 1, 2020)",
                        style = "font-size: 16px; font-weight: bold")
            )
          ),
          screen(
            p("Please choose a value.")
          ),
          screen(
            column(
              width = 3,
              align = "center",
              br(),
              tags$a(img(src = "metro_nashville_transportation_plan.jpeg", style = "height: 100px;  margin-top: 10px"),
                     href="https://filetransfer.nashville.gov/portals/0/sitecontent/MayorsOffice/docs/Transportation/Metro-Nashville-Transportation-Plan-2020.pdf", target = "_blank"),
              tags$a(img(src = "ndot.png", style = "height: 100px; margin-top: 25px"),
                     href = "https://www.nashville.gov/departments/transportation",
                     target = "_blank"),
              tags$a(img(src = "vision_zero.png", style = "height: 100px; margin-top: 25px"),
                     href = "https://www.nashville.gov/departments/transportation/plans-and-programs/vision-zero",
                     target = "_blank")
            ),
            column(
              width = 9, 
              tags$span(br(), br(), br(),
                        "December 2020: Metro Council adopts Metro Nashville Transportation Plan",
                        br(), br(), br(), br(), br(),
                        "July 2021: Metro Nashville launches Nashville Department of Transportation and Multimodal Infrastructure (NDOT)",
                        br(), br(), br(), br(), br(),
                        "December 2021: Mayor Cooper and NDOT release Draft Vision Zero Action Plan",
                        style = "font-size: 16px; font-weight: bold; padding-left: 15px; display: inline-block")
            )

          ),
          screen(
            plotOutput("total_accidents_by_year")
          ),
          screen(
            plotOutput("injury_rate_by_year")
          )
        )
      )
    )
  })
  
  # Show 'About' modal window with button click
  observeEvent(input$aboutButton, {
    toggleModal(session, modalId = "aboutModal", toggle = "toggle")
  })
  
  # Create reactive values for zoom and modal title
  rv <- reactiveValues(zoom = "zoom_12", title = "Nashville: By The Numbers")

  # Render modal title text
  output$title <- renderText(paste0("<b>", rv$title, "</b>"))
  
  # Update modal title based on glide index
  observeEvent(input$shinyglide_index_myglide, {
    rv$title <- check_glide_index(input$shinyglide_index_myglide)
  })
  
  # Add 1000 ms delay to map zoom level
  zoom_delay <- debounce(function(){input$mymap_zoom}, 1000)
  
  # Update zoom reactive value based on map zoom level
  observeEvent(zoom_delay(), {
    rv$zoom <- check_zoom(zoom_delay())
  })
  
  # Update map based on zoom reactive value and map input
  observeEvent(c(rv$zoom, input$map_input), {
    update_map("mymap", rv$zoom, parse_map_input(input$map_input))
  })
  
  # Update map legend and sidebar plot based on map input
  observeEvent(input$map_input, {
    
    # Update map legend
    draw_map_legend("mymap", parse_map_input(input$map_input), input$map_input)
    
    # Update sidebar plot
    output$sidebar_plot <- renderPlotly({
      try({
        p = eval(parse(text = paste0(parse_map_input(input$map_input), "_plot")))
        
        ggplotly(p, tooltip = "text") %>% 
        config(displayModeBar = F) %>%
        layout(xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
        }, silent = T)
    })
    
  })
  
  output$total_accidents_by_year <- renderPlot(
    accidents_to_roads %>%
      group_by(Year = as.factor(year)) %>%
      summarize(`Total Accidents` = n(), 
                `Injury Accidents` = sum(injury, na.rm = T), 
                `Injury Rate` = `Injury Accidents` / `Total Accidents`,
                `Hit-and-Runs` = sum(hit_and_run, na.rm = T), 
                `Hit-and-Run Rate` = `Hit-and-Runs` / `Total Accidents`,
                `Pedestrian Collisions` = sum(pedestrian, na.rm = T)) %>%
      ggplot(aes(x = Year, y = `Total Accidents`, fill = Year, label = `Total Accidents`)) +
      geom_bar(stat = "identity") +
      geom_text(position = position_dodge(width = 0.9), vjust = -0.5,  size = 4) +
      theme(text = element_text(size = 15), legend.position = "none") +
      scale_fill_brewer(palette = "Spectral") 
  )
  
  output$injury_rate_by_year <- renderPlot(
    accidents_to_roads %>%
      group_by(Year = as.factor(year)) %>%
      summarize(`Total Accidents` = n(), 
                `Injury Accidents` = sum(injury, na.rm = T), 
                `Injury Rate` = `Injury Accidents` / `Total Accidents`,
                `Hit-and-Runs` = sum(hit_and_run, na.rm = T), 
                `Hit-and-Run Rate` = `Hit-and-Runs` / `Total Accidents`,
                `Pedestrian Collisions` = sum(pedestrian, na.rm = T)) %>%
      ggplot(aes(x = Year, y = `Injury Rate`, fill = Year, label = percent(`Injury Rate`, accuracy = 0.1))) +
      geom_bar(stat = "identity") +
      geom_text(position = position_dodge(width = 0.9), vjust = -0.5,  size = 4) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme(text = element_text(size = 15), legend.position = "none") +
      scale_fill_brewer(palette = "Spectral")
  )
  
})