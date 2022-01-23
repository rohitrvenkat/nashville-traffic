library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(scales)
library(htmltools)
library(shinyBS)
library(shinyglide)
library(wesanderson)
library(plotly)
library(rrapply)
library(DT)

shinyServer(function(input, output, session) {
  
  # Render leaflet map
  output$mymap <- renderLeaflet({
    draw_base_map()
  })
  
  # Show 'Presentation' modal window with button click
  observeEvent(input$presentationButton, {
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
            plotOutput("total_accidents_by_year")
          ),
          screen(
            plotOutput("injury_pct_by_year")
          ),
          screen(
            column(
              width = 12, 
              tags$span(br(),
                        "1. Lack of Existing Infrastructure to Allow for Easily Implemented Mass Transit",
                        br(), br(),
                        "2. Rapid Population Growth Leading to Strained Roadways and Traffic Congestion",
                        br(), br(),
                        "3. Underdeveloped Neighborhood Infrastructure (Sidewalks, Bikeways, Greenways)",
                        style = "font-size: 18px; font-weight: bold; padding-left: 45px; display: inline-block"),
              column(
                width = 12,
                align = "center",
                br(),
                img(src = "transportation_mode.png", style = "height: 200px;")
              )
            )
          ),
          screen(
            column(
              width = 12,
              align = "center",
              br(),
              img(src = "ksi_statistics.png", style = "height: 300px;")
            )
          ),
          screen(
            column(
              width = 12,
              align = "center",
              br(),
              img(src = "traffic_deaths.png", style = "height: 350px;")
            )
          ),
          # screen(
          #   column(
          #     width = 3,
          #     align = "center",
          #     br(),
          #     tags$a(img(src = "metro_nashville_transportation_plan.jpeg", style = "height: 100px;  margin-top: 10px"),
          #            href="https://filetransfer.nashville.gov/portals/0/sitecontent/MayorsOffice/docs/Transportation/Metro-Nashville-Transportation-Plan-2020.pdf", target = "_blank"),
          #     tags$a(img(src = "ndot.png", style = "height: 100px; margin-top: 25px"),
          #            href = "https://www.nashville.gov/departments/transportation",
          #            target = "_blank"),
          #     tags$a(img(src = "vision_zero.png", style = "height: 100px; margin-top: 25px"),
          #            href = "https://www.nashville.gov/departments/transportation/plans-and-programs/vision-zero",
          #            target = "_blank")
          #   ),
          #   column(
          #     width = 9,
          #     tags$span(br(), br(), br(),
          #               "December 2020: Metro Council adopts Metro Nashville Transportation Plan",
          #               br(), br(), br(), br(), br(),
          #               "July 2021: Metro Nashville launches Nashville Department of Transportation and Multimodal Infrastructure (NDOT)",
          #               br(), br(), br(), br(), br(),
          #               "December 2021: Mayor Cooper and NDOT release Draft Vision Zero Action Plan",
          #               style = "font-size: 16px; font-weight: bold; padding-left: 15px; display: inline-block")
          #   )
          # ),
          screen(
            column(
              width = 12, 
              tags$span(br(),
                        "This app aims to identify Nashville roadway that have an increased risk for traffic accidents to help prioritize road infrastructure projects for improving traffic safety.",
                        style = "font-size: 22px; font-weight: bold; padding-left: 15px; display: inline-block"),
            ),
            column(
              width = 6,
              offset = 1,
              tags$span(br(), br(),
                        HTML("<b>Datasets:</b>"), br(),
                        HTML("<ul><li>234,640 Traffic Accidents (2014-2021)</li>"),
                        HTML("<li>9,291 Roadway Geometries</li></ul>"),
                        style = "font-size: 18px; padding-left: 15px; display: inline-block")
            ),
            column(
              width = 5,
              offset = 0,
              tags$span(br(), br(),
                        HTML("<b>Factors Assessed:</b>"), br(),
                        HTML("<ul><li>Roadway Accident Rates</li>"),
                        HTML("<li>Total Accidents</li>"),
                        HTML("<li>Injury Accidents</li>"),
                        HTML("<li>Hit-and-Runs</li></li>"),
                        HTML("<li>Pedestrian Collisions</li></ul>"), br(),
                        style = "font-size: 18px; padding-left: 15px; display: inline-block")
            )
          )
        )
      )
    )
  })
  
  # Show 'Table' modal window with button click
  observeEvent(
    eventExpr = input$tableButton,
    handlerExpr = toggleModal(session, modalId = "tableModal", toggle = "toggle")
  )
  
  # Show 'About' modal window with button click
  observeEvent(
    eventExpr = input$aboutButton, 
    handlerExpr = toggleModal(session, modalId = "aboutModal", toggle = "toggle")
  )
  
  # Create reactive values for zoom, modal title, and road segment coordinates
  rv <- reactiveValues(zoom = "zoom_12", 
                       title = "", 
                       coordinates = "")
  
  
  output$data_table <- renderDT(
    server = T, {
      datatable(
        data = accidents_by_road_DT,
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
  
  
  observeEvent(
    eventExpr = input$locateButton, 
    handlerExpr = {
      selectedRow <- as.numeric(strsplit(input$locateButton, "_")[[1]][2])
      rv$coordinates <- unlist(st_centroid(accidents_by_road_DT$geometry[selectedRow]))
      toggleModal(session, modalId = "tableModal", toggle = "hide")
    }
  )
  
  # Render modal title text
  output$title <- renderText(paste0("<b>", rv$title, "</b>"))
  
  # Update modal title based on glide index
  observeEvent(
    eventExpr = input$shinyglide_index_myglide,
    handlerExpr = {
      rv$title <- check_glide_index(input$shinyglide_index_myglide)
    }
  )
  
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
  
  # Update map view based on coordinates reactive value 
  observeEvent(
    eventExpr = rv$coordinates,
    handlerExpr = update_map_view("mymap", rv$coordinates)
  )
  
  # Update map legend and sidebar plot based on map input
  observeEvent(
    eventExpr = input$map_input, 
    handlerExpr = {
    
    # Update map legend
    draw_map_legend("mymap", parse_map_input(input$map_input), input$map_input)
    
    # Update sidebar plot
    output$sidebar_plot <- renderPlotly(
      plots[[parse_map_input(input$map_input)]] %>%
        ggplotly(tooltip = "text") %>%
        config(displayModeBar = F) %>%
        layout(xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
      )
    }
  )
  
  output$total_accidents_by_year <- renderPlot(plots[["total_accidents_by_year"]])
  output$injury_pct_by_year <- renderPlot(plots[["injury_pct_by_year"]])
    
})