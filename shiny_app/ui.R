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

shinyUI(
  tagList(
    fluidPage(
      title = "Evaluating Traffic Accident Risk for Nashville Roadways",
      div(
        class = "outer",
        tags$head(includeCSS("www/styles.css")),
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(
          id = "sidePanel", 
          class = "panel panel-default", 
          fixed = TRUE,
          draggable = FALSE, 
          top = 15, 
          left = "auto", 
          right = 15, 
          bottom = "auto",
          width = 300, 
          height = 360,
          h4(HTML("<b>Evaluating Traffic Accident Risk<br>for Nashville Roadways</b>")),
          selectInput(inputId = "map_input", label = "Map Input", choices =  input_vars),
          plotlyOutput(outputId = "sidebar_plot", height = 200)
        )
      ),
      bsModal(
        id = "aboutModal", 
        title = HTML("<b>About this Application</b>"), 
        trigger = "aboutButton", 
        size = "large",
        div(
          style = "display: inline-block; vertical-align: bottom; min-width: 100%;",
          column(
            width = 6, 
            style = "padding-left: 0",
            h4("Data Sources", style = "margin-top: 15px;"),
            tags$span("Metro Nashville Police Department traffic accident data was obtained from the ",
                      tags$a("Nashville Open Data Portal", 
                             href = "https://data.nashville.gov/Police/Traffic-Accidents/6v6w-hpcw", 
                             target = "_blank", 
                             .noWS = "after"),
                      ". Road centerlines and traffic volume data were obtained by request from the ",
                      tags$a("Tennessee Department of Transportation",
                             href = "https://www.tn.gov/tdot/long-range-planning-home/longrange-road-inventory/longrange-road-inventory-trims-data-request.html",
                             target = "_blank",
                             .noWS = "after"),
                      ".", style = "font-size: 14px;"),
            br(),
            h4("Application Author", style = "margin-top: 25px;"),
            tags$span("Hello, my name is ",
                      tags$a("Rohit Venkat", 
                             href = "https://www.linkedin.com/in/rohit-venkat/", 
                             target = "_blank", 
                             .noWS = "after"),
                      "! I am a data scientist-in-training at Nashville Software School.", 
                      style = "font-size: 14px;")
          ),
          column(
            width = 6, 
            img(src = "logo.png", style = "height: 200px; display: block; \
                                           margin-top: 15px; margin-bottom: 10px; \
                                           margin-right: auto; margin-left: auto;"
            )
          )
        )
      )
      # bsModal(
      #   id = "statsModal", 
      #   title = htmlOutput("title"),
      #   trigger = "statsButton", 
      #   size = "large",
      #         
      #   glide(
      #     id = "myglide",
      #     screen(
      #       br(),
      #       p("This is a very simple shinyglide application."),
      #       p("Please click on Next to go to the next screen.")
      #     ),
      #     screen(
      #       br(),
      #       p("Please choose a value."),
      #       numericInput("n", "n", value = 10, min = 10)
      #     ),
      #     screen(
      #       plotOutput("total_accidents_by_year", height = "500px")
      #     )
      #     # screen(
      #     #   # plotOutput("injury_rate_by_year", height = "500px")
      #     # )
      #   )
      # )
    )
  )
)