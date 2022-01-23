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
          selectInput(
            inputId = "map_input", 
            label = "Map Input", 
            choices =  c("Annual Average Daily Traffic (AADT)",
                         "Accident Rate (per MVMT)",
                         "Total Accidents",
                         "Injury Accidents", 
                         "Injury Percentage",
                         "Hit-and-Runs",
                         "Hit-and-Run Percentage",
                         "Pedestrian Collisions")
          ),
          conditionalPanel(
            condition = "input.map_input == 'Accident Rate (per MVMT)'",       
            helpText(
              withMathJax("Accident Rate per 1 Million Vehicles Miles Traveled:", br(),
                          "$${R}=\\frac{1{,}000{,}000\\times{C}}{{365}\\times{N}\\times{V}\\times{L}}$$",
                          "C = Total Number of Crashes", br(),
                          "N = Number of Years of Data", br(),
                          "V = Traffic Volume Based on AADT", br(),
                          "L = Length of Roadway Segment"),
              style = "padding-left: 10px"
            )
          ),
          conditionalPanel(
            condition = "!['Annual Average Daily Traffic (AADT)', 'Accident Rate (per MVMT)'].includes(input.map_input)",
            plotlyOutput(outputId = "sidebar_plot", height = 200)
          )
        )
      ),
      tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px }' ))),
      bsModal(
        id = "tableModal", 
        title = HTML("<b>Accidents by Roadway Segment</b>"), 
        trigger = "tableButton", 
        size = "large",
        div(
          DTOutput('data_table'), 
          style = "font-size: 100%",
          align = "center")
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
            h4("Data Sources", style = "margin-top: 15px"),
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
            h4("Application Author", style = "margin-top: 25px"),
            tags$span("Hello, my name is ",
                      tags$a("Rohit Venkat", 
                             href = "https://www.linkedin.com/in/rohit-venkat/", 
                             target = "_blank", 
                             .noWS = "after"),
                      "! I am a data scientist-in-training at Nashville Software School.", 
                      style = "font-size: 14px")
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
    )
  )
)