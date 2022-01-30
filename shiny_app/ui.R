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

shinyUI(
  tagList(
    fluidPage(
      title = "Evaluating Traffic Accident Risk for Nashville Roadways",
      div(
        class = "outer",
        tags$head(includeCSS("www/styles.css")),
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(
          id = "side_panel", 
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
                         "Pedestrian Collisions"),
            selected = "Accident Rate (per MVMT)"
          ),
          conditionalPanel(
            condition = "input.map_input == 'Annual Average Daily Traffic (AADT)'",       
            helpText(
              HTML("Estimates Based on 2020 Traffic Counts (Pre-Pandemic)"),
              style = "padding-left: 10px"
            )
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
      bsModal(
        id = "table_modal",
        title = HTML("<b>Accidents by Road Segment</b>"),
        trigger = "table_button",
        size = "large",
        DTOutput('data_table')
      )
    )
  )
)