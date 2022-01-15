library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinycssloaders)

shinyUI(
  tagList(
    navbarPage(
      
      use_theme(
        create_theme(
          theme = "default",
          bs_vars_navbar(
            default_bg = "#231E5F",
            default_link_color = "#FBCA06",
            default_link_active_color = "#FFFFFF",
            default_link_hover_color = "#FFFFFF",
            height = "45px",
          ),
          bs_vars_font(
            family_sans_serif = "Lato",
            size_base = "14px",
          ),
          bs_vars_color(
            brand_primary = "#FBCA06",
          )
        )
      ),
      
      tags$style(type = 'text/css', 
                 '.page-footer {color: #999A9E}'
      ),
      
      windowTitle = "Evaluating Traffic Accident Risk for Nashville Roadways",
      
      tabPanel(h4("Map"),
               div(class = "outer",
                   tags$head(
                     includeCSS("www/styles.css")
                   ),
                   leafletOutput("mymap", width = "100%", height = "100%"),
                   absolutePanel(top = 10, 
                                 right = 10,
                                 selectInput("map_input", "Map Input", map_variables)
                   )
               )
      ),
      
      tabPanel(h4("Plots"),
               navlistPanel(
                 "Header",
                 tabPanel("First"),
                 tabPanel("Second"),
                 tabPanel("Third")
               ))
    )
  )
)