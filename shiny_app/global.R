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

# Read in preprocessed datasets
accidents_by_road <- read_rds("data/accidents_by_road.rds")
accidents_by_road_table <- read_rds("data/accidents_by_road_table.rds")
plots <- read_rds("data/plots.rds")

# Create color palettes for map inputs
RdYlBu_pal <- colorNumeric(palette = "RdYlBu", domain = NULL, na.color = "#E5E5E5", reverse = T)
viridis_pal <- colorNumeric(palette = "viridis", domain = NULL, na.color = "#E5E5E5", reverse = F)
AADT_pal <- colorQuantile(palette = "RdYlBu", domain = NULL, probs = c(0, 0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.975, 1), na.color = "", reverse = T)
accident_rate_pal <- colorQuantile(palette = "RdYlBu", domain = NULL, probs = c(0, 0.25, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1), na.color = "", reverse = T)
injury_pct_pal <- colorQuantile(palette = "RdYlBu", domain = NULL, probs = c(0, 0.15, 0.3, 0.45, 0.6, 0.7, 0.8, 0.9, 1), na.color = "", reverse = T)
hit_and_run_pct_pal <- colorQuantile(palette = "RdYlBu", domain = NULL, probs = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1), na.color = "", reverse = T)

injury_pct_filtered <- ifelse(accidents_by_road$accidents >= 10, accidents_by_road$injury_pct, NA)
hit_and_run_pct_filtered <- ifelse(accidents_by_road$accidents >= 10, accidents_by_road$hit_and_run_pct, NA)

accidents_by_road$accidents_hex <- RdYlBu_pal(log2(accidents_by_road$accidents + 1))
accidents_by_road$accident_rate_hex <- accident_rate_pal(accidents_by_road$accident_rate)
accidents_by_road$AADT_hex <- AADT_pal(accidents_by_road$AADT)
accidents_by_road$injury_hex <- RdYlBu_pal(log2(accidents_by_road$injury + 1))
accidents_by_road$injury_pct_hex <- injury_pct_pal(injury_pct_filtered)
accidents_by_road$hit_and_run_hex <- RdYlBu_pal(log2(accidents_by_road$hit_and_run + 1))
accidents_by_road$hit_and_run_pct_hex <- hit_and_run_pct_pal(hit_and_run_pct_filtered)
accidents_by_road$pedestrian_hex <- viridis_pal(log2(accidents_by_road$pedestrian + 1))

AADT_pal_colors <- unique(AADT_pal(sort(accidents_by_road$AADT)))
AADT_pal_quantiles <- round(quantile(accidents_by_road$AADT, c(0, 0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.975, 1), na.rm = T))
AADT_pal_labels <- paste(lag(AADT_pal_quantiles), "\u2013", AADT_pal_quantiles)[-1]

accident_rate_pal_colors <- unique(accident_rate_pal(sort(accidents_by_road$accident_rate)))
accident_rate_pal_quantiles <- round(quantile(accidents_by_road$accident_rate, c(0, 0.25, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1), na.rm = T))
accident_rate_pal_labels <- paste(lag(accident_rate_pal_quantiles), "\u2013", accident_rate_pal_quantiles)[-1]

injury_pct_pal_colors <- unique(injury_pct_pal(sort(injury_pct_filtered)))
injury_pct_pal_quantiles <- percent(quantile(injury_pct_filtered, c(0, 0.15, 0.3, 0.45, 0.6, 0.7, 0.8, 0.9, 1), na.rm = T), accuracy = 1)
injury_pct_pal_labels <- paste(lag(injury_pct_pal_quantiles), "\u2013", injury_pct_pal_quantiles)[-1]

hit_and_run_pct_pal_colors <- unique(hit_and_run_pct_pal(sort(hit_and_run_pct_filtered)))
hit_and_run_pct_pal_quantiles <- percent(quantile(hit_and_run_pct_filtered, c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1), na.rm = T), accuracy = 1)
hit_and_run_pct_pal_labels <- paste(lag(hit_and_run_pct_pal_quantiles), "\u2013", hit_and_run_pct_pal_quantiles)[-1]

# Subset roads by functional class
interstates <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / INTERSTATE", "U / INTERSTATE"))

freeways <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("U / FWY OR EXP"))

arterial_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / MIN ART", "U / MIN ART", "R / OTH PRIN ART", "U OTH PRIN  ART"))

collector_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / MAJ COL", "U / MAJ COL", "R / MIN COL", "U / MIN COL"))

local_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / LOCAL", "U / LOCAL"))

# Define map line weights
line_weights <- bind_cols(
  expand_grid(
    road = c("local_roads",  "collector_roads", "arterial_roads", "freeways", "interstates"), 
    zoom = c("zoom_12", "zoom_13", "zoom_14", "zoom_15", "zoom_16"),
    option = c("default", "highlight")
  ),
  weight = c(0.1, 0.2, 0.25, 0.5, 0.5, 1, 1, 2, 1.5, 3,
             0.75, 1.5, 1, 2, 1.5, 3, 2, 3, 3, 4.5,
             1.5, 3, 2, 3, 2.5, 3.75, 3, 4.5, 4, 6,
             2.5, 3.75, 3, 4.5, 3.5, 5.25, 4.5, 6.75, 5.5, 8.25,
             3.5, 5.25, 4, 6, 5, 7.5, 6, 9, 8, 12)) %>% 
  group_by(road, option, zoom) %>%
  summarize(weight = list(weight), .groups = "drop") %>%
  rrapply(how = "unmelt")

# Initialize leaflet map
draw_base_map <- function() {
  
  leaflet(options = leafletOptions(minZoom = 12, maxZoom = 18, dragging = T)) %>%
    addProviderTiles(provider = "CartoDB.DarkMatter", group = "Dark Theme") %>%
    addProviderTiles(provider = "CartoDB.Positron", group = "Light Theme") %>%
    setView(lng = -86.7816, lat = 36.1627, zoom = 12) %>%
    setMaxBounds(lng1 = -86.7816 + 1, 
                 lat1 = 36.1627 + 1, 
                 lng2 = -86.7816 - 1, 
                 lat2 = 36.1627 - 1) %>%
    addResetMapButton() %>%
    addEasyButton(
      easyButton(
        icon = "fa-table",
        title = "Data Table",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange('table_button', Math.random());
          }")
      )
    ) %>%
    addEasyButton(
      easyButton(
        icon = "fa-info",
        title = "Info",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange('info_button', Math.random());
          }")
      )
    ) %>%
    addLayersControl(baseGroups = c("Dark Theme", "Light Theme"),
                     overlayGroups = c("Interstates / Freeways", 
                                       "Arterial Roads", 
                                       "Collector Roads", 
                                       "Local Roads"), 
                     position = "bottomleft")
}

# Update leaflet map
update_map <- function(mymap, zoom, map_input) {
  
  leafletProxy(mymap) %>%
    addPolylines(data = local_roads,
                 layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                 group = "Local Roads",
                 label = ~label,
                 color = ~eval(parse(text = paste0(map_input, "_hex"))),
                 opacity = ifelse(local_roads$accidents >= 10, 0.8, 0),
                 weight = line_weights$local_roads$default[[zoom]],
                 highlight = highlightOptions(
                   weight = line_weights$local_roads$highlight[[zoom]],
                   bringToFront = F)) %>%
    addPolylines(data = collector_roads,
                 layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                 group = "Collector Roads",
                 label = ~label,
                 color = ~eval(parse(text = paste0(map_input, "_hex"))),
                 opacity = 0.8,
                 weight = line_weights$collector_roads$default[[zoom]],
                 highlight = highlightOptions(
                   weight = line_weights$collector_roads$highlight[[zoom]],
                   opacity = 1,
                   bringToFront = F)) %>%
    addPolylines(data = arterial_roads,
                 layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                 group = "Arterial Roads",
                 label = ~label,
                 color = ~eval(parse(text = paste0(map_input, "_hex"))),
                 opacity = 0.8,
                 weight = line_weights$arterial_roads$default[[zoom]],
                 highlight = highlightOptions(
                   weight = line_weights$arterial_roads$highlight[[zoom]],
                   opacity = 1,
                   bringToFront = F)) %>%
    addPolylines(data = freeways,
                 layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                 group = "Interstates / Freeways",
                 label = ~label,
                 color = ~eval(parse(text = paste0(map_input, "_hex"))),
                 opacity = 0.8,
                 weight = line_weights$freeways$default[[zoom]],
                 highlight = highlightOptions(
                   weight = line_weights$freeways$highlight[[zoom]],
                   opacity = 1,
                   bringToFront = F)) %>%
    addPolylines(data = interstates,
                 layerId = ~paste(ID_NUMBER, BLM, ELM, sep = "_"),
                 group = "Interstates / Freeways",
                 label = ~label,
                 color = ~eval(parse(text = paste0(map_input, "_hex"))),
                 opacity = 0.8,
                 weight = line_weights$interstates$default[[zoom]],
                 highlight = highlightOptions(
                   weight = line_weights$interstates$highlight[[zoom]],
                   opacity = 1,
                   bringToFront = F))
}

# Update map view based on road segment location
update_map_view <- function(mymap, coordinates) {
  
  leafletProxy(mymap) %>%
    setView(lng = coordinates[1], lat = coordinates[2], zoom = 16)
}

# Update leaflet map legend based on map input
update_map_legend <- function(mymap, map_input, title) {

  if(map_input %in% c("AADT", "accident_rate", "injury_pct", "hit_and_run_pct")) {

    leafletProxy(mymap) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        colors = eval(parse(text = paste0(map_input, "_pal_colors"))),
        labels = eval(parse(text = paste0(map_input, "_pal_labels"))),
        title = title,
        opacity = 1
      )

  } else if(map_input %in% c("pedestrian")) {

    leafletProxy(mymap, data = accidents_by_road) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = viridis_pal,
        values = ~log2(eval(parse(text = map_input)) + 1),
        labFormat = labelFormat(transform = function(x) round(2^x)),
        title = "Pedestrian<br>Collisions",
        opacity = 1
      )

  } else {

    leafletProxy(mymap, data = accidents_by_road) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = RdYlBu_pal,
        values = ~log2(eval(parse(text = map_input)) + 1),
        labFormat = labelFormat(transform = function(x) round(2^x)),
        title = title,
        opacity = 1
      )
  }
}

# Convert zoom numeric to string
check_zoom <- function(zoom) {
  case_when(
    zoom == 12 ~ "zoom_12",
    zoom == 13 ~ "zoom_13",
    zoom == 14 ~ "zoom_14",
    zoom == 15 ~ "zoom_15",
    TRUE ~ "zoom_16"
  )
}

# Convert map input to column name
parse_map_input <- function(map_input) {
  case_when(
    map_input == "Annual Average Daily Traffic (AADT)" ~ "AADT",
    map_input == "Accident Rate (per MVMT)" ~ "accident_rate",
    map_input == "Total Accidents" ~ "accidents",
    map_input == "Injury Accidents" ~ "injury",
    map_input == "Injury Percentage" ~ "injury_pct",
    map_input == "Hit-and-Runs" ~ "hit_and_run",
    map_input == "Hit-and-Run Percentage" ~ "hit_and_run_pct",
    map_input == "Pedestrian Collisions" ~ "pedestrian",
  )
}

# Convert glide index to info modal title
check_glide_index <- function(glide_index) {
  case_when(
    glide_index == 0 ~ "Evaluating Traffic Accident Risk for Nashville Roadways",
    glide_index == 1 ~ "Traffic Accidents Are on the Rise in Nashville",
    glide_index == 2 ~ "Nashville Roadways Are Becoming Less Safe",
    glide_index == 3 ~ "Why is Nashville's Traffic Situation Worsening?",
    glide_index == 4 ~ "Traffic Fatalities Are an Unfortunate Result of a Worsening Traffic Situation",
    glide_index == 5 ~ "Nashville: By the Numbers",
    glide_index == 6 ~ "About This App",
    glide_index == 7 ~ "Acknowledgements"
  )
}

# Create info modal content
info_modal <- modalDialog(
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
        align = "center",
        tags$img(src = "traffic_skyline_logo.png", style = "height: 400px;"),
        tags$span(
          HTML("This app aims to identify Nashville roadways that have an increased risk for traffic accidents<br>
               to help prioritize road infrastructure projects for improving traffic safety"),
          style = "font-size: 18px; text-align: center; padding: 20px 15px 10px; display: inline-block"
        )
      )
    ),
    screen(
      column(
        width = 12,
        tags$br(),
        tags$br(),
        plotOutput("total_accidents_by_year")
      )
    ),
    screen(
      column(
        width = 12,
        tags$br(),
        tags$br(),
        plotOutput("injury_pct_by_year")
      )
    ),
    screen(
      column(
        width = 12,
        tags$br(),
        tags$span("1. Overreliance on motor vehicles",
                  style = "font-size: 18px; margin-bottom: 15px; padding-left: 95px; display: inline-block"),
        tags$span("2. Lack of existing infrastructure to easily implement mass transit",
                  style = "font-size: 18px; margin-bottom: 15px; padding-left: 95px; display: inline-block"),
        tags$span("3. Underdeveloped neighborhood infrastructure (sidewalks, bikeways, greenways)",
                  style = "font-size: 18px; margin-bottom: 15px; padding-left: 95px; display: inline-block"),
        tags$span("4. Rapid population growth leading to strained roadways and traffic congestion",
                  style = "font-size: 18px; margin-bottom: 30px; padding-left: 95px; display: inline-block"),
        column(
          width = 12,
          align = "center",
          tags$img(src = "transportation_mode.png", style = "height: 275px;")
        )
      )
    ),
    screen(
      column(
        width = 12,
        align = "center",
        tags$br(),
        tags$br(),
        tags$img(src = "traffic_deaths.png", style = "width: 100%")
      )
    ),
    screen(
      column(
        width = 12,
        align = "center",
        tags$br(),
        tags$br(),
        tags$img(src = "ksi_statistics.png", style = "width: 100%")
      )
    ),
    screen(
      column(
        width = 10,
        offset = 1,
        style = "padding-left: 0; padding-top: 30px",
        tags$span(
          HTML("Understanding traffic accident patterns through data analytics and visualization provides
               the ability to identify and implement traffic safety improvement strategies, including but
               not limited to improved roadway design and traffic policies. For this app, 234,640 traffic
               accidents reported to the Metro Nashville Police Department between 2014 - 2021 were mapped
               to 9,289 Davidson County road segments in order to identify high-risk roadways.<br><br>
               <b>A few questions this app aims to address:</b><br><br>",
               "<ul><li>Which roads have the highest traffic accident rates?</li><br>",
               "<li>Which roads have the highest percentage of traffic accidents resulting in injury?</li><br>",
               "<li>Which roads have the highest percentage of hit-and-runs?</li><br>",
               "<li>Which roads have the highest number of pedestrian collisions?</li></ul>"),
          style = "font-size: 18px"
        )
      )
    ),
    screen(
      column(
        width = 10,
        offset = 1,
        style = "padding-left: 0",
        tags$h3("Data Sources", style = "margin-top: 30px"),
        tags$span(tags$a("Metro Nashville Police Department",
                         href = "https://data.nashville.gov/Police/Traffic-Accidents/6v6w-hpcw",
                         target = "_blank"), br(),
                  tags$a("Tennessee Department of Transportation",
                         href = "https://www.tn.gov/tdot/long-range-planning-home/longrange-road-inventory/longrange-road-inventory-trims-data-request.html",
                         target = "_blank"),
                  style = "font-size: 16px")
      ),
      column(
        width = 10,
        offset = 1,
        style = "padding-left: 0",
        tags$h3("References & Image Sources", style = "margin-top: 30px"),
        tags$span(tags$a("Metro Nashville Vision Zero",
                         href = "https://www.nashville.gov/departments/transportation/plans-and-programs/vision-zero",
                         target = "_blank"), br(),
                  tags$a("Federal Highway Administration",
                         href = "https://safety.fhwa.dot.gov/local_rural/training/fhwasa1210/s3.cfm",
                         target = "_blank"), br(),
                  tags$a("Nashville Connector",
                         href = "https://nashconnector.org/",
                         target = "_blank"),
                  style = "font-size: 16px;")
      ),
      column(
        width = 10,
        offset = 1,
        style = "padding-left: 0",
        tags$h3(
          HTML("Application Author &nbsp"),
          actionButton(inputId = "github",
                       label = "",
                       icon = icon("fab fa-github"),
                       onclick = "window.open('https://github.com/rohitrvenkat', '_blank')"),
          actionButton(inputId = "linkedin",
                       label = "",
                       icon = icon("fab fa-linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/rohit-venkat/', '_blank')"),
          style = "margin-top: 30px"
        ),
        tags$span(
          HTML("My name is Rohit Venkat, and I am a data scientist-in-training at Nashville Software
          School! I would like to thank lead instructor Michael Holloway and teaching assistants Alvin
          Wendt and Veronica Ikeshoji-Orlati for their feedback, suggestions, and support during this
          project. I would also like to express a special thanks to Jon Slinker and colleagues at TDOT
          for providing the road centerlines and traffic volume data for this project and for their
          helpfulness in directing me to resources."),
          style = "font-size: 16px"
        )
      )
    )
  )
)