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

# Read in rds datasets
accidents_by_road <- readRDS("data/accidents_by_road.rds")
accidents_to_roads <- readRDS("data/accidents_to_roads.rds")

# Prepare data for table
accidents_by_road_DT <- accidents_by_road %>%
  transmute(Locate = sapply(X = 1:n(),
                            FUN =  function(i) {
                              as.character(
                                actionButton(
                                  inputId = paste0("button_", i),
                                  label = "", 
                                  icon = icon("map-marker"),
                                  onclick = "Shiny.onInputChange('locateButton', this.id)"
                                )
                              )
                            }),
            Road = RTE_NME,
            `Length (mi.)` = round(LENGTH, digits = 3),
            AADT = as.integer(AADT),
            Accidents = as.integer(accidents),
            `Accident Rate` = round(accident_rate, digits = 1),
            `Injury Accidents` = as.integer(injury),
            `Injury %` = round(injury_pct, digits = 3),
            `Hit-and-Runs` = as.integer(hit_and_run),
            `Hit-and-Run %` = round(hit_and_run_pct, digits = 3),
            `Pedestrian Collisions` = as.integer(pedestrian)) %>%
  group_by(Road) %>%
  mutate(Segment = 1:n(), .after = Road) %>%
  ungroup()


RdYlBu_pal <- colorNumeric(palette = "RdYlBu", domain = NULL, na.color = "#E5E5E5", reverse = T)
viridis_pal <- colorNumeric(palette = "viridis", domain = NULL, na.color = "", reverse = F)
AADT_pal <- colorQuantile(palette = "RdYlBu", probs = c(seq(0, 0.75, 0.25), 0.85, 0.9, 0.95, 0.975, 1), domain = NULL, na.color = "", reverse = T)
accident_rate_pal <- colorQuantile(palette = "RdYlBu", probs = c(0, 0.25, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1), domain = NULL, na.color = "", reverse = T)
injury_pct_pal <- colorQuantile(palette = "RdYlBu", probs = c(0, 0.15, 0.3, 0.45, 0.6, 0.7, 0.8, 0.9, 1), domain = NULL, na.color = "#E5E5E5", reverse = T)
hit_and_run_pct_pal <- colorQuantile(palette = "RdYlBu", probs = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1), domain = NULL, na.color = "#E5E5E5", reverse = T)

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
  filter(FUNC_CLASS %in% c("R / MIN ART",  "U / MIN ART", "R / OTH PRIN ART", "U / MIN ART", "U OTH PRIN  ART"))

collector_roads <- accidents_by_road %>%
  filter(FUNC_CLASS %in% c("R / MAJ COL", "U / MAJ COL", "R / MIN COL"))

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
             0.5, 1, 0.75, 1.5, 1, 2, 1.5, 3, 2, 3,
             1, 2, 1.5, 3, 2, 3, 2.5, 3.75, 3, 4.5,
             2, 3, 2.5, 3.75, 3, 4.5, 3.5, 5.25, 4.5, 6.75,
             3, 4.5, 3.5, 5.25, 4, 6, 5, 7.5, 6, 9)) %>% 
  group_by(road, option, zoom) %>%
  summarize(weight = list(weight), .groups = "drop") %>%
  rrapply(how = "unmelt")

# Initialize leaflet map function
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
        icon = "fa-bar-chart",
        title = "Presentation",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange('presentationButton', Math.random());
          }")
      )
    ) %>%
    addEasyButton(
      easyButton(
        icon = "fa-table",
        title = "Table",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange('tableButton', Math.random());
          }")
      )
    ) %>%
    addEasyButton(
      easyButton(
        icon = "fa-info",
        title = "About",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange('aboutButton', Math.random());
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

# Update leaflet map function
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
                   fillOpacity = 1,
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
                   fillOpacity = 1,
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
                   fillOpacity = 1,
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
                   fillOpacity = 1,
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
                   fillOpacity = 1,
                   bringToFront = F))
}

# Update map view based on road segment location
update_map_view <- function(mymap, coordinates) {
  leafletProxy(mymap) %>%
    setView(lng = coordinates[1], lat = coordinates[2], zoom = 15)
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

# Prepare plot data
plot_data_by_year <- accidents_to_roads %>%
  group_by(Year = as.factor(year)) %>%
  summarize(`Total Accidents` = n(), 
            `Injury Accidents` = sum(injury, na.rm = T), 
            `Injury Percentage` = `Injury Accidents` / `Total Accidents`,
            `Hit-and-Runs` = sum(hit_and_run, na.rm = T), 
            `Hit-and-Run Percentage` = `Hit-and-Runs` / `Total Accidents`,
            `Pedestrian Collisions` = sum(pedestrian, na.rm = T))

# Create list for storing plots
plots <- list()

plots$AADT <- ggplot() + theme_void() + theme(axis.line=element_blank())

plots$accidents <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Total Accidents`, fill = Year, text = `Total Accidents`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$accident_rate <- ggplot() + theme_void() + theme(axis.line=element_blank())

plots$injury <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Injury Accidents`, fill = Year, text = `Injury Accidents`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$injury_pct <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Injury Percentage`, fill = Year, 
             text = percent(`Injury Percentage`, accuracy = 0.1))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$hit_and_run <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Hit-and-Runs`, fill = Year, text = `Hit-and-Runs`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$hit_and_run_pct <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Hit-and-Run Percentage`, fill = Year, 
             text = percent(`Hit-and-Run Percentage`, accuracy = 0.1))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$pedestrian <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Pedestrian Collisions`, fill = Year, text = `Pedestrian Collisions`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(text = element_text(size = 8), legend.position = "none") +  
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$total_accidents_by_year <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Total Accidents`, fill = Year, label = `Total Accidents`)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5,  size = 5) +
  theme_minimal() +
  theme(text = element_text(size = 18), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

plots$injury_pct_by_year <- plot_data_by_year %>%
  ggplot(aes(x = Year, y = `Injury Percentage`, fill = Year, label = percent(`Injury Percentage`, accuracy = 0.1))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5,  size = 5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(size = 18), legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 8, type = "continuous"))

check_zoom <- function(zoom) {
  case_when(
    zoom == 12 ~ "zoom_12",
    zoom == 13 ~ "zoom_13",
    zoom == 14 ~ "zoom_14",
    zoom == 15 ~ "zoom_15",
    TRUE ~ "zoom_16"
  )
}

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
    map_input == "Tree Collisions" ~ "tree",
    map_input == "Animal Collisions" ~ "animal",
    map_input == "Mailbox Collisions" ~ "mailbox",
  )
}

check_glide_index <- function(glide_index) {
  case_when(
    glide_index == 0 ~ "Number of Nashville Traffic Accidents Each Year",
    glide_index == 1 ~ "Percentage of Nashville Traffic Accidents Resulting in Injury Each Year",
    glide_index == 2 ~ "Root Causes of Nashville's Traffic Problem",
    glide_index == 3 ~ "Some Scary Statistics",
    glide_index == 4 ~ "Some More Scary Statistics",
    glide_index == 5 ~ "Using Data Analytics to Evaluate Trafffic Accident Risk for Nashville Roadways"
  )
}