# Evaluating Traffic Accident Risk for Nashville Roadways

## Description

This Shiny app aims to identify Nashville roadways that have an increased risk for traffic accidents to help prioritize road infrastructure projects for improving traffic safety.

## Background

Nashville’s rapidly growing population over the past decade has translated to higher traffic volumes on Davidson County roadways and an increasing number of traffic accidents, injuries, and deaths each year. Understanding traffic accident patterns through data analytics and visualization provides the ability to identify and implement traffic safety improvement strategies, including but not limited to improved roadway design and traffic policies. For this app, 234,640 traffic accidents reported to the Metro Nashville Police Department between 2014 - 2021 were mapped to 9,289 Davidson County road segments in order to identify high-risk roadways. 

### A few questions this app aims to address:

- Which roads have the highest traffic accident rates?
- Which roads have the highest percentage of traffic accidents resulting in injury?
- Which roads have the highest percentage of hit-and-runs?
- Which roads have the highest number of pedestrian collisions?

## How to Use This App

The app is currently hosted on shinyapps.io: https://rohitvenkat.shinyapps.io/nashville-traffic/

In the upper-right corner, there is a drop-down menu to select from the following map inputs for visualization:
- Annual Average Daily Traffic (AADT)
- Accident Rate (per MVMT)
- Total Accidents
- Injury Accidents
- Injury Percentage
- Hit-and-Runs
- Hit-and-Run Percentage
- Pedestrian Collisions

In the top-left corner, there are buttons to zoom in and out, reset the map view, display the visualized data within a data table, and view more info about Nashville traffic statistics and the app.

In the bottom-left corner, there are map layer controls to toggle between a light or dark map theme and to turn off or on different road functional class layers (Interstates / Freeways, Arterial Roads, Collector Roads, Local Roads).

Hovering over a road segment brings up a text box with the following information:
- Road Name
- AADT (if available)
- Accident Rate (if available)
- Total Accidents
- Injury Accidents
- Hit-and-Run Accidents
- Pedestrian Collisions

## Data Sources

- Metro Nashville Police Department traffic accident data (2014 - 2021) was obtained from the [Nashville Open Data Portal](https://data.nashville.gov/Police/Traffic-Accidents/6v6w-hpcw). 
- Road centerlines and traffic volume data (2020) were obtained by request from the [Tennessee Department of Transportation](https://www.tn.gov/tdot/long-range-planning-home/longrange-road-inventory/longrange-road-inventory-trims-data-request.html).