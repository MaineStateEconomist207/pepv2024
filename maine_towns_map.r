---
  title: "Interactive Town level map for latest PEP release"
author: "LMY"
date: "2025-05-02"
---
  
  "###############################################################################'
# Script Purpose:
\ 
\ 
\ 
###############################################################################'"

# Set up ----

## Install and load required packages then set working directory and Census API key
library(tidycensus)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(mapview)
library(viridisLite)
library(tigris)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(DT)
library(dplyr)
library(here)
library(leaflet.extras)

## Set working directory
here::here()

## Load and Prepare Population and Spatial Data ----
"Note that some degree of pre-processing and calculations have occurred before this step."

## Load population CSV file
population_data <- read.csv("Inputs/PEP_towns_2024.csv", header=TRUE, stringsAsFactors=FALSE)

# # IF CENSUS API IS NOT WORKING UNCOMMENT AND RUN THE FOLLOWING SECTION:
# ## Load towns shape file
# me_towns1 <- st_read("Inputs/tl_2024_23_cousub/tl_2024_23_cousub.shp")

# Pull shape file for towns from Census API
## Set Census API key
census_api_key("ce506007c506c320b3720abc4c712139c482bc4b")

## Pull shape file 
me_towns <- get_acs(
  geography = "county subdivision",
  state = "ME",
  variables = "B01002_001", # var. doesn't matter we just need geometry data
  survey = "acs5",
  geometry = TRUE
)

## Drop columns
me_towns <- me_towns %>% 
  select(-variable, -estimate, -moe)

# Merge population data with spatial data
me_towns <- merge(me_towns, population_data, by.x = "GEOID", by.y = "GEOID", all.x = TRUE)

# Transform datum
me_towns <- st_transform(me_towns, crs = 4326) # crs code for WGS 84, which is required for Leaflet

# Create county outlines by dissolving towns by county
## Needed as layer for map
me_counties <- me_towns %>% 
  group_by(CountyName) %>% 
  summarise(geometry = st_union(geometry))

# For tool tip label
# me_towns$annual_prc_chg <- as.numeric(me_towns$annual_prc_chg)
me_towns$Prc_chg_label <- me_towns$annual_prc_chg * 100


# Final Map ----

# Define our exact data range and categories
breaks <- c(-Inf, -50, -10, -1, 0, 1, 10, 50, 100, Inf)

# Create a manual color palette with exactly the colors we want
# manual_colors <- c(
#   "#00008B",  # darkblue for Loss >50
#   "#4169E1",  # royalblue for Loss 10-50
#   "#B0C4DE",  # lightsteelblue for Loss 1-9
#   "#FFFFFF",  # white for No change
#   "#FFCCCB",  # light red for Gain 1-9
#   "#FF6347",  # tomato for Gain 10-50
#   "#B22222",  # firebrick for Gain 51-100
#   "#8B0000"   # darkred for Gain >100
# )

manual_colors <- c(
  "#8B0000", # Dark Red for Loss > 50%
  "#FF6347", # Tomato for Loss 10-50%
  "#FFCCCB", # Light Red for Loss 1-9%
  "#FFFFFF", # White for No Change
  "#B0C4DE", # Light Steel Blue for Gain 1-9%
  "#4169E1", # royal Blue for additional Gain option
  "#2746B0", # steal Blue for Gain 10-50%
  "#00008B" # Dark Blue for Gain > 50%
  
)

# Define CUSTOM color function that maps values directly to our colors
color_pal <- function(x) {
  if (is.na(x)) return("#ffeda0")
  else if (x <= -50) return(manual_colors[1])
  else if (x <= -10) return(manual_colors[2])
  else if (x < 0) return(manual_colors[3])
  else if (x == 0) return(manual_colors[4])
  else if (x < 10) return(manual_colors[5])
  else if (x < 50) return(manual_colors[6])
  else if (x <= 100) return(manual_colors[7])
  else return(manual_colors[8])
}

# Create a leaflet-compatible color palette
leaflet_pal <- colorFactor(
  palette = manual_colors,
  domain = c("Loss >50", "Loss 10–50", "Loss 1–9", "No change", 
             "Gain 1–9", "Gain 10–50", "Gain 51–100", "Gain >100"),
  na.color = "#ffeda0"
)

# Apply custom function to the data for coloring polygons
me_towns$color <- sapply(me_towns$annual_chg, color_pal)


# Prepare label text for tool tips
me_towns$label <- paste0( 
  "<div style='font-size: 14px; line-height: 1.4;'>",
  "<strong>", me_towns$NAME, "</strong><br/><br/>",
  "<u>Annual Change (2023–2024)</u><br/>",
  "Numeric: <strong><span style='color:",
  ifelse(is.na(me_towns$annual_chg), "gray",
         ifelse(me_towns$annual_chg > 0, "green",
                ifelse(me_towns$annual_chg < 0, "red", "gray"))),
  "'>",
  ifelse(is.na(me_towns$annual_chg), "&ndash;",
         ifelse(me_towns$annual_chg > 0, "&#9650; ", # ▲
                ifelse(me_towns$annual_chg < 0, "&#9660; ", # ▼
                       "&ndash; "))),
  format(me_towns$annual_chg, big.mark = ",", scientific = FALSE),
  "</span></strong><br/>",
  "Percent: <strong>", formatC(round(me_towns$Prc_chg_label, 1), format = "f", big.mark = ",", digits = 2), "%</strong><br/><br/>",
  "Estimated Population (2024): <strong>", format(me_towns$POPESTIMATE, big.mark = ",", scientific = FALSE), "</strong><br/>",
  "Population Density: <strong>", formatC(round(me_towns$PopDen, 1), format = "f", big.mark = ",", digits = 1), "</strong> per sq mi<br/><br/>",
  "<span style='font-size: 12px'><i>Data source: U.S. Census Bureau, <br/> Population Estimates Program (Vintage 2024)</i></span>",
  "</div>"
)


# Create map
town_popChg_map <- leaflet(width = "100%", height = 800,
                           
                           # control zoom speed, title layer (i.e., base map), and initial view
                           options = leafletOptions(
                             zoomSnap = .05, # controls how much the zoom level "snaps" when zooming.
                             zoomDelta = .05, # controls the increment of zoom when using mouse wheel or zoom controls
                             wheelPxPerZoomLevel = 120 # Smooth, slow zoom on scroll. Low = fast, higher = slow
                           )) %>%
  
  
  # Add or remove base map tiles (uncomment to add base layer map)
  # addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Set map zoom extent centered on Maine, but slightly to left to fit legend (lat = 45.2538;lng = -69.4455)
  setView(lng = -69, lat = 45.2538, zoom = 7.8) %>% # Adjust zoom level as needed (7.2 base)
  
  # Add town polygons with population data
  addPolygons(
    data = me_towns,
    fillColor = ~color,
    fillOpacity = .9, #0.7 Used to change fill transparency
    color = "gray", # Use to change line color around towns.
    weight = 0.5, # Thin border line. Use to change line width around towns
    label = ~lapply(label, HTML),
    highlightOptions = highlightOptions(
      weight = 1.5,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  
  # Add county outlines
  addPolygons(
    data = me_counties,
    fill = FALSE,
    color = "#000000", # Border color
    weight = .9, # Thin border line. Use to change line width around border
    opacity = .5 # use to soften or sharpen line around border
  ) %>%
  setMapWidgetStyle(list(background = "white")) %>% 
  # Fix the legend (v.3)
  addLegend(
    position = "topright", # Other options: bottomright, bottomleft, topleft, etc.
    colors = c(manual_colors, "#ffeda0"),
    labels = c("Loss >50", "Loss 10–50", "Loss 1–9", "No change", 
               "Gain 1–9", "Gain 10–50", "Gain 51–100", "Gain >100", "NA"),
    title = paste0("Numeric Change,", "<br/>", "2023–2024"),
    opacity = 0.9
  )


# Display the map
town_popChg_map


# Save the map as an HTML file ----
saveWidget(town_popChg_map, # data file
           "Outputs/town-pop-map.html", 
           selfcontained = TRUE,
           title = "Maine Towns Population Map",
           background = "white")


