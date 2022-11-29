
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(lubridate)
library(tmap)
library(sf)
library(tidyverse)


# Read in data ------------------------------------------------------------

shapefile_paths <-
  list.files(
    "data",
    pattern = "geojson$")

# shapefiles <- 
#   shapefile_paths %>%
#   purrr::map(
#     ~ st_read(
#       paste0("data/", .x))) %>%
#   set_names(
#     str_remove(shapefile_paths, ".geojson"))

rasters_paths <-
  list.files(
    "data",
    pattern = "tif$")

rasters <- 
  rasters_paths %>%
  purrr::map(
    ~ terra::rast(
      paste0("data/", .x))) %>%
  set_names(
    str_remove(rasters_paths, ".tif"))

rm(shapefile_paths, rasters_paths)


# UI function -------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "California Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("clipboard")),
      menuItem(
        "Tables",
        tabName = "tables",
        icon = icon("list"),
        menuItem("Wildfire List", tabName = "fire_table", icon = icon("fire")),
        menuItem("Wildfire Summary", tabName = "fire_summary", icon = icon("table"))
      ),
      menuItem(
        "Charts",
        tabName = "charts",
        icon = icon("chart-bar"),
        menuItem("Road Distance", tabName = "road", icon = icon("road")),
        menuItem("Population", tabName = "pop", icon = icon("users"))
      ),
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("map"),
        menuItem("Vegetation v.s. fire", tabName = "veg", icon = icon("tree")),
        menuItem("Building v.s. fire", tabName = "build", icon = icon("city"))
      )
    ),
    dateRangeInput(
      "date",
      "Alarm Date Range",
      start = "2000-01-01",
      end = "2020-12-31",
      min = "1980-01-01",
      max = "2023-01-01"
    ),
    selectInput(
      "subregion",
      "Subregion",
      c("All" = 0,
        "North Coast and Montane" = 1,
        "North Interior" = 2,
        "North Sierran" = 3,
        "South Sierran" = 4,
        "Central Valley" = 5,
        "Central Coast and Montane" = 6,
        "South Coast and Montane" = 7,
        "South Interior" = 8,
        "Great Basin" = 9)
    ),
    checkboxGroupInput(
      "cause",
      "Cause of fire",
      c("Human" = "Human",
        "Natural" = "Natural",
        "Structure" = "Structure",
        "Vehicle" = "Vehicle",
        "Other" = "other")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introduction")
      ),
      tabItem(
        tabName = "fire_table",
        h2("Introduction")
      ),
      tabItem(
        tabName = "fire_summary",
        h2("Introduction")
      ),
      tabItem(
        tabName = "intro",
        h2("Introduction")
      ),
    )
  )
)


# Server function ---------------------------------------------------------

server <- function(input, output) {
  
}


# Shiny Entry -------------------------------------------------------------

shinyApp(ui, server)
