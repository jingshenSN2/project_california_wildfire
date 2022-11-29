
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(lubridate)
library(tmap)
library(sf)
library(tidyverse)


# Read in data ------------------------------------------------------------

fire_dfr <-
  read_csv("data/fire.csv")

shapefile_paths <-
  list.files(
    "data",
    pattern = "geojson$")

shapefiles <-
  shapefile_paths %>%
  purrr::map(
    ~ st_read(
      paste0("data/", .x))) %>%
  set_names(
    str_remove(shapefile_paths, ".geojson"))

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
      menuItem("Wildfire Summary", tabName = "fire_summary", icon = icon("table")),
      menuItem("Large Wildfires", tabName = "large_fire", icon = icon("fire")),
      menuItem("Vegetation", tabName = "veg", icon = icon("tree")),
      menuItem("Road", tabName = "road", icon = icon("road")),
      menuItem("Population", tabName = "pop", icon = icon("users")),
      menuItem("Building", tabName = "build", icon = icon("city"))
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
      "causes",
      "Cause of fire",
      c("Human", "Natural", "Structure", "Vehicle", "Other"),
      c("Human", "Natural")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introduction")
      ),
      tabItem(
        tabName = "fire_summary",
        h2("Wildfire Summary Table"),
        p("Here is a statistical table of wildfire burned area grouped 
          by alarm date year. The acreage is in acres."),
        radioButtons(
          "summary_by",
          "Summary By",
          c("Year", "Subregion")
        ),
        dataTableOutput("summary")
      ),
      tabItem(
        tabName = "large_fire",
        h2("Large Wildfire on Map"),
        p("Here are the perimeters of wildfires that burned an area greater than 5,000 acres. 
          When the fire perimeter overlaps with more than one subregion, 
          it will be classified into the subregion with the largest overlap area.
          Map may take some time to display."),
        tmapOutput("large_fire")
      ),
      tabItem(
        tabName = "veg",
        h2("Vegetation")
      ),
      tabItem(
        tabName = "road",
        h2("Road")
      ),
      tabItem(
        tabName = "pop",
        h2("Population")
      ),
      tabItem(
        tabName = "build",
        h2("Building")
      )
    )
  )
)


# Server function ---------------------------------------------------------

server <- function(input, output) {
  
  subregion <-
    reactive({
      shapefiles$subregion %>%
        filter(subregion_id == input$subregion | input$subregion == 0)
    })
  
  large_fire <-
    reactive({
      shapefiles$large_fire %>%
        filter(
          between(
            as_date(alarm_date),
            input$date[1],
            input$date[2]),
          cause_category %in% input$causes,
          (input$subregion == 0 |
             subregion_id == input$subregion))
    })
  
  fire_dfr_filtered <-
    reactive({
      fire_dfr %>%
        filter(
          between(
            as_date(alarm_date),
            input$date[1],
            input$date[2]),
          cause_category %in% input$causes)
    })
  
  fire_summary <-
    reactive({
      if (input$summary_by == "Year") {
        grouped <-
          fire_dfr_filtered() %>%
          filter(input$subregion == 0 |
                   subregion_id == input$subregion) %>%
          group_by(Year = year(alarm_date))
      } else {
        grouped <-
          fire_dfr_filtered() %>%
          group_by(Subregion = subregion_name)
      }
      grouped %>%
        summarise(`Count` = n(),
                  `Min Area` = round(min(gis_acres), 1),
                  `Avg Area` = round(mean(gis_acres), 1),
                  `Max Area` = round(max(gis_acres), 1),
                  `Total Area` = round(sum(gis_acres), 1))
    })
  
  output$summary <-
    renderDataTable({
      fire_summary()
    })
  
  output$large_fire <-
    renderTmap({
      subregion() %>%
        tm_shape(name = "Region Boundary") +
        tm_borders(lty = "longdash") +
        large_fire() %>%
        tm_shape(name = "Fire Perimeter") +
        tm_polygons(title = "Cause",
                    col = "cause_category",
                    popup.vars = c("Alarm Date" = "alarm_date",
                                   "Containment Date" = "cont_date",
                                   "Burned Area [acres]" = "gis_acres",
                                   "Cause Category" = "cause_category",
                                   "Cause" = "cause_name"))
    })
}


# Shiny Entry -------------------------------------------------------------

shinyApp(ui, server)
