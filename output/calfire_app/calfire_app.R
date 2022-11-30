
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(lubridate)
library(tmap)
library(sf)
library(tidyverse)


# Read in data ------------------------------------------------------------

fire_dfr <-
  read_csv("data/fire.csv") %>%
  mutate(
    cause_category = factor(cause_category,
                            levels = c("Human",
                                       "Natural",
                                       "Structure",
                                       "Vehicle",
                                       "Other")))


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


# Load UI -----------------------------------------------------------------

source("ui.R")


# Plot outside server -----------------------------------------------------

subregion_map <-
  shapefiles$subregion %>%
  tm_shape(name = "Subregion") +
  tm_polygons(title = "Subregion Name",
              col = "subregion_name")

# Server function ---------------------------------------------------------

server <- function(input, output) {
  
  # Subregion reactive
  
  subregion <-
    reactive({
      shapefiles$subregion %>%
        filter(subregion_id == input$subregion | input$subregion == 0)
    })
  
  # Fire reactive
  
  fire_filtered <-
    reactive({
      shapefiles$fire %>%
        filter(
          between(
            as_date(alarm_date),
            input$date[1],
            input$date[2]),
          cause_category %in% input$causes,
          (input$subregion == 0 |
             subregion_id == input$subregion))
    })
  
  # Large wildfire reactive
  
  large_fire <-
    reactive({
      fire_filtered() %>%
        filter(gis_acres >= 5000)
    })
  
  # Fire dataframe reactive
  
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
  
  # Summary reactive
  
  fire_summary <-
    reactive({
      if (input$summary_by == "year") {
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
  
  # Fire raster reactive
  
  fire_raster <-
    reactive({
      fire_filtered() %>%
        filter(!st_is_empty(.)) %>%
        terra::rasterize(rasters$veg,
                         fun = length,
                         sum = TRUE) %>%
        terra::aggregate(2^input$agg, fun = "sum", na.rm=TRUE) %>%
        replace(. == 0, NA) %>%
        terra::mask(subregion())
    })
  
  fire_map <-
    reactive({
      fire_raster() %>%
        tm_shape(name = "Fire") +
        tm_raster(title = "Number of wildfires",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Reds")
    })
  
  # Veg raster reactive
  
  veg_raster <-
    reactive({
      rasters$veg %>%
        terra::aggregate(2^input$agg, na.rm=TRUE) %>%
        replace(. == 0, NA) %>%
        terra::mask(subregion())
    })
  
  veg_map <-
    reactive({
      veg_raster() %>%
        tm_shape(name = "Vegetation") +
        tm_raster(title = "Vegetation Level",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Greens")
    })
  
  # Road reactive
  
  road <-
    reactive({
      shapefiles$road %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion)
    })
  
  # Population raster reactive
  
  pop_raster <-
    reactive({
      rasters$pop$pop_density_2020 %>%
        terra::aggregate(2^input$agg, na.rm=TRUE) %>%
        replace(. == 0, NA) %>%
        terra::mask(subregion())
    })
  
  pop_map <-
    reactive({
      pop_raster() %>%
        tm_shape(name = "Population") +
        tm_raster(title = "Population Density",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Blues")
    })
  
  # Building raster reactive
  
  build_raster <-
    reactive({
      rasters$build %>%
        terra::aggregate(2^input$agg, na.rm=TRUE) %>%
        replace(. == 0, NA) %>%
        terra::mask(subregion())
    })
  
  build_map <-
    reactive({
      build_raster() %>%
        tm_shape(name = "Building") +
        tm_raster(title = "Building Density",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Blues")
    })
  
  # Output part
  
  output$subregion <-
    renderTmap({
      subregion_map
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
                    palette = c("Human" = "#fb8072",
                                "Natural" = "#a6d854",
                                "Structure" = "#6e549d",
                                "Vehicle" = "#80b1d3",
                                "Other" = "#aaaaaa"),
                    alpha = 0.8,
                    popup.vars = c("Alarm Date" = "alarm_date",
                                   "Containment Date" = "cont_date",
                                   "Burned Area [acres]" = "gis_acres",
                                   "Cause Category" = "cause_category",
                                   "Cause" = "cause_name"))
    })
  
  output$fire_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        group_by(Year = year(alarm_date),
                 cause_category) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Year, y = Count)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause_category,
          scales = "free_y")
    })
  
  output$veg_map <-
    renderTmap({
      veg_map() +
        fire_map()
    })
  
  output$veg_plot <-
    renderPlot({
      tibble(
        fire = fire_raster() %>% 
          terra::values() %>%
          replace_na(0),
        veg = veg_raster() %>% 
          terra::values() %>%
          replace_na(0)) %>%
        filter(!(fire == 0 & veg == 0)) %>%
        ggplot(aes(x = veg, y = fire)) +
        geom_bin2d() +
        scale_fill_continuous(type = "viridis") +
        labs(x = "Vegetation",
             y = "Fire")
    })
  
  output$road <-
    renderTmap({
      fire_map() +
        road() %>%
        tm_shape(name = "Road") +
        tm_lines(title.col = "Road Type",
                 col = "type",
                 alpha = 0.8,
                 palette = c("highway" = "#377eb8",
                             "railway" = "#984ea3"))
    })
  
  output$road_distance <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = distance_to_road)) +
        geom_histogram(color = "black", fill = "white", bins = 30) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        labs(x = "Road distance [km]",
             y = "Number of fires")
    })
  
  output$pop_map <-
    renderTmap({
      pop_map() +
        fire_map()
    })
  
  output$pop_plot <-
    renderPlot({
      tibble(
        fire = fire_raster() %>% 
          terra::values() %>%
          replace_na(0),
        pop = pop_raster() %>% 
          terra::values() %>%
          replace_na(0)) %>%
        filter(!(fire == 0 & pop == 0)) %>%
        ggplot(aes(x = pop, y = fire)) +
        geom_bin2d() +
        scale_fill_continuous(type = "viridis") +
        labs(x = "Population Density",
             y = "Fire")
    })
  
  output$build_map <-
    renderTmap({
      build_map() +
        fire_map()
    })
  
  output$build_plot <-
    renderPlot({
      tibble(
        fire = fire_raster() %>% 
          terra::values() %>%
          replace_na(0),
        build = build_raster() %>% 
          terra::values() %>%
          replace_na(0)) %>%
        filter(!(fire == 0 & build == 0)) %>%
        ggplot(aes(x = build, y = fire)) +
        geom_bin2d() +
        scale_fill_continuous(type = "viridis") +
        labs(x = "Building Density",
             y = "Fire")
    })
  
}


# Shiny Entry -------------------------------------------------------------

shinyApp(ui, server)
