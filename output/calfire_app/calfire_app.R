
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(lubridate)
library(tmap)
library(sf)
library(units)
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
              col = "subregion_name",
              popup.vars = c("Subregion Name" = "subregion_name"))

cause_palette <-
  c("Human" = "#fb8072",
    "Natural" = "#a6d854",
    "Structure" = "#6e549d",
    "Vehicle" = "#80b1d3",
    "Other" = "#aaaaaa")

mytheme <-
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16,
                                face = "bold"),
    axis.title.y = element_text(size = 16,
                                face = "bold"),
    strip.text = element_text(size = 16,
                              face = "bold"),
    panel.grid = element_blank()
  )

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
          cause_category %in% input$causes)
    })
  
  # Large wildfire reactive
  
  large_fire_map <-
    reactive({
      fire_filtered() %>%
        filter(gis_acres >= 5000,
               (input$subregion == 0 |
                  subregion_id == input$subregion)) %>%
        tm_shape(name = "Fire Perimeter") +
        tm_polygons(title = "Cause",
                    col = "cause_category",
                    palette = cause_palette,
                    alpha = 0.8,
                    popup.vars = c("Alarm Date" = "alarm_date",
                                   "Containment Date" = "cont_date",
                                   "Burned Area [acres]" = "gis_acres",
                                   "Cause Category" = "cause_category",
                                   "Cause" = "cause_name"))
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
        terra::rasterize(rasters$veg,
                         fun = length,
                         sum = TRUE) %>%
        terra::aggregate(2^input$agg, fun = "sum", na.rm=TRUE) %>%
        replace(. == 0, NA)
    })
  
  fire_map <-
    reactive({
      fire_raster() %>%
        terra::mask(subregion()) %>%
        tm_shape(name = "Fire") +
        tm_raster(title = "Number of fires",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Reds")
    })
  
  # Veg raster reactive
  
  veg_raster <-
    reactive({
      rasters$veg %>%
        terra::aggregate(2^input$agg, na.rm=TRUE) %>%
        replace(. == 0, NA)
    })
  
  veg_map <-
    reactive({
      veg_raster() %>%
        terra::mask(subregion()) %>%
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
  
  # Urban reactive
  
  urban <-
    reactive({
      shapefiles$urban %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion)
    })
  
  # Population raster reactive
  
  pop_raster <-
    reactive({
      rasters$pop$pop_density_2020 %>%
        terra::aggregate(2^input$agg, na.rm=TRUE) %>%
        replace(. == 0, NA)
    })
  
  pop_map <-
    reactive({
      pop_raster() %>%
        terra::mask(subregion()) %>%
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
        replace(. == 0, NA)
    })
  
  build_map <-
    reactive({
      build_raster() %>%
        terra::mask(subregion()) %>%
        tm_shape(name = "Building") +
        tm_raster(title = "Building Density",
                  alpha = 0.8,
                  style = "cont",
                  palette = "Blues")
    })
  
  # Subregion statistics reactive
  
  subregion_stat <-
    reactive({
      shapefiles$subregion %>%
        as_tibble() %>%
        left_join(
          fire_dfr_filtered() %>%
            group_by(
              subregion_id = as.character(subregion_id),
              cause = cause_category) %>%
            summarise(fire = n()),
          by = "subregion_id") %>%
        mutate(fire = fire / subregion_area)
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
        large_fire_map()
    })
  
  output$fire_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        group_by(Year = year(alarm_date),
                 cause_category) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Year, y = Count, col = cause_category)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        mytheme
    })
  
  output$veg_map <-
    renderTmap({
      veg_map() +
        fire_map()
    })
  
  output$veg_summary <-
    renderPlot({
      subregion_stat() %>%
        ggplot(aes(x = veg_level, y = fire, col = cause)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Vegetation",
             y = expression(bold("Fires per km"^2))) +
        mytheme
    })
  
  output$veg_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = veg_level, col = cause_category)) +
        geom_histogram(fill = "white", bins = 50) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Local Vegetation Level",
             y = "Number of fires") +
        mytheme
    })
  
  output$road_map <-
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
  
  output$road_summary <-
    renderPlot({
      subregion_stat() %>%
        ggplot(aes(x = road_density, y = fire, col = cause)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = expression(bold(Road~Density*~group('[',`km`/`km`^2,']'))),
             y = expression(bold("Fires per km"^2))) +
        mytheme
    })
  
  output$road_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = road_distance, col = cause_category)) +
        geom_histogram(fill = "white", bins = 50) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Road distance [km]",
             y = "Number of fires") +
        mytheme
    })
  
  output$urban_map <-
    renderTmap({
      fire_map() +
        urban() %>%
        tm_shape(name = "Urban Area") +
        tm_polygons(alpha = 0.8,
                 col = "#b595c4")
    })
  
  output$urban_summary <-
    renderPlot({
      subregion_stat() %>%
        ggplot(aes(x = urban_density, y = fire, col = cause)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Proportion of Urban Area",
             y = expression(bold("Fires per km"^2))) +
        mytheme
    })
  
  output$urban_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = urban_distance, col = cause_category)) +
        geom_histogram(fill = "white", bins = 50) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Urban distance [km]",
             y = "Number of fires") +
        mytheme
    })
  
  output$pop_map <-
    renderTmap({
      pop_map() +
        fire_map()
    })
  
  output$pop_summary <-
    renderPlot({
      subregion_stat() %>%
        ggplot(aes(x = pop_density, y = fire, col = cause)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Population Density",
             y = expression(bold("Fires per km"^2))) +
        mytheme
    })
  
  output$pop_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = pop_density, col = cause_category)) +
        geom_histogram(fill = "white", bins = 50) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Local Population Density",
             y = "Number of fires") +
        mytheme
    })
  
  output$build_map <-
    renderTmap({
      build_map() +
        fire_map()
    })
  
  output$build_summary <-
    renderPlot({
      subregion_stat() %>%
        ggplot(aes(x = build_density, y = fire, col = cause)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(
          ~ cause,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Building Density",
             y = expression(bold("Fires per km"^2))) +
        mytheme
    })
  
  output$build_plot <-
    renderPlot({
      fire_dfr_filtered() %>%
        filter(input$subregion == 0 |
                 subregion_id == input$subregion) %>%
        ggplot(aes(x = build_density, col = cause_category)) +
        geom_histogram(fill = "white", bins = 50) +
        facet_wrap(
          ~ cause_category,
          scales = "free_y") +
        scale_color_manual(values = cause_palette, guide = "none") +
        labs(x = "Local Building Density",
             y = "Number of fires") +
        mytheme
    })
  
}


# Shiny Entry -------------------------------------------------------------

shinyApp(ui, server)
