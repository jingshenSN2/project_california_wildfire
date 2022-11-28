
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(units)


# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")

tmap_mode("view")

# Plot of fire polygons ---------------------------------------------------

year <- 2020

area_range <-
  list(min = 100, max = Inf)

# Add vegetation layer

rasters$calveg %>%
  tm_shape(name = "vegetation") +
  tm_raster(title = "Vegetation",
            style = "cont",
            palette = "Greens",
            alpha = 0.6) +
  
  # Add building layer
  
  rasters$cal_building %>%
  tm_shape(name = "building") +
  tm_raster(title = "Building Density",
            style = "cont",
            palette = "Blues",
            alpha = 0.6) +
  
  # Add fire polygons
  
  fire_with_cause %>%
  filter(
    year(alarm_date) == year,
    gis_acres >= area_range$min,
    gis_acres <= area_range$max) %>%
  tm_shape(name = "fire") +
  tm_polygons(col = "#e85437",
              alpha = 0.6) +
  
  tm_layout(
    legend.position = c("right", "top"))
