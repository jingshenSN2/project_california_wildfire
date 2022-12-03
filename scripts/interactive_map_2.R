
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(units)


# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")

tmap_mode("view")

urban <- 
  st_read('data/raw/shapefiles/BND_Adjusted_Urban_Area.shp') %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  select(OBJECTID, NAME10)


# fire trend and population -----------------------------------------------

area_range <-
  list(min = 100, max = Inf)

five_years <- 
  list(
    first_10_15 = c(2011: 2015),
    second_15_20 = c(2016: 2020))

# create combined roads data

road <-
  bind_rows(
    cal_highway %>%
      transmute(type = "highway"),
    cal_railway %>%
      transmute(type = "railway"))


fire_with_cause %>%
  filter(
    year(alarm_date) %in% five_years$first_10_15,
    gis_acres >= area_range$min,
    gis_acres <= area_range$max) %>%
  tm_shape(name = "Fire from 2011 to 2015") +
  tm_polygons(col = "#fddb86",
              alpha = 0.6) + 
  
  fire_with_cause %>%
  filter(
    year(alarm_date) %in% five_years$second_15_20,
    gis_acres >= area_range$min,
    gis_acres <= area_range$max) %>%
  tm_shape(name = "Fire from 2016 to 2020") +
  tm_polygons(col = "#e85437",
              alpha = 0.6) +
  
  # population density 
  
  rasters$pop$pop_density_2015 %>%
  tm_shape(name = "Population density 2011 to 2015") +
  tm_raster(col = "pop_density_2015",
            title = "Population density",
            breaks = c(
              0,
              1000,
              2000,
              3000,
              5000,
              10000,
              100000),
            alpha = 0.6) +
  
  rasters$pop$pop_density_2020 %>%
  tm_shape(name = "Population density 2016 to 2020") +
  tm_raster(col = "pop_density_2020",
            title = "Population density",
            breaks = c(
              0,
              1000,
              2000,
              3000,
              5000,
              10000,
              100000),
            alpha = 0.6) + 
  
  road %>% 
  tm_shape(name = "Highway and railways") +
  tm_lines(col = "type",
           title.col = "Road type") +
  
  tm_shape(urban,
           name = 'Urban areas') +
  tm_polygons(alpha = 0.6,
              palette = '#B1D4E0')




