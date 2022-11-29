
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(units)


# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")

tmap_mode("view")


# fire trend and population -----------------------------------------------

area_range <-
  list(min = 100, max = Inf)

five_years <- 
  list(
    first_10_15 = c(2011: 2015),
    second_15_20 = c(2016: 2020))

pop <-
  bind_rows(
    cal_population_tract_2015 %>%
      transmute(year = 2015,
                estimate,
                land_area,
                population_density),
    cal_population_tract_2020 %>%
      transmute(year = 2020,
                estimate,
                land_area,
                population_density))

fire_with_cause %>%
  filter(
    year(alarm_date) %in% five_years$first_10_15,
    gis_acres >= area_range$min,
    gis_acres <= area_range$max) %>%
  tm_shape(name = "fire from 2011 to 2015") +
  tm_polygons(col = "#e85437",
              alpha = 0.6) + 
  
  fire_with_cause %>%
  filter(
    year(alarm_date) %in% five_years$second_15_20,
    gis_acres >= area_range$min,
    gis_acres <= area_range$max) %>%
  tm_shape(name = "fire from 2016 to 2020") +
  tm_polygons(col = "#e85437",
              alpha = 0.6) +
  
  # population density 
  
  pop %>% 
  filter(year == 2015) %>% 
  tm_shape(name = 'population density 2011 to 2015') +
  tm_polygons(col = 'population_density',
              title = 'Population density',
              breaks = c(
                0,
                1000,
                2000,
                3000,
                5000,
                10000,
                100000),
              border.alpha = 0,
              alpha = 0.6) 
  
  pop %>% 
  filter(year == 2020) %>% 
  tm_shape(name = 'population density 2016 to 2020') +
  tm_polygons(col = 'population_density',
              title = 'Population density',
              breaks = c(
                0,
                1000,
                2000,
                3000,
                5000,
                10000,
                100000),
              border.alpha = 0,
              alpha = 0.6) 
  










