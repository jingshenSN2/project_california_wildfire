
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(stars)
library(tmap)
library(ggplot2)


# read in data 

shapefile_paths <-
  list.files(
    'data/processed',
    pattern = 'geojson$')

st_read('data/processed/cal_counties.geojson')

shapefile_paths %>% 
  file.path('data/processed', .) %>% 
  purrr::map(
    ~ sf::st_read(.x) %>% 
      sf::st_transform(crs = 4326)) %>% 
  set_names(
    str_remove(shapefile_paths, '.geojson')) %>% 
  list2env(.GlobalEnv)



