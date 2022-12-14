
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(stars)
library(tmap)
library(ggplot2)


# Read in data  -----------------------------------------------------------

shapefile_paths <-
  list.files(
    "data/processed",
    pattern = "geojson$")

shapefile_paths %>% 
  file.path("data/processed", .) %>% 
  purrr::map(sf::st_read) %>% 
  set_names(
    str_remove(shapefile_paths, ".geojson")) %>% 
  list2env(.GlobalEnv)


raster_paths <-
  list.files(
    "data/processed",
    pattern = "tif$")

rasters <-
  raster_paths %>% 
  file.path("data/processed", .) %>% 
  purrr::map(terra::rast) %>% 
  set_names(
    str_remove(raster_paths, ".tif"))

fire_cause <- read_rds("data/processed/fire_cause.rds")

rm(shapefile_paths, raster_paths)


# Preprocess --------------------------------------------------------------

fire_with_cause <-
  cal_fire_all %>%
  filter(year(alarm_date) >= 1980 & year(alarm_date) <= 2023) %>%
  mutate(
    
    # Convert cause number to name
    
    cause_name = .$cause %>%
      
      # Use `unknown` as default value
      
      replace_na(14) %>%
      
      # Get name
      
      map_chr(~ fire_cause$cause_name[[.x]]),
    
    # Classify the cause
    
    cause_category = case_when(
      cause %in% fire_cause$category$human ~ "Human",
      cause %in% fire_cause$category$natural ~ "Natural",
      cause %in% fire_cause$category$vehicle ~ "Vehicle",
      cause %in% fire_cause$category$structure ~ "Structure",
      TRUE ~ "Other")) %>%
  select(
    -c(cause, objective))

rasters$fire <-
  fire_with_cause %>%
  terra::rasterize(rasters$calveg,
                   fun = length,
                   sum = TRUE)
rasters$pop <-
  c(
    terra::rasterize(cal_population_tract_2015 %>%
                       filter(!st_is_empty(.)),
                     rasters$calveg,
                     "population_density"),
    terra::rasterize(cal_population_tract_2020 %>%
                       filter(!st_is_empty(.)),
                     rasters$calveg,
                     "population_density"),
    terra::rasterize(cal_population_tract_2010 %>%
                       filter(!st_is_empty(.)),
                     rasters$calveg,
                     "population_density"))

terra::set.names(rasters$pop,
                 c("pop_density_2015", "pop_density_2020", "pop_density_2010"))

cal_outline <-
  cal_counties %>%
  st_union()
