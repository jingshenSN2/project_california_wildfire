
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(stars)
library(tmap)


# Read in data ------------------------------------------------------------

fire <- st_read("data/processed/cal_fire_all.geojson")
fire_cause <- read_rds("data/processed/fire_cause.rds")
county <- st_read("data/processed/cal_counties.geojson")
pop <- st_read("data/processed/cal_population_tract_2020.geojson")
building <- terra::rast("data/processed/cal_building.tif")
veg <- terra::rast("data/processed/calveg.tif")

# Preprocess --------------------------------------------------------------

fire_with_cause <-
  fire %>%
  filter(year(alarm_date) >= 1980 & year(alarm_date) <= 2023) %>%
  mutate(
    cause_category = case_when(
      cause %in% fire_cause$category$human ~ "Human",
      cause %in% fire_cause$category$natural ~ "Natural",
      cause %in% fire_cause$category$vehicle ~ "Vehicle",
      cause %in% fire_cause$category$structure ~ "Structure",
      TRUE ~ "other"))

fire_r <-
  fire_with_cause %>%
  terra::rasterize(veg,
                   fun = length,
                   sum = TRUE)

cal_outline <-
  county %>%
  st_union()

# Plot fire in counties ---------------------------------------------------

fire_by_county <-
  fire_with_cause %>%
  
  # Spatial join with county
  
  st_join(county %>%
            select(geoid)) %>%
  
  # Group by geoid
  
  as_tibble() %>%
  group_by(geoid) %>%
  
  # Count and area
  
  summarise(
    n = n(),
    area = sum(gis_acres)) %>%
  
  left_join(county, ., by = 'geoid') %>%
  
  replace_na(
    list(n = 0, area = 0))

m1 <-
  tm_shape(fire_by_county) +
  tm_polygons(title = "Number of wildfires",
              col = "n",
              style = "cont")

tmap_save(m1, "output/static_maps/fire_count.png")

m2 <-
  tm_shape(fire_by_county) +
  tm_polygons(title = "Total burned area [acres]",
              col = "area",
              style = "cont")

tmap_save(m2, "output/static_maps/fire_area.png")

m_pop <-
  tm_shape(pop) +
  tm_fill(title = "Population",
          col = "estimate",
          style = "cont")

tmap_save(m_pop, "output/static_maps/population.png")


# Some rasters ------------------------------------------------------------

m3 <-
  fire_r %>%
  terra::mask(county) %>%
  tm_shape() +
  tm_raster(title = "Number of wildfires",
            style = "cont") +
  tm_layout(
    legend.position = c("right", "top")) +
  
  tm_shape(cal_outline) +
  tm_borders(lwd = 1)

tmap_save(m3, "output/static_maps/fire_count_raster.png")


m4 <-
  veg %>%
  terra::mask(county) %>%
  tm_shape() +
  tm_raster(title = "Vegetation level",
            style = "cont",
            palette = "Greens") +
  tm_layout(
    legend.position = c("right", "top")) +
  
  tm_shape(cal_outline) +
  tm_borders(lwd = 1)

tmap_save(m4, "output/static_maps/veg_raster.png")


m5 <-
  building %>%
  terra::mask(county) %>%
  tm_shape() +
  tm_raster(title = "Building density",
            style = "cont",
            palette = "Greys") +
  tm_layout(
    legend.position = c("right", "top")) +
  
  tm_shape(cal_outline) +
  tm_borders(lwd = 1)

tmap_save(m5, "output/static_maps/building_raster.png")
