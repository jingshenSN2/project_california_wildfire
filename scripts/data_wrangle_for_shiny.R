
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Wrangling & Save --------------------------------------------------------

road <-
  bind_rows(
    cal_highway %>%
      transmute(type = "highway"),
    cal_railway %>%
      transmute(type = "railway")) %>%
  group_by(type) %>%
  summarise()

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
                population_density)) %>%
  
  # Remove empty polygons (Why do they exist?)
  
  filter(!st_is_empty(.))

# Make subregion smaller

subregion <-
  cal_subregion %>%
  st_simplify(dTolerance = 1000)

subregion %>%
  st_write("output/calfire_app/data/subregion.geojson", delete_dsn = TRUE)

cal_outline %>%
  st_write("output/calfire_app/data/outline.geojson", delete_dsn = TRUE)

# Fire with subregion

fire_with_subregion <-
  fire_with_cause %>%
  st_join(subregion, largest=TRUE)

fire_with_subregion %>%
  as_tibble() %>%
  
  # Since we joined data with subregion, we can use it in our app
  # without geometry
  
  select(-geometry) %>%
  arrange(alarm_date) %>%
  write_csv("output/calfire_app/data/fire.csv")

# Only fire larger than 5000 acres

large_fire <-
  fire_with_subregion %>%
  filter(gis_acres >= 5000)

# Save shapefiles

list(
  large_fire = large_fire,
  road = road,
  pop = pop) %>%
  iwalk(
    ~ st_write(.x,
               paste0("output/calfire_app/data/", .y, ".geojson"),
               delete_dsn = TRUE))

# Save rasters

rasters %>%
  set_names("build", "veg", "fire", "pop") %>%
  iwalk(
    ~ terra::writeRaster(.x,
                         paste0("output/calfire_app/data/", .y, ".tif"),
                         overwrite = TRUE))
