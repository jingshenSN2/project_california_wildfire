
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Wrangling & Save --------------------------------------------------------

# Make subregion smaller

subregion <-
  cal_subregion %>%
  rename(subregion_name = name) %>%
  st_simplify(dTolerance = 1000)

road <-
  bind_rows(
    cal_highway %>%
      transmute(type = "highway"),
    cal_railway %>%
      transmute(type = "railway")) %>%
  group_by(type) %>% summarise() %>%
  st_simplify(dTolerance = 100) %>% 
  st_intersection(subregion)

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
  
  # Remove empty polygons
  
  filter(!st_is_empty(.)) %>%
  st_simplify(dTolerance = 100) %>%
  st_intersection(subregion)

subregion %>%
  st_write("output/calfire_app/data/subregion.geojson", delete_dsn = TRUE)

cal_outline %>%
  st_write("output/calfire_app/data/outline.geojson", delete_dsn = TRUE)

# Fire with subregion

fire_with_subregion <-
  fire_with_cause %>%
  
  # With largest=TRUE, this line can take 5+ min to run
  
  st_join(subregion,
          left = FALSE,
          largest=TRUE) %>%
  st_simplify(dTolerance = 100) %>%
  mutate(
    distance_to_road = 
      st_distance(.,
                  road %>%
                    st_union()) %>%
      units::set_units("km") %>%
      as.numeric() %>%
      round(1))

fire_with_subregion %>%
  as_tibble() %>%
  
  # Since we joined data with subregion, we can use it in our app
  # without geometry
  
  select(-geometry) %>%
  arrange(alarm_date) %>%
  write_csv("output/calfire_app/data/fire.csv")

# Save shapefiles

list(
  fire = fire_with_subregion,
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
