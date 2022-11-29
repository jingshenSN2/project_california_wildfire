
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Wrangling & Save --------------------------------------------------------

road <-
  bind_rows(
    cal_highway %>%
      transmute(type = "highway"),
    cal_highway %>%
      transmute(type = "railway"))

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

# Make subregion smaller

cal_counties %>%
  select(geometry) %>%
  st_join(cal_subregion) %>%
  group_by(subregion_id, name) %>%
  summarise() %>%
  st_write("output/calfire_app/data/subregion.geojson", delete_dsn = TRUE)

cal_outline %>%
  st_write("output/calfire_app/data/outline.geojson", delete_dsn = TRUE)

list(
  fire = fire_with_cause,
  road = road,
  pop = pop) %>%
  iwalk(
    ~ .x %>%
      st_write(
        paste0("output/calfire_app/data/", .y, ".geojson"),
        delete_dsn = TRUE))


# Save rasters

rasters %>%
  set_names("build", "veg", "fire") %>%
  iwalk(
    ~ terra::writeRaster(.x,
                  paste0("output/calfire_app/data/", .y, ".tif"),
                  overwrite = TRUE))
