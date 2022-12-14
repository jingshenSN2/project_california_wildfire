
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Wrangling & Save --------------------------------------------------------

# Rename rasters
rasters_renamed <-
  rasters %>%
  set_names("build", "veg", "fire", "pop")

# Make subregion smaller

subregion_simplify <-
  cal_subregion %>%
  rename(subregion_name = name) %>%
  st_simplify(dTolerance = 1000) %>%
  mutate(subregion_area = st_area(.) %>%
           units::set_units("km^2") %>%
           as.numeric() %>%
           round(3))

road <-
  bind_rows(
    cal_highway %>%
      transmute(type = "highway"),
    cal_railway %>%
      transmute(type = "railway")) %>%
  group_by(type) %>% summarise() %>%
  st_simplify(dTolerance = 100) %>% 
  st_intersection(subregion_simplify)

urban <-
  cal_urban %>% 
  st_intersection(subregion_simplify)

subregion <-
  subregion_simplify %>%
  mutate(veg_level = terra::extract(rasters_renamed$veg,
                                    .,
                                    fun = mean,
                                    na.rm = TRUE) %>%
           pull() %>%
           round(4),
         build_density = terra::extract(rasters_renamed$build,
                                        .,
                                        fun = mean,
                                        na.rm = TRUE) %>%
           pull() %>%
           round(4),
         pop_density = terra::extract(rasters_renamed$pop$pop_density_2020,
                                      .,
                                      fun = mean,
                                      na.rm = TRUE) %>%
           pull() %>%
           round(4)) %>%
  left_join(road %>%
              group_by(subregion_id) %>%
              summarise() %>%
              mutate(road_density = st_length(.) %>%
                       units::set_units("km") %>%
                       as.numeric() %>%
                       round(1)) %>%
              as_tibble() %>%
              select(-geometry),
            by = "subregion_id") %>%
  left_join(urban %>%
              group_by(subregion_id) %>%
              summarise() %>%
              mutate(urban_density = st_area(.) %>%
                       units::set_units("km^2") %>%
                       as.numeric() %>%
                       round(1)) %>%
              as_tibble() %>%
              select(-geometry),
            by = "subregion_id") %>%
  mutate(road_density = road_density / subregion_area,
         urban_density = urban_density / subregion_area)


# Fire with subregion

fire_with_subregion <-
  fire_with_cause %>%
  
  # With largest=TRUE, this line can take 5+ min to run
  
  st_join(subregion_simplify,
          left = FALSE,
          largest = TRUE) %>%
  st_simplify(dTolerance = 100) %>% 
  filter(!st_is_empty(.))

fire_with_buffer <-
  fire_with_subregion %>%
  terra::vect() %>%
  terra::buffer(50000)  # 50km

fire <-
  fire_with_subregion %>%
  mutate(
    road_distance =
      st_distance(.,
                  road %>%
                    st_union()) %>%
      units::set_units("km") %>%
      as.numeric() %>%
      round(1),
    urban_distance =
      st_distance(.,
                  cal_urban %>%
                    st_union()) %>%
      units::set_units("km") %>%
      as.numeric() %>%
      round(1),
    veg_level = terra::extract(rasters$calveg,
                               fire_with_buffer,
                               fun = mean,
                               na.rm = TRUE) %>%
      pull() %>%
      round(4),
    build_density = terra::extract(rasters$cal_building,
                                   fire_with_buffer,
                                   fun = mean,
                                   na.rm = TRUE) %>%
      pull() %>%
      round(4),
    pop_density = terra::extract(rasters$pop$pop_density_2020,
                                 fire_with_buffer,
                                 fun = mean,
                                 na.rm = TRUE) %>%
      pull() %>%
      round(4))

fire %>%
  as_tibble() %>%
  
  # Since we joined data with subregion, we can use it in our app
  # without geometry
  
  select(-geometry) %>%
  arrange(alarm_date) %>%
  write_csv("output/calfire_app/data/fire.csv")

# Save shapefiles

list(
  subregion = subregion,
  fire = fire_with_subregion,
  road = road,
  urban = urban) %>%
  iwalk(
    ~ st_write(.x,
               paste0("output/calfire_app/data/", .y, ".geojson"),
               delete_dsn = TRUE))

# Save rasters

rasters_renamed %>%
  iwalk(
    ~ terra::writeRaster(.x,
                         paste0("output/calfire_app/data/", .y, ".tif"),
                         overwrite = TRUE))
