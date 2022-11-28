
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Plot fire in counties ---------------------------------------------------

fire_by_county <-
  fire_with_cause %>%
  
  # Spatial join with county
  
  st_join(cal_counties %>%
            select(geoid)) %>%
  
  # Group by geoid
  
  as_tibble() %>%
  group_by(geoid) %>%
  
  # Count and area
  
  summarise(
    n = n(),
    area = sum(gis_acres)) %>%
  
  left_join(cal_counties,
            .,
            by = 'geoid') %>%
  
  replace_na(
    list(n = 0, area = 0))

m1 <-
  tm_shape(fire_by_county) +
  tm_polygons(title = "Number of wildfires",
              col = "n",
              style = "cont") +
  
  
  # credits
  
  tm_credits('Source: 2021 California Natural Resources Agency, State of California') +
  
  # change layout 
  
  tm_layout(
    main.title = 'Number of wilderfires at county level in california',
    frame = '#999999',
    legend.outside = TRUE,
    attr.outside = TRUE) 

  

tmap_save(m1, "output/static_maps/fire_count.png")

m2 <-
  tm_shape(fire_by_county) +
  tm_polygons(title = "Total burned area [acres]",
              col = "area",
              style = "cont") +
  
  tm_layout(
    main.title = 'Burned areas at county level',
    frame = '#999999',
    legend.outside = TRUE)

tmap_save(m2, "output/static_maps/fire_area.png")

m_pop <-
  tm_shape(cal_population_tract_2020) +
  tm_fill(title = "Population",
          col = "estimate",
          style = "cont")

tmap_save(m_pop, "output/static_maps/population.png")


# Some rasters ------------------------------------------------------------

m3 <-
  fire_r %>%
  terra::mask(cal_counties) %>%
  tm_shape() +
  tm_raster(title = "Number of wildfires",
            style = "cont") +
  tm_layout(
    legend.position = c("right", "top")) +
  
  tm_shape(cal_outline) +
  tm_borders(lwd = 1)

tmap_save(m3, "output/static_maps/fire_count_raster.png")


m4 <-
  rasters$calveg %>%
  terra::mask(cal_counties) %>%
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
  rasters$cal_building %>%
  terra::mask(cal_counties) %>%
  tm_shape() +
  tm_raster(title = "Building density",
            style = "cont",
            palette = "Greys") +
  tm_layout(
    legend.position = c("right", "top")) +
  
  tm_shape(cal_outline) +
  tm_borders(lwd = 1)

tmap_save(m5, "output/static_maps/building_raster.png")
