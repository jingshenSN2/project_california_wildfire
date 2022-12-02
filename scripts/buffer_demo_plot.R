
# Read in data ------------------------------------------------------------

source("scripts/source_plot_data.R")


# Select a fire in north coast --------------------------------------------

fire <-
  fire_with_cause %>%
  filter(
    gis_acres > 1000000,
    gis_acres < 1050000,
    cause_name == "Lightning")

fire_with_buffer <-
  fire %>%
  st_buffer(units::set_units(50, km))


# Plot on map -------------------------------------------------------------

tmap_mode("view")

rasters$calveg %>%
  tm_shape(name = "Vegetation") +
  tm_raster(title = "Vegetation Level",
            alpha = 0.8,
            style = "cont",
            palette = "Greens") +
  fire_with_buffer %>%
  tm_shape(name = "Buffer") +
  tm_polygons(title = "50km Buffer",
              col = "#fec05b",
              alpha = 0.4,
              border.alpha = 0) +
  fire %>%
  tm_shape(name = "Fire") +
  tm_polygons(title = "Fire Perimeter",
              col = "#df171c",
              alpha = 0.4,
              border.alpha = 0)
  