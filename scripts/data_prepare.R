
# Setup -------------------------------------------------------------------

library(purrr)
library(tidyverse)
library(sf)
library(stars)
library(tmap)


# Download ----------------------------------------------------------------

# Since we are using github to collaborate, there's a file size limitation
# to avoid triggering git LFS. So, all datasets are added to .gitignore,
# and will be downloaded using the following code.

# Clear directory

unlink("data/raw/shapefiles/[!.]*", recursive = TRUE)

# Urls to download files

urls <- 
  list(
    calfire = "https://gis.data.cnra.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all-1.geojson",
    building = "https://usbuildingdata.blob.core.windows.net/usbuildings-v2/California.geojson.zip",
    county = "https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/b0007416-a325-4777-9295-368ea6b710e6/download/ca-county-boundaries.zip")

# Filenames

filenames <-
  list(
    calfire = "cal_fire_all.geojson",
    building = "cal_building.zip",
    county = "cal_counties.zip") %>%
  map(
    ~paste0("data/raw/shapefiles/", .x))

# Download files

# Use a larger timeout
options(timeout = 1200)

walk2(
  filenames,
  urls,
  ~ download.file(.y, .x))

# Urls for CalVeg data

calveg_names <-
  c("CentralCoast",
    "CentralValley",
    "GreatBasin",
    "NorCoastEast",
    "NorCoastMid",
    "NorCoastWest",
    "NorthInterior",
    "NorthSierra",
    "SouCoast",
    "SouthInterior",
    "SouthSierra")

calveg_urls <-
  calveg_names %>%
  map(
    ~ paste0(
      "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.EVMid_R05_",
      .x,
      ".gdb.zip"))

# Generate filenames

calveg_filenames <-
  calveg_names %>%
  map(
    ~ paste0(
      "data/raw/shapefiles/calveg_",
      tolower(.x),
      ".gdb.zip"))

# Download files

walk2(
  calveg_filenames,
  calveg_urls,
  ~ download.file(.y, .x))


# Unzip files -------------------------------------------------------------

filenames %>%
  keep(
    ~ endsWith(.x, ".zip")) %>%
  walk(
    ~ unzip(
      .x,
      junkpaths = TRUE,
      exdir = gsub(pattern = "\\.zip$", "", .x)))

# Special zip with nested folder, need to move to parent folder

file.copy(
  from = "data/raw/shapefiles/cal_building/California.geojson",
  to = "data/raw/shapefiles/cal_building.geojson")

unlink("data/raw/shapefiles/cal_building", recursive = TRUE)

# Unzip calveg

calveg_filenames %>%
  walk(
    ~ unzip(
      .x,
      junkpaths = TRUE,
      exdir = gsub(pattern = "\\.zip$", "", .x)))

rm(filenames, urls)


# Exploring shape files --------------------------------------------------------------

# Many of our raw datasets are so big which prevents us from
# using purrr:map() function. We have to read, process one-by-one
# to save memory. Also, raw shapefiles won't be stored in global
# variables in order to save memory.


# California County -------------------------------------------------------

cal_counties <-
  st_read("data/raw/shapefiles/cal_counties") %>%
  st_make_valid() %>%
  st_transform(4326)

cal_counties %>%
  set_names(
    tolower(
      names(.))) %>%
  st_write(
    dsn = paste0("data/processed/cal_counties.geojson"),
    delete_dsn = TRUE)


# California Fire ---------------------------------------------------------

# Make polygon valid and save as processed data

cal_fire <-
  st_read("data/raw/shapefiles/cal_fire_all.geojson") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  set_names(
    tolower(
      names(.))) %>%
  select(alarm_date,
         cont_date,
         cause,
         gis_acres,
         objective,
         geometry)

cal_fire_r <-
  cal_fire %>%
  filter(alarm_date > '2010-01-01') %>%
  select(geometry) %>%
  mutate(n = 1) %>%
  st_rasterize(cal_stars,
               options = c("MERGE_ALG=ADD",
                           "ALL_TOUCHED=TRUE")) %>%
  replace(. == 0, NA)

(cal_fire_r / 1000) %>%
  tm_shape() +
  tm_raster(col = "n",
            alpha = 0.8,
            palette = "YlOrRd",
            style = "cont")

# Save raster data

write_stars(cal_fire_r / 1000, "data/processed/cal_fire.tif")

# Save shapefile

cal_fire %>%
  st_write(
    dsn = "data/processed/cal_fire_all.geojson",
    delete_dsn = TRUE)

# Release memory

rm(cal_fire, cal_fire_r)


# California Building -----------------------------------------------------

# Bbox for rasterize

cal_stars <-
  st_as_stars(
    st_bbox(
      cal_counties %>%
        st_transform(4326)),
    nx = 800,
    ny = 800,
    values = 0)

# Rasterize building, count all points in a grid.

cal_building_r <-
  st_read("data/raw/shapefiles/cal_building.geojson") %>%
  select(geometry) %>%
  mutate(n = 1) %>%
  st_transform(4326) %>%
  st_rasterize(cal_stars,
               options = c("MERGE_ALG=ADD",
                           "ALL_TOUCHED=TRUE")) %>%
  replace(. == 0, NA)

# A quick viz

tmap_mode("view")

(cal_building_r / 1000) %>%
  tm_shape() +
  tm_raster(col = "n",
            alpha = 0.8,
            palette = "Greys",
            style = "cont")

# Save raster data

write_stars(cal_building_r / 1000, "data/processed/cal_building.tif")

# Remove from memory

rm(cal_building_r)


# California Vegetation ---------------------------------------------------

calveg <-
  calveg_filenames %>%
  map(
    ~ gsub(pattern = "\\.zip$", "", .x) %>%
      st_read() %>%
      
      # Calculate tree coverage area
      
      mutate(
        TREE_CFA = as.numeric(TREE_CFA_CLASS) * SHAPE_Area / 100) %>%
      
      # Drop na
      
      drop_na(TREE_CFA) %>%
      
      # Filter geometry and tree coverage area
      
      select(SHAPE, TREE_CFA) %>%
      
      # Transform crs
      
      st_transform(4326) %>%
      
      # Rasterize
      
      st_rasterize(cal_stars,
                   options = c("MERGE_ALG=ADD",
                               "ALL_TOUCHED=TRUE")))

# Combine rasters from different regions

calveg_r <-
  calveg %>%
  reduce(c) %>%
  merge() %>%
  st_apply(
    1:2,
    sum,
    .fname = "tree_area") %>%
  replace(. == 0, NA)

# A quick viz

calveg_r %>%
  tm_shape() +
  tm_raster(col = "tree_area",
            alpha = 0.8,
            palette = "Greens",
            style = "cont")

# Save raster data

write_stars(calveg_r, "data/processed/calveg.tif")

# Release memory

rm(calveg,
   calveg_r,
   calveg_names,
   calveg_filenames,
   calveg_urls)

rm(cal_stars,
   cal_counties)


# Test reload -------------------------------------------------------------

# Read back some raster data

r <-
  list(
    veg = read_stars("data/processed/calveg.tif"),
    building = read_stars("data/processed/cal_building.tif"),
    fire = read_stars("data/processed/cal_fire.tif"))

# Plot in one

tm_shape(r$veg) +
  tm_raster(alpha = 0.8, palette = "Greens", style = "cont", contrast = c(0.2, 0.9)) +
  
  tm_shape(r$building) +
  tm_raster(alpha = 0.8, palette = "Greys", style = "cont", contrast = c(0.2, 0.9)) +
  
  tm_shape(r$fire) +
  tm_raster(alpha = 0.8, palette = "YlOrRd", style = "cont", contrast = c(0.2, 0.9))

# Plot some correlation

r_tbl <-
  r %>%
  map(
    ~ .x %>%
      as_tibble() %>%
      set_names("x", "y", "value") %>%
      pull(value) %>%
      replace_na(0)) %>%
  as_tibble()

r_tbl %>%
  select(veg, building) %>%
  filter(veg != 0 | building != 0) %>%
  ggplot() +
  geom_point(aes(x = veg, y = building))

r_tbl %>%
  select(veg, fire) %>%
  filter(fire != 0) %>%
  ggplot() +
  geom_point(aes(x = veg, y = fire))

r_tbl %>%
  select(building, fire) %>%
  filter(fire != 0) %>%
  ggplot() +
  geom_point(aes(x = building, y = fire))

rm(r, r_tbl)

# Code domain mapping -----------------------------------------------------

# Fire cause, from https://frap.fire.ca.gov/frap-projects/fire-perimeters/

fire_cause <-
  list(
    
    # Human readable names for fire causes
    
    cause_name = list(
      `1` = "Lightning",
      `2` = "Equipment Use",
      `3` = "Smoking",
      `4` = "Campfire",
      `5` = "Debris",
      `6` = "Railroad",
      `7` = "Arson",
      `8` = "Playing with fire",
      `9` = "Miscellaneous",
      `10` = "Vehicle",
      `11` = "Powerline",
      `12` = "Firefighter Training",
      `13` = "Non-Firefighter Training",
      `14` = "Unknown / Unidentified",
      `15` = "Structure",
      `16` = "Aircraft",
      `17` = "Volcanic",  # Not appeared in data
      `18` = "Escaped Prescribed Burn",
      `19` = "Illegal Alien Campfire"),
    
    # Category of fire causes
    # `human`: Caused by human, but vehicle/structure are not include.
    # `natural`: Caused by natural phenomenons, like lightning.
    # `vehicle`: Caused by vehicle, perhaps car spontaneous combustion.
    # `structure`: Caused by static structures, like power line.
    # `other`: Cause is not easy to classify into any categories above.
    
    category = list(
      human = c(2, 3, 4, 6, 7, 8, 19),
      natural = c(1, 17, 18),
      vehicle = c(10, 16),
      structure = c(11, 15),
      other = c(5, 9, 12, 13, 14)))

saveRDS(fire_cause, "data/processed/fire_cause.rds")

rm(fire_cause)


# Clean folder ------------------------------------------------------------

# To save storage, we remove all raw data since they no longer needed.

unlink("data/raw/shapefiles/[!.]*", recursive = TRUE)
