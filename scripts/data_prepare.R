
# Setup -------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
library(purrr)

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

# Read shape files meta info --------------------------------------------------------------

all_filenames <-
  c("data/raw/shapefiles/cal_fire_all.geojson",
    "data/raw/shapefiles/cal_building.geojson",
    "data/raw/shapefiles/cal_counties",
    calveg_filenames)

shapes <-
  all_filenames %>%
  map(
    ~ st_read(.x) %>%
      set_names(
        tolower(
          names(.))))
  
# Check validity

shapes %>%
  map(
    ~ filter(
      .,
      !st_is_valid(.)) %>%
      nrow())

# heck crs

shapes %>%
  map(
    ~ st_crs(.x))


