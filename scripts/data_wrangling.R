
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(purrr)

# Download ----------------------------------------------------------------

# Since we are using github to collaborate, there's a file size limitation
# to avoid triggering git LFS. So, all datasets are added to .gitignore,
# and will be downloaded using the following code.

# Clear directory

unlink("data/raw/shapefiles/*", recursive = TRUE)

# Urls to download files

urls <-
  list(
    calfire = "https://gis.data.cnra.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all-1.geojson",
    park = "https://opendata.arcgis.com/datasets/0fea4fa1db734794bdb3b5410bb3eed9_0.zip",
    picnic = "https://opendata.arcgis.com/datasets/d4a0a22eb5e5486d9902ae7decde256a_0.zip",
    camp = "https://opendata.arcgis.com/datasets/81c47beefff143ecb6c21bfd3ed6a933_0.zip",
    state = "https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/b0007416-a325-4777-9295-368ea6b710e6/download/ca-county-boundaries.zip")

# Filenames

filenames <-
  list(
    calfire = "cal_fire_all.geojson",
    park = "park_boundaries.zip",
    picnic = "picnic_grounds.zip",
    camp = "camp_grounds.zip",
    state = "cal_counties.zip") %>%
  map(
    ~paste0("data/raw/shapefiles/", .x))

# Download files

walk2(
  filenames,
  urls,
  ~ download.file(.y, .x))

# Unzip files

filenames %>%
  keep(
    ~ endsWith(.x, ".zip")) %>%
  walk(
    ~ unzip(
      .x,
      exdir = gsub(pattern = "\\.zip$", "", .x)))

# Special zip with nested folder, need to move to parent folder

file.copy(
  from = list.files(
    "data/raw/shapefiles/cal_counties/CA_Counties",
    full.names = TRUE
  ),
  to = "data/raw/shapefiles/cal_counties")

unlink("data/raw/shapefiles/cal_counties/CA_Counties", recursive = TRUE)

# Read shape files --------------------------------------------------------------

shapes <-
  filenames %>%
  map(
    ~ .x %>%
      gsub(pattern = "\\.zip$", "", .) %>%
      st_read() %>%
      set_names(
        tolower(
          names(.))))

# Check data ----------------------------------------------------------

# Check validity

shapes %>%
  map(
    ~ filter(
      .,
      !st_is_valid(.)) %>%
      nrow())

# Check crs

shapes %>%
  map(
    ~ st_crs(.x))

# Make all shapefiles valid

valid_shapes <-
  shapes %>%
  map(
    ~ st_make_valid(.x))

# Remove shapes

rm(shapes)

# Explore the data --------------------------------------------------------

# Take a glimpse

valid_shapes %>%
  walk(
    ~ glimpse(.x))

tmap_mode("view")

tm_shape(valid_shapes$state) +
  tm_polygons(alpha = 0.2) +
  
  tm_shape(valid_shapes$picnic) +
  tm_dots(col = "blue", size = 0.01) +
  
  tm_shape(valid_shapes$camp) +
  tm_dots(col = "orange", size = 0.01) +
  
  tm_shape(valid_shapes$park) +
  tm_polygons(col = "green", alpha = 0.2) +
  
  tm_shape(valid_shapes$calfire %>%
             mutate(year_ = as.integer(year_)) %>%
             filter(year_ > 2010)) +
  tm_polygons(col = "red", alpha = 0.2)


# Metadata ----------------------------------------------------------------

# We want get the following metadata of data files:
# file_name,online_link,file_size_mb,crs_epsg,number_of_fields_or_layers,
# number_of_features_or_cells,extent_xmin,extent_xmax,
# extent_ymin,extent_ymax,description_of_data

tibble(
  file_name = map_chr(filenames, basename),
  online_link = unlist(urls),
  file_size_mb = map_chr(filenames,
                         ~ file.size(.x) / 1e6),
  crs_epsg = map_chr(valid_shapes,
                     ~ st_crs(.x)$epsg),
  number_of_fields_or_layers = map_int(valid_shapes, ncol),
  number_of_features_or_cells = map_int(valid_shapes, nrow),
  extent_xmin = map_dbl(valid_shapes,
                        ~ st_bbox(.x)$xmin),
  extent_xmax = map_dbl(valid_shapes,
                        ~ st_bbox(.x)$xmax),
  extent_ymin = map_dbl(valid_shapes,
                        ~ st_bbox(.x)$ymin),
  extent_ymax = map_dbl(valid_shapes,
                        ~ st_bbox(.x)$ymax),
  description_of_data = c(
    "California fires record (as ploygons, with attributes like year, cause).",
    "California park boundaries (as polygons).",
    "All picnic areas (as points) in California.",
    "All campgrounds (as points) in California.",
    "All counties (as polygons) of California.")) %>%
  write_csv("data/metadata.csv")

# Save to geojson ---------------------------------------------------------

valid_shapes %>%
  imap(
    ~ .x %>%
      
      # Change crs
      
      st_transform(
        st_crs(valid_shapes$calfire)) %>%
      
      # Write to file
      
      st_write(
        dsn = paste0("data/processed/", .y, ".geojson"),
        delete_dsn = TRUE))

# Remove zip files

unlink("data/raw/shapefiles/*.zip", recursive = TRUE)
