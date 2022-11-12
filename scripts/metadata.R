
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(stars)


# Extract meta information ------------------------------------------------

extract_metainfo <-
  function(filename, url, desp) {
    
    # Read in data
    
    if (endsWith(filename, ".tif")) {
      shp <- read_stars(filename)
    }
    else {
      shp <- st_read(filename)
    }
    
    # Get bbox
    
    bbox <- st_bbox(shp)
    
    # Get num of feature and num of layer
    
    if (is(shp, "sf")) {
      # shapefile
      nlayer = ncol(shp)
      nfeat = nrow(shp)
    }
    if (is(shp, "stars")) {
      # raster data
      nlayer = length(shp)
      nfeat = nrow(shp) * ncol(shp)
    }
    
    meta <- tibble(
      file_name = basename(filename),
      online_link = url,
      file_size_mb = file.size(filename) / 1e6,
      crs_epsg = st_crs(shp)$epsg,
      number_of_fields_or_layers = nlayer,
      number_of_features_or_cells = nfeat,
      extent_xmin = bbox$xmin,
      extent_xmax = bbox$xmax,
      extent_ymin = bbox$ymin,
      extent_ymax = bbox$ymax,
      description_of_data = desp)
    
    return(meta)
  }


cal_fire_meta <-
  extract_metainfo(
    "data/processed/cal_fire_all.geojson",
    "https://gis.data.cnra.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all-1.geojson",
    "California fires record (as ploygons). We select some interested attributes
    including date, cause, burned area, etc.")

cal_counties_meta <-
  extract_metainfo(
    "data/processed/cal_counties.geojson",
    "https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/b0007416-a325-4777-9295-368ea6b710e6/download/ca-county-boundaries.zip",
    "California county boundary shapefile.")

cal_building_meta <-
  extract_metainfo(
    "data/processed/cal_building.tif",
    "https://usbuildingdata.blob.core.windows.net/usbuildings-v2/California.geojson.zip",
    "California building density (raster, unit: 1000 building/pixel). We use Microsoft's USBuildingFootPrints
    dataset (which contains more than 11 million building polygons) to create this
    raster data. The raw data is too large (3.3 GB), so we only report meta info
    of this raster data.")

cal_veg_meta <-
  extract_metainfo(
    "data/processed/calveg.tif",
    "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.EVMid_R05_CentralCoast.gdb.zip",
    "California tree coverage area (raster). We use CalVeg dataset (which contains
    about 300,000 polygons of vegetation classification records) to create this
    raster data. The raw data is separated in 11 sub-regions and the total size
    is too large(~ 3 GB), so we only report meta info of this raster data.")

cal_pop_2015_meta <-
  extract_metainfo(
    "data/processed/cal_population_tract_2015.geojson",
    "https://www.census.gov/programs-surveys/acs",
    "California population data (polygons) from Census ACS. We calculate population
    density based on the area of polygons. Time range: 2010-2015.")

cal_pop_2020_meta <-
  extract_metainfo(
    "data/processed/cal_population_tract_2020.geojson",
    "https://www.census.gov/programs-surveys/acs",
    "California population data (polygons) from Census ACS. We calculate population
    density based on the area of polygons. Time range: 2016-2020.")

cal_highway_meta <-
  extract_metainfo(
    "data/processed/cal_highway.geojson",
    "https://gisdata-caltrans.opendata.arcgis.com/datasets/1f71fa512e824ff09d4b9c3f48b6d602_0/about",
    "California highway shapefile (linestring).")

cal_railway_meta <-
  extract_metainfo(
    "data/processed/cal_railway.geojson",
    "https://gisdata-caltrans.opendata.arcgis.com/datasets/2ac93358aca84aa7b547b29a42d5ff52_0/about",
    "California railway shapefile (linestring).")

meta_tbl <-
  bind_rows(
    cal_fire_meta,
    cal_counties_meta,
    cal_building_meta,
    cal_veg_meta,
    cal_pop_2015_meta,
    cal_pop_2020_meta,
    cal_highway_meta,
    cal_railway_meta)

meta_tbl %>%
  write_csv("data/metadata.csv")

rm(cal_fire_meta,
   cal_counties_meta,
   cal_building_meta,
   cal_veg_meta,
   cal_pop_2015_meta,
   cal_pop_2020_meta,
   cal_highway_meta,
   cal_railway_meta,
   meta_tbl,
   extract_metainfo)
