

# setup -------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(stars)


# Request an api key from https://api.census.gov/data/key_signup.html

# Install your census key:

tidycensus::census_api_key(
  # Paste your key here!,
  install = TRUE)

# Get census variables associated with the annual American Community Survey 
# (ACS):

census_vars <-
  tidycensus::load_variables(
    year = 2021,
    dataset = 'acs1')



# finding variables  ------------------------------------------------------



# Look for variables 

census_vars %>% 
  filter(str_detect(concept, 'POPULATION')) %>% 
  select(concept) %>% 
  distinct()

# get the variable id for median monthly housing costs 

census_vars %>%   
  filter(concept == 'TOTAL POPULATION') %>% 
  pull(name)



# Get ACS data ------------------------------------------------------------

# ACS 2020 - 2015 population data data census tract level  

acs_population_2020 <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = 'B01003_001',
    year = 2020,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid()

# creating map

tmap_mode('view')

tm_shape(acs_population_2020) + 
  tm_polygons(col = 'estimate',
              title = 'observations',
              breaks = 
                c(2000,
                  3000,
                  4000,
                  5000,
                  10000,
                  15000,
                  20000, 
                  25000,
                  30000,
                  35000,
                  40000))

acs_population_2020 %>%
  st_rasterize(
    .,
    st_as_stars(
      st_bbox(.), nx = 200, ny = 200, values = NA_real_)) %>%
  tm_shape() +
  tm_raster(col = 'estimate',
            breaks = c(
              0,
              1000,
              2000,
              3000,
              5000,
              10000,
              100000))

# Save

state <-
  st_read("data/processed/state.geojson")

acs_population_2020 %>%
  
  # Transform the CRS
  
  st_transform(
    st_crs(state)) %>%
  
  # Write to disk
  
  st_write(
    dsn = "data/processed/ca_population_tract.geojson",
    delete_dsn = TRUE)
