

# setup -------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(stars)
library(units)


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

census_vars %>% 
  filter(str_detect(concept, 'SEX')) %>% 
  select(concept) %>% 
  distinct()

# get the variable id for median monthly housing costs 

census_vars %>%   
  filter(concept == 'TOTAL POPULATION') %>% 
  pull(name)

census_vars %>%   
  filter(concept == 'SEX BY AGE') %>% 
  pull(name)

# B01001_026 total female, 
# B01001_001 total population same as B01003_001
# B01001_002 total male 


# Get ACS data ------------------------------------------------------------

# ACS 2020 - 2016 population data data census tract level  

acs_population_2020 <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = 'B01003_001',
    year = 2020,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid() %>% 
  
  # caculate land area 
  
  mutate(land_area = 
           st_area(.) %>% 
           units::set_units('km^2')) %>% 
  
  # caculate population density 
  
  mutate(population_density = estimate/land_area)

# county 

acs_population_2020_county <-
  tidycensus::get_acs(
    geography = 'county',
    variables = 'B01003_001',
    year = 2020,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid()


acs_population_2020_f <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = c('B01003_001', 'B01001_026'),
    year = 2020,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid()

# 2015 - 2010 ACS data 

acs_population_2010_2015 <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = 'B01003_001',
    year = 2015,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid() %>% 
  
  # caculate land area 
  
  mutate(land_area = 
           st_area(.) %>% 
           units::set_units('km^2')) %>% 
  
  # caculate population density 
  
  mutate(population_density = estimate/land_area)


# 2006 - 2010 ACS data 

acs_population_2010 <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = 'B01003_001',
    year = 2010,
    state = '06', # 06 represent CA
    survey = 'acs5',
    geometry = TRUE) %>% 
  st_make_valid() %>% 
  
  # caculate land area 
  
  mutate(land_area = 
           st_area(.) %>% 
           units::set_units('km^2')) %>% 
  
  # caculate population density 
  
  mutate(population_density = estimate/land_area)

# creating map

tmap_mode('view')

tm_shape(acs_population_2020) + 
  tm_polygons(col = 'estimate',
              title = 'population',
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

# population density map 

# check min and max population density

summary(acs_population_2020$population_density)

acs_population_2020 %>% 
  ggplot() +
  geom_sf(
    aes(fill = 
          as.numeric(population_density))) +
  scale_fill_viridis_c(
    option = 'plasma',
    na.value = '#dcdcdc') 


tm_shape(acs_population_2020) +
  tm_polygons(col = 'population_density',
              title = 'Population density',
              breaks = 
                c(500,
                  1000,
                  2000,
                  2500,
                  3000,
                  4000,
                  10000,
                  20000,
                  30000,
                  40000,
                  50000,
                  60000,
                  70000),
              border.alpha = 0)



# county 

tm_shape(acs_population_2020_county) + 
  tm_polygons(col = 'estimate',
              title = 'population')
# breaks = 
#   c(
#     10000,
#     15000,
#     20000, 
#     25000,
#     30000,
#     35000,
#     40000)

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

# plot for 2010 - 2015 

tm_shape(acs_population_2010_2015) + 
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

# Save

acs_population_2020 %>%
  
  # Transform the CRS
  
  st_transform(4326) %>%
  
  # Write to disk
  
  st_write(
    dsn = "data/processed/cal_population_tract_2020.geojson",
    delete_dsn = TRUE)

acs_population_2010_2015 %>%
  
  # Transform the CRS
  
  st_transform(4326) %>%
  
  # Write to disk
  
  st_write(
    dsn = "data/processed/cal_population_tract_2015.geojson",
    delete_dsn = TRUE)

acs_population_2010 %>%
  
  # Transform the CRS
  
  st_transform(4326) %>%
  
  # Write to disk
  
  st_write(
    dsn = "data/processed/cal_population_tract_2010.geojson",
    delete_dsn = TRUE)

# Release memory

rm(acs_population_2010_2015,
   acs_population_2020,
   acs_population_2020_county,
   acs_population_2020_f,
   census_vars)
