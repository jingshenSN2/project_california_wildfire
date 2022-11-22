
# Setup -------------------------------------------------------------------

library(lubridate)
library(purrr)
library(tidyverse)
library(sf)
library(units)

library(ggthemes)


# Read in data ------------------------------------------------------------

fire <- st_read("data/processed/cal_fire_all.geojson")
fire_cause <- read_rds("data/processed/fire_cause.rds")
highway <- st_read("data/processed/cal_highway.geojson")
railway <- st_read("data/processed/cal_railway.geojson")


# Preprocess --------------------------------------------------------------

roads <- st_union(
  bind_rows(
    highway %>%
      select(geometry),
    railway %>%
      select(geometry)))

fire_with_cause <-
  fire %>%
  filter(year(alarm_date) >= 1950 & year(alarm_date) <= 2023) %>%
  mutate(
    cause_category = case_when(
      cause %in% fire_cause$category$human ~ "Human",
      cause %in% fire_cause$category$natural ~ "Natural",
      cause %in% fire_cause$category$vehicle ~ "Vehicle",
      cause %in% fire_cause$category$structure ~ "Structure",
      TRUE ~ "other"),
    distance = st_distance(., roads) %>%
      set_units("km"))


ourtheme <-
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text())

# Plot of fire vs year ----------------------------------------------------

fire_with_cause %>%
  
  filter(cause_category != "other") %>%
  
  mutate(year = year(alarm_date)) %>%
  
  as_tibble() %>%
  
  # Group by cause and alarm year
  
  group_by(year, cause_category) %>%
  
  # Simply count number of fires
  
  summarise(n = n()) %>%
  
  # Plot fire count per year by cause
  
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_smooth(method = "lm") +
  
  # Facet by cause
  
  facet_wrap(
    ~ cause_category,
    scales = "free_y") +
  
  ourtheme +
  
  labs(x = "Year",
       y = "Number of fires",
       title = "Wildfires become more frequent in recent years?",
       subtitle = "A scatterplot of fire count per year by cause")

ggsave("output/plots/fire_year.png",
       height = 8,
       width = 8)


# Plot of distance to road ------------------------------------------------

fire_with_cause %>%
  
  filter(cause_category != "other") %>%
  
  as_tibble() %>%
  
  # Plot distance to road
  
  ggplot(aes(x = distance)) +
  geom_histogram(color = "black", fill = "white") +
  
  facet_wrap(
    ~ cause_category,
    scales = "free_y") +
  
  ourtheme +
  
  labs(x = "Distance to road (highway/railway)",
       y = "Number of fires",
       title = "Wildfires occur more often close to roads?",
       subtitle = "A histogram of fire and distance to road by cause")

ggsave("output/plots/fire_road_distance.png",
       height = 8,
       width = 8)


# Calendar heatmap --------------------------------------------------------

fire_with_cause %>%
  as_tibble() %>%
  
  filter(
    cause_category != "other") %>%
  
  mutate(
    year = year(alarm_date),
    week = week(alarm_date)) %>%
  
  filter(year >= 1980 & year <= 2020) %>%
  
  group_by(year, week, cause_category) %>%
  
  summarise(n = n()) %>%
  
  mutate(
    level = cut(
      n,
      breaks = c(-Inf, 5, 10, 20, 50, Inf),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("Very Low",
                 "Low",
                 "Medium",
                 "High",
                 "Very High"))) %>%
  
  ggplot() +
  geom_tile(aes(x = week, y = year, fill = level)) +
  scale_fill_brewer(palette = "OrRd") +
  
  facet_wrap(
    ~ cause_category) +
  
  ourtheme +
  
  labs(x = "Week of Year",
       y = "Year",
       title = "When did wildfires happen?",
       subtitle = "A calendar heatmap of fire count each week in 1980-2020",
       caption = "Very Low: 0~5, Low: 6~10,\nMedium: 11~20, High: 21~50,\nVery High: 50+",
       fill = "Frequency")

ggsave("output/plots/fire_week.png",
       height = 8,
       width = 8)
