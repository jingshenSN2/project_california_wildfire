
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)


# UI function -------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "California Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("clipboard")),
      menuItem("Wildfire Summary", tabName = "fire_summary", icon = icon("table")),
      menuItem("Wildfires Visualization", tabName = "fire_viz", icon = icon("fire")),
      menuItem("Vegetation", tabName = "veg", icon = icon("tree")),
      menuItem("Road", tabName = "road", icon = icon("road")),
      menuItem("Population", tabName = "pop", icon = icon("users")),
      menuItem("Building", tabName = "build", icon = icon("city"))
    ),
    dateRangeInput(
      "date",
      "Alarm Date Range",
      start = "2000-01-01",
      end = "2020-12-31",
      min = "1980-01-01",
      max = "2023-01-01"
    ),
    selectInput(
      "subregion",
      "Subregion",
      c("All" = 0,
        "North Coast and Montane" = 1,
        "North Interior" = 2,
        "North Sierran" = 3,
        "South Sierran" = 4,
        "Central Valley" = 5,
        "Central Coast and Montane" = 6,
        "South Coast and Montane" = 7,
        "South Interior" = 8,
        "Great Basin" = 9)
    ),
    checkboxGroupInput(
      "causes",
      "Cause of fire",
      c("Human", "Natural", "Structure", "Vehicle", "Other"),
      c("Human", "Natural")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introduction"),
        tmapOutput("subregion")
      ),
      tabItem(
        tabName = "fire_summary",
        h2("Wildfire Summary Table"),
        p("Here is a statistical table of wildfire burned area grouped 
          by alarm date year or subregion. The area is in acres."),
        radioButtons(
          "summary_by",
          "Summary By",
          c("Year" = "year",
            "Subregion (The selection of the drop-down menu in the sidebar is ignored)" = "subregion")
        ),
        dataTableOutput("summary")
      ),
      tabItem(
        tabName = "fire_viz",
        h2("Wildfire Visualization"),
        p("Plot and map will take some time to display."),
        box(
          title = "Large Wildfire on Map",
          status = "primary",
          p("Here are the perimeters of wildfires that burned an area greater than 5,000 acres."),
          tmapOutput("large_fire")
        ),
        box(
          title = "Wildfire Plot by Year",
          status = "primary",
          p("Here are the number of wildfires count by year."),
          plotOutput("fire_plot")
        )
      ),
      tabItem(
        tabName = "veg",
        h2("Vegetation"),
        p("Plot and map will take some time to display."),
        box(
          title = "Vegetation on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("veg_map")
        ),
        box(
          title = "Wildfire vs Vegetation",
          status = "primary",
          p("Wildfires have a negative correlation with vegetation level."),
          plotOutput("veg_plot")
        )
      ),
      tabItem(
        tabName = "road",
        h2("Road"),
        p("Roads(highway/railway) are a sign for human presence/activity. 
          Let's explore the relationship between wildfires and the 
          distance to the nearest road."),
        box(
          title = "Road v.s. Fire on Map",
          status = "primary",
          tmapOutput("road")
        ),
        box(
          title = "Road Distance Histogram",
          status = "primary",
          plotOutput("road_distance")
        )
      ),
      tabItem(
        tabName = "pop",
        h2("Population")
      ),
      tabItem(
        tabName = "build",
        h2("Building")
      )
    )
  )
)
