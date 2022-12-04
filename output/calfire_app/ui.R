
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
      menuItem("Nature Factor",
               tabName = "nature_factor",
               icon = icon("mountain"),
               menuItem("Vegetation", tabName = "veg", icon = icon("tree"))),
      menuItem("Human Factor",
               tabName = "human_factor",
               icon = icon("tree"),
               menuItem("Road", tabName = "road", icon = icon("road")),
               menuItem("Urban Area", tabName = "urban", icon = icon("city")),
               menuItem("Population", tabName = "pop", icon = icon("users")),
               menuItem("Building", tabName = "build", icon = icon("house")))
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
    ),
    sliderInput(
      "agg",
      "Raster Aggregation Level",
      min = 0,
      max = 4,
      value = 2,
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introduction"),
        p("This is a geospital analyisis project about the trend of wildwires and human activity. We picked the state of Califronia as our region of focus since enormous wildfire occured in this region in recent years. In our analysis, We devided california into 9 subregions. Below is a map of showing the boundaries of these subregions."),
        box(
          tmapOutput("subregion")
        )
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
        p("Before analyzing the relationship with other factors, let's first
          understand a little bit about the wildfire itself."),
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
        p("Vegetation (trees/shrubs/grass) is a good fuel the wildfires.
          Are wildfires more likely to occur in heavy vegetation areas?"),
        p("Plot and map will take some time to display."),
        box(
          title = "Vegetation v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("veg_map")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Vegetation Level vs Number of Wildfires in 9 subregions."),
            plotOutput("veg_summary")
          ),
          tabPanel(
            title = "Local Vegetation Level",
            p("Average Vegetation Level within a wildfire and its 50km surrouding area."),
            plotOutput("veg_plot")
          )
        )
      ),
      tabItem(
        tabName = "road",
        h2("Road"),
        p("Roads(highway/railway) are a sign of human presence/activity. 
          Are wildfires more likely to occur near roads?"),
        p("Plot and map will take some time to display."),
        box(
          title = "Road v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("road_map")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Road Length vs Number of Wildfires in 9 subregions."),
            plotOutput("road_summary")
          ),
          tabPanel(
            title = "Nearest Road Distance",
            p("Distance from a wildfire to the nearest road."),
            plotOutput("road_plot")
          )
        )
      ),
      tabItem(
        tabName = "urban",
        h2("Urban Area"),
        p("Urban area can be a sign of human presence/activity. 
          Are wildfires more likely to occur near urban area?"),
        p("Plot and map will take some time to display."),
        box(
          title = "Urban Area v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("urban_map")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Urban Area vs Number of Wildfires in 9 subregions."),
            plotOutput("urban_summary")
          ),
          tabPanel(
            title = "Nearest Urban Area",
            p("Distance from a wildfire to the nearest urban area."),
            plotOutput("urban_plot")
          )
        )
      ),
      tabItem(
        tabName = "pop",
        h2("Population"),
        p("Population is a very straighforward sign of human presence. 
          Are wildfires more likely to occur near populated areas?"),
        box(
          title = "Population v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("pop_map")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Population Density vs Number of Wildfires in 9 subregions."),
            plotOutput("pop_summary")
          ),
          tabPanel(
            title = "Local Population Density",
            p("Average Population Densityl within a wildfire and its 50km surrouding area."),
            plotOutput("pop_plot")
          )
        )
      ),
      tabItem(
        tabName = "build",
        h2("Building"),
        p("Buildings are also a very straighforward sign of human presence. 
          Are wildfires more likely to occur near areas of high building density?"),
        box(
          title = "Building v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("build_map")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Building Density vs Number of Wildfires in 9 subregions."),
            plotOutput("build_summary")
          ),
          tabPanel(
            title = "Local Building Density",
            p("Average Building Densityl within a wildfire and its 50km surrouding area."),
            plotOutput("build_plot")
          )
        )
      )
    )
  )
)
