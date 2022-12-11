
# Setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)


# UI function -------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "California Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",
               tabName = "intro", 
               icon = icon("clipboard")),
      menuItem("Wildfire Summary",
               tabName = "fire_summary",
               icon = icon("table")),
      menuItem("Wildfires Visualization",
               tabName = "fire viz",
               icon = icon("fire"),
               menuItem('Static Map',
                        tabName = 'map'),
               menuItem('Interactive map',
                        tabName = 'fire_viz')),
      menuItem("Nature Factor",
               tabName = "nature_factor",
               icon = icon("mountain"),
               menuItem("Vegetation",
                        tabName = "veg",
                        icon = icon("tree"))),
      menuItem("Human Factor",
               tabName = "human_factor",
               icon = icon("tree"),
               menuItem("Road",
                        tabName = "road", 
                        icon = icon("road")),
               menuItem("Urban Area",
                        tabName = "urban",
                        icon = icon("city")),
               menuItem("Population",
                        tabName = "pop",
                        icon = icon("users")),
               menuItem("Building",
                        tabName = "build",
                        icon = icon("house")))
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
        h1("Wildfires: Human Activity and Natural Facotr"),
        p(HTML("A case study of wildfire records in California during 1980-2020
               <br/>by Jingsong Gao & Haiwen Lei"),
          style = "font-size:18px"),
        h2("Introduction"),
        p("This is a geospatial analysis project about the trend of wildfires
          and human activities. We picked the state of California as our region
          of focus since enormous wildfires occurred in this region in recent
          years. The research question of the project is how human activity
          affects the occurrence of wildfires. We hypothesize that wildfires
          caused by humans are geographically associated with human factors.
          Vice versa, wildfires caused by nature are more geographically
          associated with vegetation. While the results from our tests match
          the prediction that wildfires tend to be positively associated with
          population and building density, distance to roads, and distance
          to the urban area, a more sophisticated model may be needed in future
          research to prove these relationships."), 
        p("In this shiny app, you can explore the relationship between the
          trend of wildfires that happened in California from 1980 to 2020
          and the human factor and nature factor (level of vegetation)."),
        p("In our analysis, we divided California into 9 subregions.
          Below is a map showing the boundaries of these subregions."),
        box(
          plotOutput("subregion")
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
            "Subregion (The selection of the drop-down menu
            in the sidebar is ignored)" = "subregion")
        ),
        dataTableOutput("summary")
      ),
      tabItem(
        tabName = "map",
        h2("Wildfire occurances and burned areas at county level 
           from 1980 to 2020"),
        p("Before analyzing the relationship with other factors, let's first
          understand a little bit about the wildfire itself. While more fires 
          happened in the south of California, the total burned areas in the 
          north of California are larger. The fire data used
          in the project is from 
          California Department of Forestry and Fire Protection, 
          Fire and Resource Assessment Program (FRAP)."),
        box(
          plotOutput('map1'),
          status = "primary",
          p(
            HTML(
            'Data Source:<br/> California Department of Forestry 
            and Fire Protection, Fire and Resource Assessment Program (FRAP)'),
          style = "font-size:10px")
        ),
        box(
          plotOutput('map2'),
          status = "primary",
          p(
            HTML(
            'Data Source:<br/> California Department of Forestry and 
            Fire Protection,  Fire and Resource Assessment Program (FRAP)'),
          style = "font-size:10px")
        ),
      ),
      tabItem(
        tabName = "fire_viz",
        h2('Interactive map of wildfire'),
        p("There are 19 types of wildfire causes recorded in the data.
          The project categorized them into five types: 
          human, vehicle, structure, natural, and other.
          Interactive map and plot will take some time to display."),
        box(
          title = "Large Wildfire on Map",
          status = "primary",
          p("Here are the perimeters of wildfires that burned an area greater
            than 5,000 acres."),
          tmapOutput("large_fire"),
          p(
            HTML(
            'Data Source:<br/> California Department of Forestry 
            and Fire Protection, Fire and Resource Assessment Program (FRAP)'),
          style = "font-size:10px")
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
          Are wildfires more likely to occur in heavy vegetation areas?
         You can select through cause of fire radio buttom to find out that 
         areas with a high level of vegetation seem to
         have more natural wildfires."),
        p("Plot and map will take some time to display."),
        box(
          title = "Vegetation v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("veg_map"),
          p(
            HTML('Data Source:<br/> 1. California Department of
                 Forestry and Fire Protection, Fire and Resource 
                 Assessment Program (FRAP) 
           <br/> 2. USDA:Forest services Calveg'),
          style = "font-size:10px")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Vegetation Level vs Number of
              Wildfires in 9 subregions."),
            plotOutput("veg_summary")
          ),
          tabPanel(
            title = "Local Vegetation Level",
            p("Average Vegetation Level within a wildfire and its 50km
              surrouding area."),
            plotOutput("veg_plot")
          )
        )
      ),
      tabItem(
        tabName = "road",
        h2("Road"),
        p("Roads(highway/railway) are a sign of human presence/activity. 
          Human casued wildfires tend to occur closer to roads."),
        p("Plot and map will take some time to display."),
        box(
          title = "Road v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("road_map"),
          p(
            HTML('Data Source:<br/> 1. California Department 
                 of Forestry and Fire Protection, Fire and Resource 
                 Assessment Program (FRAP) 
           <br/> 2. Caltrans Gis Data'),
          style = "font-size:10px")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Road density vs Number of Wildfires in 9 subregions."),
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
          Human caused wildfires tends to occur near urban areas"),
        p("Plot and map will take some time to display."),
        box(
          title = "Urban Area v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("urban_map"),
          p(
            HTML('Data Source:<br/> 1. California Department of 
                 Forestry and Fire Protection, Fire and Resource 
                 Assessment Program (FRAP) 
           <br/> 2. Caltrans Gis Data'),
           style = "font-size:10px")
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
          You can choose the layer of population density in 2020, 2015, and 2010.
          There seems to be a postive relationship between wildfire and
          population density."),
        box(
          title = "Population v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("pop_map"),
          p(
            HTML('Data Source:<br/> 1. California Department of Forestry 
                 and Fire Protection, Fire and Resource Assessment Program (FRAP) 
           <br/>2. American Community Survey 5-year'),
           style = "font-size:10px")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Population Density vs Number of Wildfires
              in 9 subregions."),
            plotOutput("pop_summary")
          ),
          tabPanel(
            title = "Local Population Density",
            p("Average Population Densityl within a wildfire and its
              50km surrouding area."),
            plotOutput("pop_plot")
          )
        )
      ),
      tabItem(
        tabName = "build",
        h2("Building"),
        p("Buildings are also a very straighforward sign of human presence. 
          Are wildfires more likely to occur near areas of high
          building density? The analysis showed similar results 
          as in population density"),
        box(
          title = "Building v.s. Wildfire on Map",
          status = "primary",
          p("Switch between two layers to see the relationship."),
          tmapOutput("build_map"),
          p(
            HTML('Data Source:<br/> 1. California Department of Forestry and 
                 Fire Protection, 
          Fire and Resource Assessment Program (FRAP) 
          <br/>2.Microsoft US Building Footprints'),
          style = "font-size:10px")
        ),
        tabBox(
          tabPanel(
            title = "Subregion Summary",
            p("Average Building Density vs Number of Wildfires
              in 9 subregions."),
            plotOutput("build_summary")
          ),
          tabPanel(
            title = "Local Building Density",
            p("Average Building Densityl within a wildfire
              and its 50km surrouding area."),
            plotOutput("build_plot")
          )
        )
      )
    )
  )
)
