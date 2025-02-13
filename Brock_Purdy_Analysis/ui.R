#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application
dashboardPage(
  
  # Application title
  dashboardHeader(title = "Brock Purdy Analysis"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("High-Pressure Situations", tabName = "pressure", icon = icon("chart-line")),
      menuItem("High-Rushing Games", tabName = "rushing", icon = icon("running")),
      menuItem("Short Passing Games", tabName = "shortpass", icon = icon("football-ball")),
      menuItem("Overall Performance vs. Other QBs", tabName = "performance", icon = icon("balance-scale")),
      menuItem("Predictive Model Analysis", tabName = "model", icon = icon("cogs")),
      menuItem("Exploratory Playground", tabName = "exploratory", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    fluidRow(
      
      column(width = 3,
             wellPanel(
               h4("Filters & Settings"),
               shinyjs::useShinyjs(),
               
               sliderInput("year_range",
                           "Select Year",
                           min = 1999, max = 2024, value = c(1999, 2024), step = 1,
                           sep = "",
                           animate = TRUE),
               
               selectInput("coach",
                           "Select Coach",
                           choices = qb_clean |> distinct(coach) |> pull(coach) |> sort(),
                           selected = "Kyle Shanahan",
                           selectize = TRUE),
               
               checkboxInput("active_players",
                             "Active Players Only",
                             FALSE),
               
               selectInput("qb_name",
                           "Select Quarterback",
                           choices = NULL,
                           selected = NULL,
                           multiple = FALSE,
                           selectize = TRUE),
               
               
               selectInput("dependent_var",
                           "Select Metric",
                           choices = c("EPA", "ANY/A", "Passer Rating")),
               
               sliderInput("max_rush_yds",
                           "Games With Rushing Yards Between:",
                           min = 0, max = 400, value = c(0, 400), step = 10,
                           animate = TRUE),
               
               checkboxInput("high_rush",
                             "High Rushing Support",
                             FALSE),
               
               sliderInput("min_pressure_rate",
                           "Select Minimum Sacks per Dropback",
                           min = 0, max = .2, value = 0, step = .01, 
                           animate = TRUE),
               
               textOutput("high_rush_explanation"),
               
               textOutput("stat_explanation"),
               
               textOutput('qb_error'),
               
               actionButton("reset_input", "Reset")
             )
      ),
      column(width = 9,
             tabItems(
               tabItem(tabName = "home",
                       h2("Home Page"),
                       p("Welcome!")
               ),
               tabItem(tabName = "pressure",
                       h2("Purdy under High-Pressure Situations"),
                       fluidRow(
                         box(width = 12, plotlyOutput("scatter_pressure", height = 400))
                       ),
                       checkboxInput("average_cluster",
                                     "Average of each Cluster",
                                     FALSE),
                       fluidRow(
                         box(width = 12, plotlyOutput("average_pressure_line", height = 400))
                       )
               ),
               tabItem(tabName = "rushing",
                       h2("Purdy's Performance in High-Rushing Games"),
                       fluidRow(
                         box(width = 12, plotlyOutput("runBoxPlot", height = 700))
                       )
               ),
               tabItem(tabName = "shortpass",
                       h2("Purdy's Performance in Short Passing Games")
               ),
               tabItem(tabName = "performance", 
                       h2("Evaluating Purdy's Overall Performance vs. Other QBs"),
                       fluidRow(
                         box(width = 12, plotlyOutput("boxplot", height = 500))
                       )
               ),
               tabItem(tabName = "model",
                       h2("Predictive Model Analysis"),
                       fluidRow(
                         box(width = 12, plotlyOutput("plot_model_actual", height = 500))
                       ),
                       fluidRow(
                       box(width = 12, checkboxInput("average_cluster",
                                     "Average of each Cluster",
                                     FALSE), height = 60)
                       ),
                       fluidRow(
                         box(width = 12, DTOutput("qb_games_table"))
                       )
               ),
               tabItem(tabName = "exploratory",
                       h2("Exploratory Playground"),
                       fluidRow(
                         box(width = 4,
                             selectInput('x_axis',
                                         "Select x-axis",
                                         choices = qb_clean_numeric_col_names,
                                         selected = "Yards After Catch"),
                             selectInput('y_axis',
                                         "Select y-axis",
                                         choices = qb_clean_numeric_col_names,
                                         selected = "Sacks per Dropback"),
                             selectInput('z_axis',
                                         "Select z-axis",
                                         choices = qb_clean_numeric_col_names,
                                         selected = "EPA"),
                             rglwidgetOutput("multi_scatter_plot", width = 800, height = 600)
                         ),
                         box(width = 8,
                             rglwidgetOutput("multi_scatter_plot", width = "100%", height = 600)
                         )
                       )
               )
             )
      )
    )
  )
)




# Main panel with tabs for different plots









#qb pressure sensitivity
#QB performance with high YAC games
#3d scatterplot
#box plot for each ind. var and 3 dep var