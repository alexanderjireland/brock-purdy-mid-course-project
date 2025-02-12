#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application
fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Brock Purdy Analysis"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      id = 'side-panel',
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
                  "Select Statistic",
                  choices = c("EPA", "ANY/A", "Passer Rating")),
      
      sliderInput("max_rush_yds",
                  "Games With Rushing Yards Between:",
                  min = 0, max = 400, value = c(0, 400), step = 10,
                  animate = TRUE),
      
      checkboxInput("high_rush",
                    "High Rushing Support",
                    FALSE),
      
      checkboxInput("average_cluster",
                    "Average of each Cluster",
                    FALSE),
      
      sliderInput("min_pressure_rate",
                  "Select Minimum Sacks per Dropback",
                  min = 0, max = .45, value = 0, step = .01, 
                  animate = TRUE),
      
      textOutput("high_rush_explanation"),
      
      textOutput("stat_explanation"),
      
      textOutput('qb_error'),
      
      actionButton("reset_input", "Reset"),
      
      tags$hr()
    ),
    
    # Main panel with tabs for different plots
    mainPanel(
      tabsetPanel(
        type = 'pills',
        
        tabPanel("Home Page"),
        tabPanel("Purdy under High-Pressure Situations",
                 plotlyOutput("scatter_pressure")),
        tabPanel("Purdy's Performance in High-Rushing Games", 
                 plotlyOutput("runBoxPlot")),
        tabPanel("Purdy's Performance in Short Passing Games"),
        tabPanel("Evaluating Purdy's Overall Performance vs. Other QBs",
                 plotlyOutput("boxplot")),
        tabPanel("Predictive Model Analysis", 
                 plotlyOutput("plot_model_actual"),
                 DTOutput("qb_games_table")),
        tabPanel("Exploratory Playground", 
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
        )
      )
    )
  )
)



#qb pressure sensitivity
#QB performance with high YAC games
#3d scatterplot
#box plot for each ind. var and 3 dep var