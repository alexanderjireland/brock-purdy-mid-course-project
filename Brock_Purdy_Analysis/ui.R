#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
      
      checkboxInput("average_cluster",
                    "Average of each Cluster",
                    FALSE),
      
      selectInput("dependent_var",
                  "Select Statistic",
                  choices = c("EPA", "ANY/A", "Passer Rating")),
      
     sliderInput("max_rush_yds",
                 "Games With Rushing Yards Fewer Than:",
                 min = 0, max = 250, value = c(0, 250), step = 10,
                 animate = TRUE),
      
      textOutput("stat_explanation"),
      
      textOutput('qb_error'),
      
      actionButton("reset_input", "Reset"),
      
      tags$hr()
    ),
    
    # Main panel with tabs for different plots
    mainPanel(
      tabsetPanel(
        tabPanel("Model vs. Reality", plotlyOutput("plot_model_actual"),
                 DTOutput("qb_games_table")),
        
        tabPanel("3D Plot", 
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