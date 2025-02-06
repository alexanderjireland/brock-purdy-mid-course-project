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
  
  # Application title
  titlePanel("Brock Purdy Analysis"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      sliderInput("min_games",
                  "Minimum Number of Games Played",
                  min = 10, max = 100, value = 10),
      
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
                  "Select Dependent Variable",
                  choices = c("EPA", "ANY/A", "Passer Rating")),
      
      textOutput('qb_error')
    ),
    
    # Main panel with tabs for different plots
    mainPanel(
      tabsetPanel(
        tabPanel("Model vs. Reality", plotlyOutput("plot_model_actual")),
        
        tabPanel("3D Plot", 
                 selectInput('x_axis',
                             "Select x-axis",
                             choices = qb_clean_numeric_cols),
                 selectInput('y_axis',
                             "Select y-axis",
                             choices = qb_clean_numeric_cols),
                 selectInput('z_axis',
                             "Select z-axis",
                             choices = qb_clean_numeric_cols),
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