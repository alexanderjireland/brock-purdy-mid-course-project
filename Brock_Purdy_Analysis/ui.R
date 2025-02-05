#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Brock Purdy Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("min_games",
                  "Minimum Number of Games Played",
                  min = 10, max = 100, value = 10),
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
    
    #work on creating a selection for dependent var
    
    #select coach
    #select with and without rb
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model vs. Reality", plotlyOutput("plot_model_actual")),
        tabPanel("3D Plot", rglwidgetOutput("multi_scatter_plot", width = 800, height = 600))
      )
    )
  )
)


#qb pressure sensitivity
#QB performance with high YAC games
#3d scatterplot
#box plot for each ind. var and 3 dep var