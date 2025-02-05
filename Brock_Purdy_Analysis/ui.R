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
      selectInput("qb_name",
                  "Select Quarterback",
                  choices = qb_clean |> distinct(passer_player_name) |> pull(passer_player_name) |> sort(),
                  selected = "B.Purdy"),
      selectInput("dependent_var",
                  "Select Dependent Variable",
                  choices = c("EPA", "ANY/A", "Passer Rating")),
      sliderInput("min_games",
                  "Select Minimum Number of Games Played",
                  min = 10, max = 100, value = 10)
    ),
    
    #work on creating a selection for dependent var
    
    #select coach
    #select with and without rb
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model vs. Reality", plotOutput("plot_model_actual"))
        
      )
    )
  )
)


#qb pressure sensitivity
#QB performance with high YAC games
#3d scatterplot
#box plot for each ind. var and 3 dep var