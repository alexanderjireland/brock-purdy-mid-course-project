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
                  selected = "B.Purdy")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model vs. Reality", plotOutput("plot_model_actual")),
      )
    )
  )
)
