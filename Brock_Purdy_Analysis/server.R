#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

get_quartiles <- function (col_name){
  quantile(qb_game_data$col_name, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
}

# Define server logic required to draw a histogram
function(input, output, session) {

    output$plot_model_actual <- renderPlot({

      ggplot(data = qb_comparison, aes(x=predicted_qb_passer_rating, 
                                       y=actual_qb_passer_rating, 
                                       color = passer_player_name == input$qb_name, 
                                       alpha = .5)) +
        geom_abline(a=0, b=1) + 
        geom_point()
      
    })

    output$epa_pr_Plot <- renderPlot({
      
      qb_game_data |> 
        ggplot(aes(x=qb_epa, y=passer_rating, color=passer_player_name=="B.Purdy")) +
        geom_point()
      
    })
    
    output$anya_epa_Plot <- renderPlot({
      
      qb_game_data |> 
        ggplot(aes(x=any.a, y=qb_epa, color=passer_player_name=="B.Purdy")) +
        geom_vline(xintercept = quantile(qb_game_data$any.a, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) +
        geom_hline(yintercept = quantile(qb_game_data$qb_epa, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) + 
        geom_point(alpha = .2)
      
    })
}
