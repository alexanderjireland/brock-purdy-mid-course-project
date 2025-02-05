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
  
  filtered_qb_comparison <- reactive({
    qb_comparison |> 
      filter(num_games >= input$min_games)
  })
  
  filtered_qb_comparison_grouped <- reactive({
    filtered_qb_comparison() |> 
    mutate(group = case_when(
      passer_player_name == input$qb_name ~ "Selected QB",
      passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top Paid QBs",
      TRUE ~ "Other QBs"
    ))
  })
  
  output$plot_model_actual <- renderPlot({
    

    
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    ggplot(data = filtered_qb_comparison_grouped(), aes(
      x=.data[[paste0("predicted_qb_", dependent_var)]], 
      y=.data[[paste0("actual_qb_", dependent_var)]], 
      color = group
    )) +
      ggtitle(glue("Actual vs. Predicted {input$dependent_var}")) +
      xlab(glue("Predicted {input$dependent_var}")) +
      ylab(glue("Actual {input$dependent_var}")) +
      geom_abline(a=0, b=1, linetype = 'dashed') + 
      geom_point(aes(alpha = .7), size = 1) +
      scale_color_manual(values = c("Selected QB" = "red", "Top Paid QBs" = "blue", "Other QBs" = 'grey')) +
      geom_label_repel(data = filtered_qb_comparison_grouped() |> filter(passer_player_name == input$qb_name), aes(label = passer_player_name), nudge_y = .2)
    
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
