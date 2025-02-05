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
  
  filtered_qbs_input <- reactive({
    qb_clean |> 
      group_by(passer_player_name) |> 
      filter(n() >= input$min_games) |> 
      ungroup() |> 
      distinct(passer_player_name) |> 
      pull(passer_player_name) |> 
      sort()
  })
  
  qb_initialized <- reactiveVal(FALSE)
  
  observe({
    available_qbs <- filtered_qbs_input()
    if (!qb_initialized()) {
    
    updateSelectInput(session, "qb_name", 
                      choices = available_qbs,
                      selected = if ("B.Purdy" %in% available_qbs) "B.Purdy" else NULL)
      qb_initialized(TRUE)
    }
    else {
      updateSelectInput(session, "qb_name", 
                        choices = available_qbs,
                        selected = if (input$qb_name %in% available_qbs) input$qb_name else input$qb_name)
    }
  })
  
  output$qb_error <- renderText({
    if (!is.null(input$qb_name) & !(input$qb_name %in% filtered_qbs_input())) {
      return(glue("Selected QB '{input$qb_name}' has not played the required minimum number of games. Please select a different QB or lower the required minimum number of games played."))
    }
    return(NULL)
  })
  
  filtered_qb_comparison <- reactive({
    qb_comparison |> 
      filter(num_games >= input$min_games)
  })
  
  filtered_qb_comparison_grouped <- reactive({
    filtered_qb_comparison() |> 
    mutate(group = case_when(
      passer_player_name == "B.Purdy" ~ "Brock Purdy",
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
      scale_color_manual(values = c("Brock Purdy" = "red", "Selected QB" = "green", "Top Paid QBs" = "blue", "Other QBs" = 'grey')) +
      geom_label_repel(data = filtered_qb_comparison_grouped() |> filter(passer_player_name == "B.Purdy"), aes(label = passer_player_name), nudge_y = .2) +
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
