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
      filter(n() >= input$min_games & !(input$active_players & retired)) |> 
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
      group_by(passer_player_name) |> 
      filter(num_games >= input$min_games & !(input$active_players & retired)) |> 
      ungroup()
  })
  
  filtered_qb_clean <- reactive({
    qb_clean |> 
      filter(passer_player_name %in% filtered_qbs_input())
  })
  
  filtered_qb_comparison_grouped <- reactive({
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    filtered_qb_comparison() |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ "Selected QB",
        passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
        passer_player_name %in% coached_qbs ~ glue("{input$coach}'s QBs"),
        TRUE ~ "Other QBs"
      ))
  })
  
  output$plot_model_actual <- renderPlotly({
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    p <- ggplot(data = filtered_qb_comparison_grouped(), aes(
      x=.data[[paste0("predicted_qb_", dependent_var)]], 
      y=.data[[paste0("actual_qb_", dependent_var)]], 
      color = Group,
      text = ifelse(!is.na(avg_salary_year),
                    paste("QB:", passer_player_name, "<br>",
                          "APY Salary (2025):", dollar(avg_salary_year)),
                    paste("QB:", passer_player_name, "<br>",
                          "Retired"))
    )) +
      ggtitle(glue("Actual vs. Predicted {input$dependent_var}")) +
      xlab(glue("Predicted {input$dependent_var}")) +
      ylab(glue("Actual {input$dependent_var}")) +
      geom_abline(a=0, b=1, linetype = 'dashed') + 
      geom_point(alpha = .7, size = 1) +
      scale_color_manual(values = setNames(c("red", "springgreen4", "blue", "orange", "grey"),
                                           c("Brock Purdy", "Selected QB", "Top 10 Highest Paid QBs", glue("{input$coach}'s QBs"), "Other QBs"))) +
      coord_cartesian(xlim = c(min(qb_comparison[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE),
                               max(qb_comparison[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE)),
                      ylim = c(min(qb_comparison[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE),
                               max(qb_comparison[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE))) +
      geom_text(data = filtered_qb_comparison_grouped() |> filter(passer_player_name %in% c("B.Purdy", input$qb_name)), 
                aes(label = passer_player_name), nudge_y = .05)
    
    ggplotly(p, tooltip = "text") 
  })
  
  
  output$multi_scatter_plot <- renderRglwidget({
    # https://rpubs.com/pjozefek/576206
    
    # use plotly to create surface plot??
    
    filtered_qb_data <- filtered_qb_clean()
    req(input$x_axis, input$y_axis , input$z_axis)
    rgl.open(useNULL = TRUE)
    
    x <- filtered_qb_data[[input$x_axis]] 
    y <- filtered_qb_data[[input$y_axis]]
    z <- filtered_qb_data[[input$z_axis]]
    
    x_range <- range(qb_clean[[input$x_axis]], na.rm = TRUE)
    y_range <- range(qb_clean[[input$y_axis]], na.rm = TRUE)
    z_range <- range(qb_clean[[input$z_axis]], na.rm = TRUE)
    
    bg3d(color = 'white')
    qb_colors <- rainbow(length(unique(filtered_qb_data$passer_player_name)))[as.factor(filtered_qb_data$passer_player_name)]
    
    model <- lm(z ~ x + y, data = filtered_qb_data)
    x_seq <- seq(x_range[1], x_range[2], length.out = 20)
    y_seq <- seq(y_range[1], y_range[2], length.out = 20)
    grid <- expand.grid(x=x_seq, y=y_seq)
    grid$z <- predict(model, newdata = grid)
    
    plot3d(x, y, z,
           col=qb_colors,
           type = 'p',
           size = 3,
           xlab = input$x_axis, ylab = input$y_axis, zlab = input$z_axis,
           xlim = x_range, ylim = y_range, zlim = z_range)
    
    surface3d(unique(grid$x), unique(grid$y), matrix(grid$z, nrow = 20),
              color = 'red', alpha = .5)
    
    rglwidget()
  })
}


