#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define server logic required to draw a histogram
function(input, output, session) {
  
  qb_comparison <- reactive({
    qb_clean |> 
      filter(year %in% seq(input$year_range[1], input$year_range[2], step = 1),
             between(total_rush_yds, input$max_rush_yds[1], input$max_rush_yds[2]),
             sacks_per_dropback >= input$min_pressure_rate,
             if (input$high_rush) high_rushing else TRUE) |> 
      
      group_by(passer_player_name) |> 
      summarize(actual_qb_anya = mean(any_a),
                predicted_qb_anya = mean(predicted_anya),
                actual_qb_epa = mean(qb_epa),
                predicted_qb_epa = mean(predicted_epa),
                actual_qb_passer_rating = mean(passer_rating),
                predicted_qb_passer_rating = mean(predicted_passer_rating),
                num_games = n(),
                avg_salary_year = mean(avg_year),
                retired = first(retired))
  })
  
  filtered_qbs <- reactive({
    qb_clean |> 
      filter(year %in% seq(input$year_range[1], input$year_range[2], by = 1),
             total_rush_yds <= input$max_rush_yds[2] & total_rush_yds >= input$max_rush_yds[1],
             sacks_per_dropback >= input$min_pressure_rate
      ) |> 
      group_by(passer_player_name) |> 
      filter(if (input$active_players) retired == FALSE else TRUE) |> 
      ungroup() 
  })
  
  filtered_qbs_input <- reactive({
    filtered_qbs() |> 
      distinct(passer_player_name) |> 
      pull(passer_player_name) |> 
      sort()
  })
  
  filtered_qb_comparison <- reactive({
    qb_comparison() |> 
      group_by(passer_player_name) |> 
      filter(passer_player_name %in% filtered_qbs_input()) |> 
      ungroup()
  })
  
  filtered_qb_clean <- reactive({
    qb_clean |> 
      filter(passer_player_name %in% filtered_qbs_input())
  })
  
  filtered_qb_comparison_grouped <- reactive({
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach & year %in% seq(input$year_range[1], input$year_range[2], by = 1),
             total_rush_yds <= input$max_rush_yds[2] & total_rush_yds >= input$max_rush_yds[1],
             sacks_per_dropback >= input$min_pressure_rate) |> 
      pull(passer_player_name)
    filtered_qb_comparison() |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
        passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
        passer_player_name %in% coached_qbs ~ glue("{input$coach}'s QBs"),
        TRUE ~ "Other QBs"
      ))
  })
  
  scatter_pressure_data <- reactive({
    
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    
    filtered_qbs() |> 
      rename(epa = qb_epa,
             anya = any_a) |> 
      mutate(coached_group = if_else(passer_player_name %in% coached_qbs,
                                     glue("{input$coach}'s QBs"),
                                     "Other QBs")) |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
        passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
        TRUE ~ coached_group
      )) |> 
      mutate(Group = factor(Group, levels = c("Other QBs", glue("{input$coach}'s QBs"), "Top 10 Highest Paid QBs", glue("{input$qb_name}"), "Brock Purdy")))
  })
  
  confidence_scatter_pressure <- reactive({
    scatter_pressure_data() |> 
      group_by(Group) |> 
      summarize(pressure_mean = compute_confidence_interval(.data[["sacks_per_dropback"]])[1],
                pressure_low = compute_confidence_interval(.data[["sacks_per_dropback"]])[2],
                pressure_high = compute_confidence_interval(.data[["sacks_per_dropback"]])[3],
                
                epa_low = compute_confidence_interval(.data[['epa']])[2],
                epa_high = compute_confidence_interval(.data[['epa']])[3],
                epa = compute_confidence_interval(.data[['epa']])[1],
                
                anya_low = compute_confidence_interval(.data[['anya']])[2],
                anya_high = compute_confidence_interval(.data[['anya']])[3],
                anya = compute_confidence_interval(.data[['anya']])[1],
                
                passer_rating_low = compute_confidence_interval(.data[['passer_rating']])[2],
                passer_rating_high = compute_confidence_interval(.data[['passer_rating']])[3],
                passer_rating = compute_confidence_interval(.data[['passer_rating']])[1]
      )
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
  
  group_color_assignments <- reactive({
    setNames(c("#8C1515", "#1F497D", "#5F9EA0", "#CBB677", "#D9D9D9"),
             c("Brock Purdy", glue("{input$qb_name}"), "Top 10 Highest Paid QBs", glue("{input$coach}'s QBs"), "Other QBs"))
  })
  
  heavy_light_group_color_assignments <- reactive({
    setNames(c("#8C1515", "#5E0F0F",
               "#1F497D", "#162F55",
               "#5F9EA0", "#4682B4",
               "#CBB677", "#B8860B",
               "#D9D9D9", "#808080"),
             c("Brock Purdy (Light Rush)", "Brock Purdy (Heavy Rush)",
               glue("{input$qb_name} (Light Rush)"), glue("{input$qb_name} (Heavy Rush)"),
               "Top 10 Highest Paid QBs (Light Rush)", "Top 10 Highest Paid QBs (Heavy Rush)",
               glue("{input$coach}'s QBs (Light Rush)"), glue("{input$coach}'s QBs (Heavy Rush)"),
               "Other QBs (Light Rush)", "Other QBs (Heavy Rush)")
    )
  })
  
  output$stat_explanation <- renderText({
    explanations <- list(
      "ANY/A" = "Adjusted Net Yards per Attempt (ANY/A) factors in passing yards, touchdowns, interceptions, and sacks.",
      "EPA" = "Expected Points Added (EPA) measures the impact of each play on the team's chances of scoring.",
      "Passer Rating" = "Passer Rating is a traditional QB efficiency metric based on completions, yards, touchdowns, and interceptions."
    )
    explanations[[input$dependent_var]]
  })
  
  output$qb_error <- renderText({
    if (!is.null(input$qb_name) & !(input$qb_name %in% filtered_qbs_input())) {
      return(glue("Selected QB '{input$qb_name}' did not play in the selected year(s)."))
    }
    return(NULL)
  })
  
  output$pressureBoxPlot <- renderPlotly({
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    
    scatter_data <-  filtered_qb_clean() |> 
      rename(epa = qb_epa,
             anya = any_a) |> 
      mutate(coached_group = if_else(passer_player_name %in% coached_qbs,
                                     glue("{input$coach}'s QBs"),
                                     "Other QBs")) |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
        passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
        TRUE ~ coached_group
      )) |> 
      mutate(Group = factor(Group, levels = rev(c("Other QBs", glue("{input$coach}'s QBs"), "Top 10 Highest Paid QBs", glue("{input$qb_name}"), "Brock Purdy"))))
    
    p <- ggplot(scatter_data, aes(x = Group, y = sacks_per_dropback, fill = Group)) + 
      geom_violin(aes(fill = Group), alpha = .4, draw_quantiles = c(.25, .5, .75)) +
      geom_boxplot(aes(fill = Group), width = .6, alpha = .7) + 
      geom_jitter(aes(color = Group,
                      text = paste0("QB: ", passer_player_name, "<br>",
                                    "Pressure: ", round(sacks_per_dropback, 2), " sacks/dropback", "<br>",
                                    input$dependent_var, ": ", round(.data[[paste0(dependent_var)]], 2))),
                  width = .2, alpha = .6, size = .2) + 
      ggtitle("Distribution of Pressure by QB Group") + 
      xlab("Quarterback Group") + 
      ylab("Sacks per Dropback") + 
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 15, hjust = 1, size = 12)
      ) +
      scale_color_manual(values = group_color_assignments()) +
      scale_fill_manual(values = group_color_assignments())
    
    ggplotly(p, tooltip = 'text') |> 
      layout(height = 600)
  }) 
  
  output$runBoxPlot <- renderPlotly({
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    
    scatter_data <-  filtered_qb_clean() |> 
      rename(epa = qb_epa,
             anya = any_a) |> 
      mutate(coached_group = if_else(passer_player_name %in% coached_qbs,
                                     glue("{input$coach}'s QBs"),
                                     "Other QBs")) |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
        passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
        TRUE ~ coached_group
      )) |> 
      mutate(Group = factor(Group, levels = rev(c("Other QBs", glue("{input$coach}'s QBs"), "Top 10 Highest Paid QBs", glue("{input$qb_name}"), "Brock Purdy"))))
    
    p <- ggplot(scatter_data, aes(x = Group, y = total_rush_yds, fill = Group)) + 
      geom_violin(aes(fill = Group), alpha = .4, draw_quantiles = c(.25, .5, .75)) +
      geom_boxplot(aes(fill = Group), width = .6, alpha = .7) + 
      geom_jitter(aes(color = Group,
                      text = paste0("QB: ", passer_player_name, "<br>",
                                    "Rush Yards: ", total_rush_yds, "<br>",
                                    input$dependent_var, ": ", round(.data[[paste0(dependent_var)]], 2))),
                  width = .2, alpha = .6, size = .2) + 
      ggtitle("Distribution of Rushing Yards by QB Group") + 
      xlab("Quarterback Group") + 
      ylab("Rushing Yards a Game") + 
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 15, hjust = 1, size = 12)
      ) +
      scale_color_manual(values = group_color_assignments()) +
      scale_fill_manual(values = group_color_assignments())
    
    ggplotly(p, tooltip = 'text') |> 
      layout(height = 700)
  })
  
  
  output$qbcompareboxplot <- renderPlotly({
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    p <- ggplot(data = filtered_qb_comparison_grouped() |> mutate(Group = factor(Group, levels = c("Brock Purdy", glue("{input$qb_name}"), "Top 10 Highest Paid QBs", glue("{input$coach}'s QBs"), "Other QBs"))),
                aes(x = Group, y = .data[[paste0("actual_qb_", dependent_var)]]))+
      ylab(glue("Career {input$dependent_var}")) +
      xlab("Quarterback Group") +
      geom_boxplot(aes(fill = Group), width = .6, alpha = .7) + 
      geom_jitter(aes(color = Group,
                      text = paste0("QB: ", passer_player_name, "<br>",
                                    "Average ", input$dependent_var, ": ", round(.data[[paste0("actual_qb_", dependent_var)]], 2))),
                  width = .2, alpha = .6, size = .2) + 
      ggtitle(glue("Boxplot of Career {input$dependent_var}")) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 15, hjust = 1, size = 12)
      ) +
      scale_color_manual(values = group_color_assignments()) +
      scale_fill_manual(values = group_color_assignments()) +
      coord_cartesian(ylim = c(min(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE),
                               max(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE)))
    
    ggplotly(p, tooltip = "text") |> 
      layout(height = 700)
  })
  
  
  
  output$scatter_rush <- renderPlotly({
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    confidence_scatter_rush <- scatter_pressure_data() |> 
      group_by(Group) |> 
      summarize(rush_mean = compute_confidence_interval(.data[["total_rush_yds"]])[1],
                rush_low = compute_confidence_interval(.data[["total_rush_yds"]])[2],
                rush_high = compute_confidence_interval(.data[["total_rush_yds"]])[3],
                
                epa_low = compute_confidence_interval(.data[['epa']])[2],
                epa_high = compute_confidence_interval(.data[['epa']])[3],
                epa = compute_confidence_interval(.data[['epa']])[1],
                
                anya_low = compute_confidence_interval(.data[['anya']])[2],
                anya_high = compute_confidence_interval(.data[['anya']])[3],
                anya = compute_confidence_interval(.data[['anya']])[1],
                
                passer_rating_low = compute_confidence_interval(.data[['passer_rating']])[2],
                passer_rating_high = compute_confidence_interval(.data[['passer_rating']])[3],
                passer_rating = compute_confidence_interval(.data[['passer_rating']])[1]
      )
    
    rush_low <- confidence_scatter_rush$rush_low
    rush_high <- confidence_scatter_rush$rush_high
    actual_low <- confidence_scatter_rush[[paste0(dependent_var, "_low")]]
    actual_high <- confidence_scatter_rush[[paste0(dependent_var, "_high")]]
    
    
    if (input$average_cluster) {
      qb_clean <- qb_clean |> 
        rename(epa = qb_epa,
               anya = any_a)
      
      p <- ggplot(data = confidence_scatter_rush, aes(x = rush_mean, y = .data[[dependent_var]], 
                                                      color = Group,
                                                      text = paste("Group:", Group, "<br>",
                                                                   "Average Rushing Yards:", round(rush_mean, 2), "<br>",
                                                                   glue("Average {input$dependent_var}:"), round(.data[[dependent_var]], 4))),
                  size = .1) + 
        geom_point(alpha = .5) +
        scale_color_manual(values = group_color_assignments()) +
        ggtitle(glue("{input$dependent_var} vs. Rushing Yards")) +
        xlab("Rushing Yards") +
        ylab(input$dependent_var) +
        geom_point(alpha = .7, size = 1) +
        geom_errorbar(aes(ymin = actual_low, ymax = actual_high)) +
        geom_errorbarh(aes(xmin = rush_low, xmax = rush_high)) +
        coord_cartesian(xlim = c(0, 400),
                        ylim = c(min(qb_clean[[paste0(dependent_var)]], na.rm = TRUE),
                                 max(qb_clean[[paste0(dependent_var)]], na.rm = TRUE)))
    }
    else if (input$rush_lines) {
      qb_clean <- qb_clean |> 
        rename(epa = qb_epa,
               anya = any_a)
      p <- ggplot(data = scatter_pressure_data(), aes(x = total_rush_yds, y = .data[[dependent_var]], 
                                                      color = Group,
                                                      text = paste("QB:", passer_player_name)),
                  size = .1) + 
        scale_color_manual(values = group_color_assignments()) +
        scale_fill_manual(values = group_color_assignments()) +
        ggtitle(glue("{input$dependent_var} vs. Rushing Yards")) +
        xlab("Rushing Yards") +
        ylab(input$dependent_var) +
        geom_smooth(method = "lm", aes(group = Group, fill = Group), linetype = "dashed", alpha = .3) +
        # Add R^2 and equations of each line
        coord_cartesian(xlim = c(0, 400),
                        ylim = c(min(qb_clean[[paste0(dependent_var)]], na.rm = TRUE),
                                 max(qb_clean[[paste0(dependent_var)]], na.rm = TRUE))) 
    }
    
    else {
      qb_clean <- qb_clean |> 
        rename(epa = qb_epa,
               anya = any_a)
      p <- ggplot(data = scatter_pressure_data(), aes(x = total_rush_yds, y = .data[[dependent_var]], 
                                                      color = Group,
                                                      text = paste("QB:", passer_player_name)),
                  size = .1) + 
        geom_point(alpha = .5) +
        scale_color_manual(values = group_color_assignments()) +
        ggtitle(glue("{input$dependent_var} vs. Rushing Yards")) +
        xlab("Rushing Yards") +
        ylab(input$dependent_var) +
        coord_cartesian(xlim = c(0, 400),
                        ylim = c(min(qb_clean[[paste0(dependent_var)]], na.rm = TRUE),
                                 max(qb_clean[[paste0(dependent_var)]], na.rm = TRUE))) 
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$heavy_light_rush_boxplot <- renderPlotly({
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    cutoff <- input$heavy_light_slider
    
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    
    heavy_light_df <- filtered_qbs() |> 
      rename(epa = qb_epa,
             anya = any_a) |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" & total_rush_yds > cutoff ~ "Brock Purdy (Heavy Rush)",
        passer_player_name == "B.Purdy" & total_rush_yds <= cutoff ~ "Brock Purdy (Light Rush)",
        passer_player_name == input$qb_name & total_rush_yds > cutoff ~ glue("{input$qb_name} (Heavy Rush)"),
        passer_player_name == input$qb_name & total_rush_yds <= cutoff ~ glue("{input$qb_name} (Light Rush)"),
        passer_player_name %in% ten_highest_paid_qbs_2024 & total_rush_yds > cutoff ~ "Top 10 Highest Paid QBs (Heavy Rush)",
        passer_player_name %in% ten_highest_paid_qbs_2024 & total_rush_yds <= cutoff ~ "Top 10 Highest Paid QBs (Light Rush)",
        passer_player_name %in% coached_qbs & total_rush_yds > cutoff ~ glue("{input$coach}'s QBs (Heavy Rush)"),
        passer_player_name %in% coached_qbs & total_rush_yds <= cutoff ~ glue("{input$coach}'s QBs (Light Rush)"),
        total_rush_yds > cutoff ~ "Other QBs (Heavy Rush)",
        TRUE ~ "Other QBs (Light Rush)"
      )) |> 
      mutate(Group = factor(Group, levels = c("Brock Purdy (Light Rush)", "Brock Purdy (Heavy Rush)",
                                              glue("{input$qb_name} (Light Rush)"), glue("{input$qb_name} (Heavy Rush)"), 
                                              "Top 10 Highest Paid QBs (Light Rush)", "Top 10 Highest Paid QBs (Heavy Rush)", 
                                              glue("{input$coach}'s QBs (Light Rush)"), glue("{input$coach}'s QBs (Heavy Rush)"), 
                                              "Other QBs (Light Rush)", "Other QBs (Heavy Rush)"))) 
    
    p <- ggplot(heavy_light_df, aes(x = Group, y = .data[[dependent_var]])) +
      geom_boxplot(aes(fill = Group), width = .5, alpha = .7, outlier.shape = NA) + 
      geom_jitter(aes(color = Group,
                      text = paste0("QB: ", passer_player_name, "<br>",
                                    input$dependent_var, ": ", round(.data[[dependent_var]], 2), "<br>",
                                    "Rushing Yards: ", total_rush_yds)),
                  width = .3, alpha = .6, size = .2) + 
      ggtitle(glue("{input$dependent_var} for Light and Heavy Rush Games")) +
      ylab(glue("{input$dependent_var}")) +
      xlab("Quarterback Group") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_blank()
      ) +
      scale_x_discrete(expand = c(.2,.2)) +
      scale_color_manual(values = heavy_light_group_color_assignments()) +
      scale_fill_manual(values = heavy_light_group_color_assignments())
    
    ggplotly(p, tooltip = "text") |> 
      layout(legend = list(orientation = "h", x=.5, xanchor = "center"))
    
  })
  
  output$scatter_pressure <- renderPlotly({
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    scatter_pressure_data <- scatter_pressure_data()
    confidence_scatter_pressure <- confidence_scatter_pressure()
    
    pressure_low <- confidence_scatter_pressure$pressure_low
    pressure_high <- confidence_scatter_pressure$pressure_high
    actual_low <- confidence_scatter_pressure[[paste0(dependent_var, "_low")]]
    actual_high <- confidence_scatter_pressure[[paste0(dependent_var, "_high")]]
    
    
    if (input$average_cluster) {
      
      
      p <- ggplot(data = confidence_scatter_pressure, aes(x = pressure_mean, y = .data[[dependent_var]], 
                                                          color = Group,
                                                          text = paste("Group:", Group, "<br>",
                                                                       "Average Sacks per Dropback:", round(pressure_mean, 4), "<br>",
                                                                       glue("Average {input$dependent_var}:"), round(.data[[dependent_var]], 4))),
                  size = .1) + 
        geom_point(alpha = .5) +
        scale_color_manual(values = group_color_assignments()) +
        ggtitle(glue("Pressure Sensitivity")) +
        xlab("Sacks per Dropback") +
        ylab(input$dependent_var) +
        geom_point(alpha = .7, size = 1) +
        geom_errorbar(aes(ymin = actual_low, ymax = actual_high)) +
        geom_errorbarh(aes(xmin = pressure_low, xmax = pressure_high)) +
        coord_cartesian(xlim = c(min(qb_all_years_comp[["sacks_per_dropback"]], na.rm = TRUE),
                                 max(qb_all_years_comp[["sacks_per_dropback"]], na.rm = TRUE)),
                        ylim = c(min(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE),
                                 max(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE))) 
    }
    else {
      qb_clean <- qb_clean |> 
        rename(epa = qb_epa,
               anya = any_a)
      p <- ggplot(data = scatter_pressure_data, aes(x = sacks_per_dropback, y = .data[[dependent_var]], 
                                                    color = Group,
                                                    text = paste("QB:", passer_player_name)),
                  size = .1) + 
        geom_point(alpha = .5) +
        scale_color_manual(values = group_color_assignments()) +
        ggtitle(glue("Pressure Sensitivity")) +
        xlab("Sacks per Dropback") +
        ylab(input$dependent_var) +
        coord_cartesian(xlim = c(0, .45),
                        ylim = c(min(qb_clean[[paste0(dependent_var)]], na.rm = TRUE),
                                 max(qb_clean[[paste0(dependent_var)]], na.rm = TRUE))) 
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$average_pressure_line <- renderPlotly({
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    
    get_stat_summary <- function(threshold, stat_col){
      coached_qbs <- qb_clean |> 
        filter(coach == input$coach) |> 
        pull(passer_player_name)
      
      filtered_threshold_data <- qb_clean |> 
        filter(year %in% seq(input$year_range[1], input$year_range[2], by = 1),
               total_rush_yds <= input$max_rush_yds[2] & total_rush_yds >= input$max_rush_yds[1],
               sacks_per_dropback >= threshold
        ) |> 
        group_by(passer_player_name) |> 
        filter(if (input$active_players) retired == FALSE else TRUE) |> 
        ungroup() |> 
        rename(epa = qb_epa,
               anya = any_a)|> 
        mutate(coached_group = if_else(passer_player_name %in% coached_qbs,
                                       glue("{input$coach}'s QBs"),
                                       "Other QBs")) |> 
        mutate(Group = case_when(
          passer_player_name == "B.Purdy" ~ "Brock Purdy",
          passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
          passer_player_name %in% ten_highest_paid_qbs_2024 ~ "Top 10 Highest Paid QBs",
          TRUE ~ coached_group
        )) 
      
      filtered_threshold_data |> 
        group_by(Group) |> 
        summarize(
          min_pressure_rate = threshold,
          mean_stat = compute_confidence_interval(.data[[stat_col]])[1],
          low_stat = compute_confidence_interval(.data[[stat_col]])[2],
          high_stat = compute_confidence_interval(.data[[stat_col]])[3],
          Group = first(Group),
          .groups = "drop"
        )
      
    }
    
    thresholds <- seq(0, .13, by = .01)
    stat_summary_thresholds <- map_dfr(thresholds, ~ get_stat_summary(.x, stat_col = dependent_var))
    
    p <- ggplot(stat_summary_thresholds |> 
                  mutate(Group = factor(Group, levels = c("Brock Purdy", glue("{input$qb_name}"), "Top 10 Highest Paid QBs", glue("{input$coach}'s QBs"), "Other QBs"))), 
                aes(x = min_pressure_rate, y = mean_stat, color = Group, fill = Group), 
                alpha = .5,
                text = paste("Group:", Group, "<br>",
                             "Mean", input$dependent_var, ":", mean_stat)) +
      geom_line() + 
      geom_ribbon(aes(ymin = low_stat, ymax = high_stat, fill = Group), alpha = .1) +
      scale_color_manual(values = group_color_assignments()) +
      scale_fill_manual(values = group_color_assignments()) +
      theme_minimal() +
      ggtitle("Average Pressure Sensitivity") +
      xlab("Minimum Sacks per Dropback") +
      ylab(glue("Average {input$dependent_var}"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_model_actual <- renderPlotly({
    
    dependent_var <- dependent_var_hashmap[input$dependent_var]
    print(dependent_var)
    
    x_vals = filtered_qb_comparison_grouped()[[paste0("predicted_qb_", dependent_var)]]
    y_vals = filtered_qb_comparison_grouped()[[paste0("actual_qb_", dependent_var)]]
    
    confidence_vals_x <- compute_confidence_interval(x_vals)
    print(confidence_vals_x)
    confidence_vals_y <- compute_confidence_interval(y_vals)
    print(confidence_vals_y)
    
    
    if (input$average_cluster){
      clustered_data <- filtered_qb_comparison_grouped() |> 
        group_by(Group) |> 
        summarize(pred_epa_low = compute_confidence_interval(.data[['predicted_qb_epa']])[2],
                  pred_epa_high = compute_confidence_interval(.data[['predicted_qb_epa']])[3],
                  predicted_qb_epa = compute_confidence_interval(.data[['predicted_qb_epa']])[1],
                  
                  actual_epa_low = compute_confidence_interval(.data[['actual_qb_epa']])[2],
                  actual_epa_high = compute_confidence_interval(.data[['actual_qb_epa']])[3],
                  actual_qb_epa = compute_confidence_interval(.data[['actual_qb_epa']])[1],
                  
                  pred_anya_low = compute_confidence_interval(.data[['predicted_qb_anya']])[2],
                  pred_anya_high = compute_confidence_interval(.data[['predicted_qb_anya']])[3],
                  predicted_qb_anya = mean(predicted_qb_anya),
                  
                  actual_anya_low = compute_confidence_interval(.data[['actual_qb_anya']])[2],
                  actual_anya_high = compute_confidence_interval(.data[['actual_qb_anya']])[3],
                  actual_qb_anya = mean(actual_qb_anya),
                  
                  pred_passer_rating_low = compute_confidence_interval(.data[['predicted_qb_passer_rating']])[2],
                  pred_passer_rating_high = compute_confidence_interval(.data[['predicted_qb_passer_rating']])[3],
                  predicted_qb_passer_rating = mean(predicted_qb_passer_rating),
                  
                  actual_passer_rating_low = compute_confidence_interval(.data[['actual_qb_passer_rating']])[2],
                  actual_passer_rating_high = compute_confidence_interval(.data[['actual_qb_passer_rating']])[3],
                  actual_qb_passer_rating = mean(actual_qb_passer_rating)
        )
      pred_low <- clustered_data[[paste0("pred_", dependent_var, "_low")]]
      pred_high <- clustered_data[[paste0("pred_", dependent_var, "_high")]]
      actual_low <- clustered_data[[paste0("actual_", dependent_var, "_low")]]
      actual_high <- clustered_data[[paste0("actual_", dependent_var, "_high")]]
      
      p <- ggplot(data = clustered_data, aes(
        x=.data[[paste0("predicted_qb_", dependent_var)]],
        y=.data[[paste0("actual_qb_", dependent_var)]],
        color = Group,
        text = paste(glue("Average of {Group}"), "<br>",
                     glue("Predicted {input$dependent_var}"), round(.data[[paste0("predicted_qb_", dependent_var)]], digits = 4), "<br>",
                     glue("Actual {input$dependent_var}:"), round(.data[[paste0("actual_qb_", dependent_var)]], digits = 4)
        ))) +
        ggtitle(glue("Actual vs. Predicted {input$dependent_var}")) +
        xlab(glue("Predicted {input$dependent_var}")) +
        ylab(glue("Actual {input$dependent_var}")) +
        geom_abline(a=0, b=1, linetype = 'dashed') + 
        geom_point(alpha = .7, size = 1) +
        geom_errorbar(aes(ymin = actual_low, ymax = actual_high)) +
        geom_errorbarh(aes(xmin = pred_low, xmax = pred_high)) +
        scale_color_manual(values = group_color_assignments()) +
        coord_cartesian(xlim = c(min(qb_all_years_comp[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE),
                                 max(qb_all_years_comp[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE)),
                        ylim = c(min(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE),
                                 max(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE))) +
        geom_text(data = filtered_qb_comparison_grouped() |> filter(passer_player_name %in% c("B.Purdy", input$qb_name)), 
                  aes(label = passer_player_name), nudge_y = .05)
      
      
      
    }
    else {
      p <- ggplot(data = filtered_qb_comparison_grouped(), aes(
        x=.data[[paste0("predicted_qb_", dependent_var)]],
        y=.data[[paste0("actual_qb_", dependent_var)]],
        color = Group,
        text = ifelse(!is.na(avg_salary_year),
                      paste("QB:", passer_player_name, "<br>",
                            "APY Salary (2025):", dollar(avg_salary_year), "<br>",
                            glue("Games played since 1999:"), total_games_played[passer_player_name], "<br>", # try to add ability to count num games played since year in year_range selector
                            glue("Predicted {input$dependent_var}:"), round(.data[[paste0("predicted_qb_", dependent_var)]], digits = 4), "<br>",
                            glue("Actual {input$dependent_var}:"), round(.data[[paste0("actual_qb_", dependent_var)]], digits = 4)),
                      paste("QB:", passer_player_name, "<br>",
                            "Retired", "<br>",
                            "Games played since 1999:", total_games_played[passer_player_name], "<br>",
                            glue("Predicted {input$dependent_var}:"), round(.data[[paste0("predicted_qb_", dependent_var)]], digits = 4), "<br>",
                            glue("Actual {input$dependent_var}:"), round(.data[[paste0("actual_qb_", dependent_var)]], digits = 4))),
        customdata = passer_player_name
      )) +
        ggtitle(glue("Actual vs. Predicted {input$dependent_var}")) +
        xlab(glue("Predicted {input$dependent_var}")) +
        ylab(glue("Actual {input$dependent_var}")) +
        geom_abline(a=0, b=1, linetype = 'dashed') + 
        geom_point(alpha = .7, size = 1) +
        scale_color_manual(values = group_color_assignments()) +
        coord_cartesian(xlim = c(min(qb_all_years_comp[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE),
                                 max(qb_all_years_comp[[paste0("predicted_qb_", dependent_var)]], na.rm = TRUE)),
                        ylim = c(min(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE),
                                 max(qb_all_years_comp[[paste0("actual_qb_", dependent_var)]], na.rm = TRUE))) +
        geom_text(data = filtered_qb_comparison_grouped() |> filter(passer_player_name %in% c("B.Purdy", input$qb_name)), 
                  aes(label = passer_player_name), nudge_y = .05)
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  selected_qb <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    if (!is.null(event)) {
      selected_qb(event$customdata)
    }
  })
  
  output$qb_games_table <- renderDT({
    req(selected_qb())
    qb_games <- qb_clean |> 
      filter(passer_player_name == selected_qb())
    datatable(qb_games, 
              caption = glue("{selected_qb()} Data Table"),
              options = list(pageLength = 10),
              selection = 'single')
  })
  
  selected_game <- reactive({
    req(selected_qb(), input$qb_games_table_rows_selected)
    qb_game <- qb_clean |> filter(passer_player_name == selected_qb())
    
    qb_game[input$qb_games_table_rows_selected, ]
  })
  
  output$multi_scatter_plot <- renderRglwidget({
    # https://rpubs.com/pjozefek/576206
    
    # use plotly to create surface plot??
    
    coached_qbs <- qb_clean |> 
      filter(coach == input$coach) |> 
      pull(passer_player_name)
    filtered_qb_data <- filtered_qb_clean() |> 
      mutate(Group = case_when(
        passer_player_name == "B.Purdy" ~ "Brock Purdy",
        passer_player_name == input$qb_name ~ glue("{input$qb_name}"),
        passer_player_name %in% coached_qbs ~ glue("{input$coach}'s QBs"),
        TRUE ~ "Other QBs"
      ))
    
    req(input$x_axis, input$y_axis , input$z_axis)
    rgl.open(useNULL = TRUE)
    input_x = qb_clean_col_map[input$x_axis]
    input_y = qb_clean_col_map[input$y_axis]
    input_z = qb_clean_col_map[input$z_axis]
    
    x <- filtered_qb_data[[input_x]] 
    y <- filtered_qb_data[[input_z]]
    z <- filtered_qb_data[[input_y]]
    
    x_range <- range(qb_clean[[input_x]], na.rm = TRUE)
    y_range <- range(qb_clean[[input_z]], na.rm = TRUE)
    z_range <- range(qb_clean[[input_y]], na.rm = TRUE)
    
    bg3d(color = 'white')
    col_values <-   group_color_assignments()
    size_values <- setNames(c(3, 3, 1, 1, .8),
                            c("Brock Purdy", glue("{input$qb_name}"), glue("{input$coach}'s QBs"), "Other QBs"))
    qb_colors <- col_values[filtered_qb_data$Group]
    qb_sizes <- size_values[filtered_qb_data$Group]
    
    model <- lm(z ~ x + y, data = filtered_qb_data)
    x_seq <- seq(x_range[1], x_range[2], length.out = 20)
    y_seq <- seq(y_range[1], y_range[2], length.out = 20)
    grid <- expand.grid(x=x_seq, y=y_seq)
    grid$z <- predict(model, newdata = grid)
    
    par3d(windowRect = c(100, 100, 900, 600))
    plot3d(x, y, z,
           type = 'p',
           col = qb_colors,
           xlab = input$x_axis, ylab = input$z_axis, zlab = input$y_axis,
           xlim = x_range, ylim = y_range, zlim = z_range)
    
    
    surface3d(unique(grid$x), unique(grid$y), matrix(grid$z, nrow = 20),
              color = 'grey', alpha = .5)
    
    legend3d("topright", legend = names(col_values), col = col_values, pch = 16, cex = 1.2, inset = c(0, 0))
    view3d(theta = -30, phi = 30, fov = 0)
    rglwidget()
  })
  
  observeEvent(input$reset_input, {
    updateSliderInput(session, "year_range", value = c(1999, 2024))
    updateSelectInput(session, "coach", selected = "Kyle Shanahan")
    updateCheckboxInput(session, "active_players", value = FALSE)
    updateSelectInput(session, "qb_name", selected = "B.Purdy")
    updateSelectInput(session, "dependent_var", selected = "EPA")
    updateSliderInput(session, "max_rush_yds", value = c(0, 400))
    updateSliderInput(session, "min_pressure_rate", value = 0)
    updateCheckboxInput(session, "average_cluser", value = FALSE)
  })
}