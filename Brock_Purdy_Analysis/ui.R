#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application
dashboardPage(
  skin = "red",
  
  # Application title
  dashboardHeader(title = "Brock Purdy Analysis"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Overall Performance vs. Other QBs", tabName = "performance", icon = icon("balance-scale")),
      menuItem("High-Pressure Situations", tabName = "pressure", icon = icon("chart-line")),
      menuItem("High-Rushing Games", tabName = "rushing", icon = icon("running")),
      #menuItem("Short Passing Games", tabName = "shortpass", icon = icon("football-ball")),
      menuItem("Predictive Model Analysis", tabName = "model", icon = icon("cogs")),
      menuItem("Exploratory Playground", tabName = "exploratory", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    #tags$head(
    #  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #),
    
    fluidRow(
      
      column(width = 3,
             wellPanel(
               h4("Filters & Settings"),
               shinyjs::useShinyjs(),
               
               selectInput("dependent_var",
                           "Select Metric",
                           choices = c("EPA", "ANY/A", "Passer Rating")),
               
               textOutput("stat_explanation"),
               
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
               
               sliderInput("max_rush_yds",
                           "Games With Rushing Yards Between:",
                           min = 0, max = 400, value = c(0, 400), step = 10,
                           animate = TRUE),
               
               sliderInput("min_pressure_rate",
                           "Select Minimum Sacks per Dropback",
                           min = 0, max = .2, value = 0, step = .01, 
                           animate = TRUE),
               
               textOutput("high_rush_explanation"),
               
               textOutput('qb_error'),
               
               actionButton("reset_input", "Reset")
             )
      ),
      column(width = 9,
             tabItems(
               tabItem(tabName = "home",
                       h2("Home Page"),
                       tabsetPanel(
                         tabPanel("Introduction",
                                  h2("Introduction"),
                                  fluidRow(
                                    column(4, img(src = "brockpurdy_warmup.webp", height = "300px")
                                    ),
                                    column(4, img(src = "brockpurdy_nfc.webp", height = "300px")
                                    )
                                  ),
                                  p("Brock Purdy’s rapid rise from the final pick of the 2022 NFL Draft to leading the 49ers to deep playoff runs in his first two seasons was nothing short of remarkable. However, after a challenging 2024 season marked by injuries to key teammates and a decline in performance, questions arise about how much of his success is due to his individual talent versus the strength of the elite system around him."),
                                  h2("Motivation"),
                                  p("Brock Purdy has had an impressive start to his NFL career with the San Francisco 49ers. Selected very last in the 2022 NFL draft, Purdy had the opportunity to start for the 49ers his rookie year when their veteran quarterback Jimmy Garoppolo and first round pick Trey Lance both became injured. Purdy shot off to an impressive 5-0 in his first five games as a starter, and his success continued as Purdy brought the 49ers to the NFC championship in 2022, where he was forced off the field with an elbow injury and the 49ers ended up losing to the Philadelphia Eagles. The following season (2023) was just as impressive for Purdy, taking the 49ers all the way to the Super Bowl where San Fransisco lost to the Kansas City Chiefs by 3 points in overtime. Purdy’s first two seasons are undeniably impressive. However, the 2024 season proved challenging for Purdy, as his completion percentage, passer rating, and TD-INT ratio all declined amidst a year marked by injuries to key offensive players, most notably running back Christian McCaffrey. The 49ers ended their 2024 season with Purdy’s worst record yet, finishing 6-11."),
                                  p(strong("Purdy’s early success followed by a decline raises critical questions: To what extent is his performance a reflection of his own skill versus the strength of the team around him?")),
                                  p("Purdy benefits from an elite supporting cast, each playing a crucial role in his success:"),
                                  fluidRow(
                                    column(6, tags$li("Trent Williams – Left Tackle (LT): The anchor of the offensive line, responsible for protecting Purdy’s blindside and giving him the time needed to make accurate throws.")
                                           
                                    ), 
                                    column(4, img(src = "trentwilliams.webp", height = "100px"))
                                  ),
                                  fluidRow(
                                    column(6, tags$li("George Kittle – Tight End (TE): A versatile player who excels in both blocking and receiving, providing a reliable target for short and intermediate passes while also contributing to pass protection.")
                                    ),
                                    column(4, img(src = "georgekittle.jpg", height = "100px")
                                    )
                                  ),
                                  fluidRow(
                                    column(6, tags$li("Deebo Samuel & Brandon Aiyuk – Wide Receivers (WR): Dynamic playmakers who create separation, make tough catches, and extend plays after the catch, giving Purdy multiple downfield threats.")
                                    ),
                                    column(4, img(src = "deeboandbrandon.webp", height = "100px")
                                    )
                                  ),
                                  fluidRow(
                                    column(6, tags$li("Christian McCaffrey – Running Back (RB): A dual-threat weapon who not only excels in rushing but also serves as a dependable receiving option, easing pressure on Purdy by keeping defenses off balance.")
                                    ),
                                    column(4, img(src = "christianmccaffrey.webp", height = "100px")
                                    )
                                  ),
                                  p("Purdy is further supported by his head coach, Kyle Shanahan, who is known for his elite offensive schemes."),
                                  img(src = "kyleshanahan.webp", height = "300px"),
                                  p(strong("Brock Purdy's rookie contract will expire next season. The 49ers have made it clear they intend to significantly increase their quarterback’s compensation this offseason, raising his current average salary of $934,252 to $50 million or more. This would place Purdy among the highest-paid quarterbacks in the NFL, but does his skill really justify such a contract?"))
                         ),
                         tabPanel("Key Metrics",
                                  h2("Key Analysis Metrics"),
                                  p("To evaluate Brock Purdy's skill as a quarterback, we examine three metrics:"),
                                  h4("1. Expected Points Added (EPA)"),
                                  img(src = "expectedpoints.png", height = "300px"),
                                  p("Expected Points Added (EPA) per play is a metric that quantifies the impact of each play on a team’s likelihood of scoring. The values for Expected Points (see figure above) were taken from historical data by calculating the average number of points scored by the possessing team according to each down and field position. EPA per play is simply the difference in Expected Points before and after a play. Instead of just measuring yardage, EPA accounts for game context, such as downs, distance to a first down, and field position, to determine how much a play increases or decreases a team’s expected points. For example, a 5-yard gain on a third-and-4 is much more valuable than a 5-yard gain on a third-and-10, as it results in a first down and extends the drive. EPA is an effective metric for measuring quarterback skill because it captures a quarterback’s ability to make plays that meaningfully contribute to scoring."),
                                  h4("2. Adjusted Net Yards per Attempt (ANY/A)"),
                                  p("Adjusted Net Yards per Attempt (ANY/A) is an advanced passing efficiency metric that improves on traditional yards per attempt by incorporating touchdowns, interceptions, and sacks, thus providing a more comprehensive measure of a quarterback’s effectiveness in the passing game. ANY/A can be a valuable tool for comparing performance across different players and seasons."),
                                  h4("3. Passer Rating"),
                                  p("Passer rating is calculated based on completions, passing yards, touchdowns, and interceptions per attempt, generating a score between 0 and 158.3 that is designed to summarize a quarterback’s effectiveness. A higher score indicates better performance. While passer rating is useful for comparing QBs within a season, it has limitations as it does not account for sacks, rushing ability, or game context."),
                                  p(strong("Combining ANY/A, EPA, and Passer Rating provides a well-rounded approach to evaluating quarterback performance, capturing efficiency (ANY/A), impact on scoring (EPA), and traditional passing effectiveness (Passer Rating)."))
                         ),
                         tabPanel("Conclusion",
                                  h2("Conclusion"),
                                  p("This app aims to give insight into Brock Purdy's true skill apart from the elite support surrounding him. This is, of course, a quite challenging thing to do. To approach this question, we examine the aforementioned quarterback metrics for Brock Purdy in high-pressure situations and heavy vs. light rushing games. In addition, we created models for each metric designed to predict a quartterback's EPA, ANY/A, or Passer Rating depending on the following variables: Yards After the Catch (YAC), Percentage of Passing Yards from YAC, Sacks per Dropback, Short Pass Attempts, Percentage of Short Passes, and Total Rushing Yards. An elite quarterback would be thought to outperform their predicted metric, on average. Finally, we compare Purdy to other quarterbacks coached by Kyle Shanahan to see how Shanahan's offensive playmaking factors into Purdy's metrics. We also compare Purd to the top ten highest paid quarterbacks per year (as of 2024), to evaluate where he compares to these play-makers.")
                         )
                       )
                       
                       
                       
                       
               ),
               tabItem(tabName = "performance", 
                       h2("Evaluating Purdy's Overall Performance vs. Other QBs"),
                       checkboxInput("high_rush",
                                     "Inspect Games with Over 165 Rushing Yards",
                                     FALSE),
                       fluidRow(
                         box(width = 12, plotlyOutput("qbcompareboxplot", height = 700))
                       )
               ),
               tabItem(tabName = "pressure",
                       h2("Purdy under High-Pressure Situations"),
                       
                       tabsetPanel(
                         tabPanel("Distribution of Pressure",
                                  fluidRow(
                                    box(width = 12, plotlyOutput("pressureBoxPlot", height = 600))
                                  )
                         ),
                         tabPanel("Pressure's Influence on Metric Outcomes",
                                  fluidRow(
                                    box(width = 12, plotlyOutput("scatter_pressure", height = 400))
                                  ),
                                  checkboxInput("average_cluster",
                                                "Average of Each Group (with 95% confidence intervals)",
                                                FALSE),
                                  fluidRow(
                                    box(width = 12, plotlyOutput("average_pressure_line", height = 400))
                                  )
                         )
                       )
               ),
               tabItem(tabName = "rushing",
                       h2("Purdy's Performance in High-Rushing Games"),
                       
                       tabsetPanel(
                         tabPanel("Distribution of Rushing Yards per Game",
                                  fluidRow(
                                    box(width = 12, plotlyOutput("runBoxPlot", height = 700))
                                  )
                         ),
                         tabPanel("Rushing's Influence on Metric Outcomes",
                                  fluidRow(
                                    box(width = 12, plotlyOutput("scatter_rush", height = 400))
                                  ),
                                  checkboxInput("rush_lines",
                                                "Generate Regression Lines",
                                                FALSE),
                                  checkboxInput("average_cluster",
                                                "Average of Each Group (with 95% confidence intervals)",
                                                FALSE),
                                  fluidRow(
                                    box(width = 12, plotlyOutput("heavy_light_rush_boxplot", height = 700))
                                  ),
                                  sliderInput("heavy_light_slider",
                                              "Select Light-Heavy Rushing Yardage Cutoff",
                                              min = 80, max = 165, value = 113, step = 1)
                         )
                       )
                       
                       
                       
               ),
               #tabItem(tabName = "shortpass",
               #        h2("Purdy's Performance in Short Passing Games")
               #),
               tabItem(tabName = "model",
                       h2("Predictive Model Analysis"),
                       fluidRow(
                         box(width = 12, plotlyOutput("plot_model_actual", height = 500))
                       ),
                       p("Click on a quarterback to see a table of all their games."),
                       checkboxInput("average_cluster",
                                     "Average of each Group (with 95% confidence intervals)",
                                     FALSE),
                       fluidRow(
                         box(width = 12, div(style = "overflow-x: auto;", DTOutput("qb_games_table")))
                       )
               ),
               tabItem(tabName = "exploratory",
                       h2("Exploratory Playground"),
                       fluidRow(
                         box(width = 4,
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
                         ),
                         box(width = 8,
                             rglwidgetOutput("multi_scatter_plot", width = "100%", height = 600)
                         )
                       )
               )
             )
      )
    )
  )
)




# Main panel with tabs for different plots









#qb pressure sensitivity
#QB performance with high YAC games
#3d scatterplot
#box plot for each ind. var and 3 dep var