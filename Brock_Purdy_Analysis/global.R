library(tidyverse)
library(glue)

qb_game_data = read.csv("../data/qb_game_data.csv")

get_qb_data <- function (qb_name){
  qb_game_data |> 
    filter(passer_player_name == qb_name)
}

get_quartiles <- function (col_name){
  quantile(qb_game_data$col_name, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
}

purdy_data <- get_qb_data("B.Purdy")
garoppolo_data <- get_qb_data("J.Garoppolo")

ten_highest_paid_qbs_2024 = c('D.Prescott', 'J.Burrow', 'J.Love', 'T.Lawrence', 'T.Tagovailoa', 'J.Goff', 'J.Herbert', 'L.Jackson', 'J.Hurts', 'K.Murray')
top_ten_paid_qbs <- qb_game_data |> 
  filter(passer_player_name %in% ten_highest_paid_qbs_2024)
