library(tidyverse)
library(glue)

qb_game_data = read.csv("../data/qb_game_data.csv")

get_qb_data <- function (qb_name){
  qb_game_data |> 
    filter(passer_player_name == qb_name)
}

get_quartiles <- function (col_name){
  quantile(qb_game_data[[col_name]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
}

purdy_data <- get_qb_data("B.Purdy")
garoppolo_data <- get_qb_data("J.Garoppolo")

ten_highest_paid_qbs_2024 = c('D.Prescott', 'J.Burrow', 'J.Love', 'T.Lawrence', 'T.Tagovailoa', 'J.Goff', 'J.Herbert', 'L.Jackson', 'J.Hurts', 'K.Murray')
top_ten_paid_qbs <- qb_game_data |> 
  filter(passer_player_name %in% ten_highest_paid_qbs_2024)

qb_clean <- qb_game_data |> 
    select(passer_player_name, any_a, qb_epa, passer_rating, yards_after_catch, percent_pass_yds_from_yac, sacks_per_dropback, short_pass, percent_short_pass) |> 
    mutate_if(is.numeric, ~ replace(., !is.finite(.), NA)) |> 
    na.omit()

create_dependent_var_model <- function (dependent_var) {
    lm(as.formula(glue("{dependent_var} ~ yards_after_catch + percent_pass_yds_from_yac + sacks_per_dropback + short_pass + percent_short_pass")), data = qb_clean) 
  # working on rush_epa kinks, should add to lm
}

anya_model <- create_dependent_var_model('any_a')
epa_model <- create_dependent_var_model('qb_epa')
pr_model <- create_dependent_var_model('passer_rating')
qb_clean <- qb_clean |> 
  mutate(predicted_anya = predict(anya_model, newdata = qb_clean),
         predicted_epa = predict(epa_model, newdata = qb_clean),
         predicted_passer_rating = predict(pr_model, newdata = qb_clean)
         )


qb_comparison <- qb_clean |> 
  group_by(passer_player_name) |> 
  summarize(actual_qb_anya = mean(any_a),
         predicted_qb_anya = mean(predicted_anya),
         actual_qb_epa = mean(qb_epa),
         predicted_qb_epa = mean(predicted_epa),
         actual_qb_passer_rating = mean(passer_rating),
         predicted_qb_passer_rating = mean(predicted_passer_rating),
         num_games = n())


#ggplot(data = qb_comparison, aes(x=predicted_qb_epa, y=actual_qb_epa, color = passer_player_name == "B.Purdy")) +
  #geom_abline(a=0, b=1) + 
  #geom_point()
#ggplot(data = qb_comparison, aes(x=predicted_qb_anya, y=actual_qb_anya, color = passer_player_name == "B.Purdy", alpha = .3)) +
  #geom_abline(a=0, b=1) + 
  #geom_point()
#ggplot(data = qb_comparison, aes(x=predicted_qb_passer_rating, y=actual_qb_passer_rating, color = passer_player_name == "B.Purdy", alpha = .3)) +
  #geom_abline(a=0, b=1) + 
  #geom_point()