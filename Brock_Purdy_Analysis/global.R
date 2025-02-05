library(tidyverse)
library(glue)
library(ggrepel)
library(plotly)
library(scales)
library(rgl)

qb_game_data = read.csv("../data/qb_game_data.csv")
qb_salary = read.csv("../data/QB_Salary.csv")
coach_data = read.csv("../data/all_coach_data.csv")
qb_salary <- qb_salary |> 
  mutate(passer_player_name = paste0(substr(sub(" .*", "", Player), 1, 1), ".", sub(".* ", "", Player))) |> 
  select(passer_player_name, Avg..Year)

qb_game_data <- qb_game_data |> 
  left_join(qb_salary)

names(qb_game_data)[names(qb_game_data) == 'Avg..Year'] <- 'avg_year'
names(qb_game_data)[names(qb_game_data) == 'posteam'] <- 'team'

qb_game_data <- qb_game_data |> 
  mutate(team = str_replace_all(team, "\\bLA\\b", 'LAR'),
         retired = is.na(avg_year))

merge_teamnames_map <- c("GNB" = "GB",
                         "KAN" = "KC",
                         "NOR" = "NO",
                         "NWE" = "NE",
                         "OAK" = "LV",
                         "LVR" = "LV", 
                         "SDG" = "LAC",
                         "SFO" = "SF",
                         "STL" = "LAR",
                         "TAM" = "TB")
coach_data$team <- ifelse(coach_data$team %in% names(merge_teamnames_map), merge_teamnames_map[coach_data$team], coach_data$team)

qb_game_data <- qb_game_data |> 
  merge(coach_data, by = c("year", "team")) |> 
  select(-X.x, -X.y, -rush_epa)

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
    select(avg_year, coach, team, home_team, away_team, year, passer_player_name, any_a, qb_epa, passer_rating, yards_after_catch, percent_pass_yds_from_yac, sacks_per_dropback, short_pass, percent_short_pass, team_rush_epa, retired) |> 
    mutate_if(is.numeric, ~ replace(., !is.finite(.), NA)) |> 
    mutate(avg_year = as.numeric((gsub(",","", sub(".", "", avg_year)))))

qb_clean_numeric_cols <- qb_clean |> 
  select(where(is.numeric)) |> 
  names()

create_dependent_var_model <- function (dependent_var) {
  if (dependent_var == 'passer_rating') {
    lm(as.formula(glue("{dependent_var} ~ yards_after_catch + percent_pass_yds_from_yac + sacks_per_dropback + short_pass + percent_short_pass + team_rush_epa")), data = qb_clean)
  }
  else {
    lm(as.formula(glue("{dependent_var} ~ yards_after_catch + percent_pass_yds_from_yac + sacks_per_dropback + short_pass + percent_short_pass")), data = qb_clean)
  }
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
         num_games = n(),
         avg_salary_year = mean(avg_year),
         retired = first(retired))

keys <- c("EPA", "ANY/A", "Passer Rating")
values <- c("epa", "anya", "passer_rating")
dependent_var_hashmap <- setNames(values, keys)

max_games <- qb_clean |> 
  group_by(passer_player_name) |> 
  summarize(num_games = n()) |> 
  summarize(max_games = max(num_games)) |> 
  pull(max_games)