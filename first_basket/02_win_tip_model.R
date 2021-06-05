library(tidyverse)
library(lubridate)
library(data.table)
library(reshape2)
library(scales)
library(recipes)
library(ranger)
library(pROC)
library(glmnet)
library(earth)

# Parameters

# Example - for full run where you use all history and start the test data at 2019-2020 season
# earliest_train_data_date <- "2017-09-01"
# train_test_date_split <- "2018-09-01"

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2017-09-01"
# Setting date we want to start logging test data on
# Default uses today's date so that all completed games are used in calculating ratings
train_test_date_split <- "2018-09-01"

model_iterative <- fread("data/02_curated/nba_first_to_score/model_iterative.csv.gz")

# Read in all player csvs
file_list <- list.files(path="./data/nba_player_info/")
player_info_df <- tibble()

for (i in 1:length(file_list)){
  # Read in file for each player
  temp_player <- read.csv(paste0("./data/nba_player_info/", file_list[i]))
  # Bind the new data to the dataset
  player_info_df <- rbind.data.frame(player_info_df, temp_player)
}

# Reads in list of teams to join on team id
team_list <-
  fread("./data/01_raw/nba_teams/current.csv") %>%
  select(TEAM_ID, ABBREVIATION) %>%
  mutate(TEAM_ID = as.character(TEAM_ID))

# Relevant fields for players
# Changing height to inches
player_info_mutated <-
  player_info_df %>%
  separate(HEIGHT, c("feet", "inches"), remove = FALSE) %>%
  mutate(PERSON_ID = as.character(PERSON_ID),
         feet = as.integer(feet),
         inches = as.integer(inches)) %>%
  mutate(HEIGHT = (feet*12)+inches) %>%
  select(PERSON_ID, HEIGHT, WEIGHT)

possession_binded <- fread("data/02_curated/nba_first_to_score/jump_ball_dataset.csv.gz",
                           colClasses = c('game_date' = 'Date',
                                          'game_id' = 'character',
                                          'home_team_person_id' = 'character', 
                                          'away_team_person_id' = 'character'))

# Adds in player height and weight
possession_player_info <-
  possession_binded %>%
  left_join(player_info_mutated, by = c("home_team_person_id" = "PERSON_ID")) %>%
  rename(home_team_height = HEIGHT, home_team_weight = WEIGHT) %>%
  left_join(player_info_mutated, by = c("away_team_person_id" = "PERSON_ID")) %>%
  rename(away_team_height = HEIGHT, away_team_weight = WEIGHT) %>%
  mutate(home_height_diff = home_team_height - away_team_height,
         home_weight_diff = home_team_weight - away_team_weight) %>%
  filter(!is.na(home_team_height),
         !is.na(away_team_height))

possession_home <-
  possession_player_info %>%
  select(season, game_date, game_id, matchup, period, period_clock,
         person_id = home_team_person_id, jumper = home_team_jumper, 
         height = home_team_height, weight = home_team_weight, team_abbrev = home_team_abbrev,
         opp_person_id = away_team_person_id, opp_jumper = away_team_jumper, 
         opp_height = away_team_height, opp_weight = away_team_weight, opp_team_abbrev = away_team_abbrev,
         opening_jump, center_court, possession, height_diff = home_height_diff, weight_diff = home_weight_diff, 
         won_tip = home_won_tip, score_first = home_score_first) %>%
  mutate(home = TRUE)
  
possession_away <-
  possession_player_info %>%
  select(season, game_date, game_id, matchup, period, period_clock,
         person_id = away_team_person_id, jumper = away_team_jumper, 
         height = away_team_height, weight = away_team_weight, team_abbrev = away_team_abbrev,
         opp_person_id = home_team_person_id, opp_jumper = home_team_jumper, 
         opp_height = home_team_height, opp_weight = home_team_weight, opp_team_abbrev = home_team_abbrev,
         opening_jump, center_court, possession, height_diff = home_height_diff, weight_diff = home_weight_diff, 
         won_tip = home_won_tip, score_first = home_score_first) %>%
  mutate(home = FALSE) %>%
  mutate(height_diff = -height_diff,
         weight_diff = -weight_diff,
         won_tip = !won_tip,
         score_first = !score_first)

possession_df <- rbind.data.frame(possession_home, possession_away)

rollmean_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    mean,
    .before = p,
    ...
  )
}

possession_df_vars <-
  possession_df %>%
  arrange(jumper, game_date, period, desc(period_clock)) %>%
  group_by(jumper) %>%
  mutate(jumps = replace_na(lag(row_number()), 0),
         all_jumps_avg = lag(cummean(won_tip)),
         last_10_avg = lag(rollmean_p(won_tip, 9, .complete = FALSE)),
         last_25_avg = lag(rollmean_p(won_tip, 24, .complete = FALSE)),
         last_50_avg = lag(rollmean_p(won_tip, 49, .complete = FALSE)),
         last_75_avg = lag(rollmean_p(won_tip, 74, .complete = FALSE)),
         last_100_avg = lag(rollmean_p(won_tip, 99, .complete = FALSE))) %>%
  group_by(jumper, season) %>%
  mutate(season_jumps = replace_na(lag(row_number()), 0),
         season_all_jumps_avg = lag(cummean(won_tip)),
         season_last_10_avg = lag(rollmean_p(won_tip, 9, .complete = FALSE)),
         season_last_25_avg = lag(rollmean_p(won_tip, 24, .complete = FALSE)),
         season_last_50_avg = lag(rollmean_p(won_tip, 49, .complete = FALSE))) %>%
  arrange(opp_jumper, game_date, period, desc(period_clock)) %>%
  group_by(opp_jumper) %>%
  mutate(opp_jumps = replace_na(lag(row_number()), 0),
         opp_all_jumps_avg = lag(cummean(!won_tip)),
         opp_last_10_avg = lag(rollmean_p(!won_tip, 9, .complete = FALSE)),
         opp_last_25_avg = lag(rollmean_p(!won_tip, 24, .complete = FALSE)),
         opp_last_50_avg = lag(rollmean_p(!won_tip, 49, .complete = FALSE)),
         opp_last_75_avg = lag(rollmean_p(!won_tip, 74, .complete = FALSE)),
         opp_last_100_avg = lag(rollmean_p(!won_tip, 99, .complete = FALSE))) %>%
  group_by(opp_jumper, season) %>%
  mutate(opp_season_jumps = replace_na(lag(row_number()), 0),
         opp_season_all_jumps_avg = lag(cummean(!won_tip)),
         opp_season_last_10_avg = lag(rollmean_p(!won_tip, 9, .complete = FALSE)),
         opp_season_last_25_avg = lag(rollmean_p(!won_tip, 24, .complete = FALSE)),
         opp_season_last_50_avg = lag(rollmean_p(!won_tip, 49, .complete = FALSE))) %>%
  ungroup() %>%
  mutate(won_tip = as.factor(won_tip))

### Adding data from other models
model_iterative_split <-
  model_iterative %>%
  select(season, game_date, game_id, matchup, opening_jump,
         person_id = home_team_person_id, 
         team_abbrev = home_team_abbrev,
         opp_person_id = away_team_person_id, 
         opp_team_abbrev = away_team_abbrev, 
         iterative_rating = home_exp_win_adj, 
         opp_iterative_rating = away_exp_win_adj,
         iterative_win_tip = final_exp_win_adj) %>%
  bind_rows(
    model_iterative %>%
    select(season, game_date, game_id, matchup, opening_jump,
           person_id = away_team_person_id, 
           team_abbrev = away_team_abbrev,
           opp_person_id = home_team_person_id, 
           opp_team_abbrev = home_team_abbrev,
           iterative_rating = away_exp_win_adj, 
           opp_iterative_rating = home_exp_win_adj,
           iterative_win_tip = final_exp_win_adj) %>%
    mutate(iterative_win_tip = 1 - iterative_win_tip)) %>%
  mutate(game_id = as.character(game_id),
         game_date = as.Date(game_date),
         person_id = as.character(person_id),
         opp_person_id = as.character(opp_person_id))

possession_models_joined <-
  possession_df_vars %>%
  mutate(game_date = as.Date(game_date),
         game_id = as.character(as.integer(game_id)),
         person_id = as.character(as.integer(person_id)),
         opp_person_id = as.character(as.integer(opp_person_id)),
         opening_jump = as.logical(opening_jump),
         center_court = as.logical(center_court)) %>%
  left_join(model_iterative_split) %>%
  distinct()

################# Day by Day Run #####################
possession_relevant_dates <-
  possession_models_joined %>%
  filter(game_date >= earliest_train_data_date,
         !is.na(iterative_rating),
         !is.na(opp_iterative_rating))

# Set of dates to loop through for test data
unique_dates <-
  possession_relevant_dates %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  filter(game_date >= train_test_date_split)

possession_relevant_cols <-
  possession_relevant_dates %>%
  select(-c(person_id, team_abbrev, opp_person_id, opp_team_abbrev, possession, score_first))

function_to_run_models <- function(possession_train_model, possession_test_model, model_desc){
  
  set.seed(060321)
  
  ## subset predictors, outcomes, extras
  outcome_vars <- "won_tip"
  game_info_vars <- c("season", "game_date", "game_id", "matchup", "period", "period_clock", "jumper", "opp_jumper")
  predictor_vars <- colnames(subset(possession_train_model, select = !names(possession_train_model) %in% c(game_info_vars, outcome_vars)))
  
  predictors_train <- 
    possession_train_model %>%
    select(any_of(predictor_vars))
  
  predictors_test <-
    possession_test_model %>%
    select(any_of(predictor_vars))
  
  outcomes_train <- 
    possession_train_model %>%
    select(any_of(outcome_vars))
  
  outcomes_test <- 
    possession_test_model %>%
    select(any_of(outcome_vars))
  
  game_info_train <-
    possession_train_model %>%
    select(any_of(game_info_vars))
  
  game_info_test <-
    possession_test_model %>%
    select(any_of(game_info_vars))
  
  # model_ranger <- ranger(x = predictors_train, y = outcomes_train$won_tip, 
  #                        probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)
  # 
  # preds_ranger <- 
  #   cbind.data.frame(game_info_test,
  #                    predictors_test, 
  #                    outcomes_test,
  #                    predict(model_ranger, predictors_test, type = 'response')$predictions) %>%
  #   rename(win_tip_prob_ranger = `TRUE`) %>%
  #   select(-c(`FALSE`)) %>%
  #   mutate(won_tip = as.logical(won_tip))
  
  model_glmnet <- cv.glmnet(x = as.matrix(predictors_train), y = outcomes_train$won_tip, family = 'binomial')
  
  preds_glmnet <- 
    cbind.data.frame(game_info_test,
                     predictors_test, 
                     outcomes_test,
                     win_tip_prob_glmnet = as.numeric(predict(model_glmnet, as.matrix(predictors_test), type = 'response'))) %>%
    mutate(won_tip = as.logical(won_tip))
  
  model_earth <- earth(x = predictors_train, y = outcomes_train$won_tip, degree = 2, glm = list(family = binomial), nfold = 3)
  
  preds_earth <- 
    cbind.data.frame(game_info_test,
                     predictors_test, 
                     outcomes_test,
                     predict(model_earth, predictors_test, type = 'response')) %>%
    rename(win_tip_prob_earth = `TRUE`) %>%
    mutate(won_tip = as.logical(won_tip))
  
  preds <-
    preds_glmnet %>%
    select(season, game_date, game_id, matchup, period, period_clock, jumper, opp_jumper, home,
           won_tip, iterative_win_tip, win_tip_prob_glmnet) %>%
    cbind.data.frame(win_tip_prob_earth = preds_earth$win_tip_prob_earth,
                     model = model_desc)
  
  return(preds)
  
}

# If no values in unique_dates (i.e. doing current day run instead of historical testing)
# Set unique_dates to current date so loop runs once to get player ratings
if (nrow(unique_dates) == 0) {
  unique_dates <- tibble(game_date = Sys.Date())
}

# Initializing df that stores test data predictions
test_df_exp_win_master <- tibble()

for (i in 1:length(unique_dates$game_date)) {
  message(unique_dates$game_date[i])
  
  # Splitting data into train and test sets
  possession_train <-
    possession_relevant_cols %>%
    filter(game_date < unique_dates$game_date[i])

  possession_test <-
    possession_relevant_cols %>%
    filter(game_date == unique_dates$game_date[i],
           opening_jump == TRUE) 

  # When jumper has no jumps and opponent has jumps, but not this season
  count_no_jumps_opp_jumps_not_this_season <- sum(possession_test$jumps == 0 & possession_test$opp_jumps > 0 & possession_test$opp_season_jumps == 0)
  # When jumper has no jumps and opponent has jumps this season
  count_no_jumps_opp_jumps_this_season <- sum(possession_test$jumps == 0 & possession_test$opp_season_jumps > 0)
  # When jumper and opponent have no jumps
  count_no_jumps_opp_no_jumps <- sum(possession_test$jumps == 0 & possession_test$opp_jumps == 0)
  
  # When jumper has jumps, but not this season and opponent has no jumps
  count_jumps_not_this_season_opp_no_jumps <- sum(possession_test$jumps > 0 & possession_test$season_jumps == 0 & possession_test$opp_jumps == 0)
  # When jumper and opponent have jumps, but not this season
  count_jumps_not_this_season_opp_jumps_not_this_season <- sum(possession_test$jumps > 0 & possession_test$season_jumps == 0 & 
                                                                  possession_test$opp_jumps > 0 & possession_test$opp_season_jumps == 0)
  # When jumper has jumps, but not this season and opponent has jumps this year
  count_jumps_not_this_season_opp_jumps_this_season <- sum(possession_test$jumps > 0 & possession_test$season_jumps == 0 & possession_test$opp_season_jumps > 0)
  
  # When jumper has jumps this season and opponent has no jumps
  count_jumps_this_season_opp_no_jumps <- sum(possession_test$season_jumps > 0 & possession_test$opp_jumps == 0)
  # When jumper has jumps this season and opponent has jumps, but not this season
  count_jumps_this_season_opp_jumps_not_this_season <- sum(possession_test$season_jumps > 0 & possession_test$opp_jumps > 0 & possession_test$opp_season_jumps == 0)
  # When jumper and opponent have jumps this season
  count_jumps_this_season_opp_jumps_this_season <- sum(possession_test$season_jumps > 0 & possession_test$opp_season_jumps > 0)
  
  # Initializing df that stores daily test data predictions
  daily_model_preds <- tibble()
  
  #### Models for when Player has had 0 jumps and Opponent has jumped, but not jumped this season
  if (count_no_jumps_opp_jumps_not_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(opp_jumps > 0) %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps == 0 & opp_jumps > 0 & opp_season_jumps == 0) %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "no_jumps_opp_jumps_not_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)

  }
  
  #### Models for when Player has had 0 jumps and Opponent has jumped this season
  if (count_no_jumps_opp_jumps_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(opp_season_jumps > 0) %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps == 0 & opp_season_jumps > 0) %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "no_jumps_opp_jumps_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player and opponent have 0 jumps
  if (count_no_jumps_opp_no_jumps > 0) {
    
    possession_train_model <-
      possession_train %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps == 0 & opp_jumps == 0) %>%
      select(-c(jumps, all_jumps_avg, last_10_avg, last_25_avg, last_50_avg, last_75_avg, last_100_avg,
                season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "no_jumps_opp_no_jumps")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps, but not this season, and opponent has 0 jumps
  if (count_jumps_not_this_season_opp_no_jumps > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(jumps > 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps > 0 & season_jumps == 0 & opp_jumps == 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_not_this_season_opp_no_jumps")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps, but not this season, and opponent has jumps, but not this season
  if (count_jumps_not_this_season_opp_jumps_not_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(jumps > 0 & opp_jumps > 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps > 0 & season_jumps == 0 & opp_jumps > 0 & opp_season_jumps == 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_not_this_season_opp_jumps_not_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps, but not this season, and opponent has jumped this season
  if (count_jumps_not_this_season_opp_jumps_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(jumps > 0 & opp_season_jumps > 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(jumps > 0 & season_jumps == 0 & opp_season_jumps > 0) %>%
      select(-c(season_jumps, season_all_jumps_avg, season_last_10_avg, season_last_25_avg, season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_not_this_season_opp_jumps_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps this season, and opponent has 0 jumps
  if (count_jumps_this_season_opp_no_jumps > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(season_jumps > 0) %>%
      select(-c(opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(season_jumps > 0 & opp_jumps == 0) %>%
      select(-c(opp_jumps, opp_all_jumps_avg, opp_last_10_avg, opp_last_25_avg, opp_last_50_avg, opp_last_75_avg, opp_last_100_avg,
                opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_this_season_opp_no_jumps")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps this season, and opponent has jumps, but not this season
  if (count_jumps_this_season_opp_jumps_not_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(season_jumps > 0 & opp_jumps > 0) %>%
      select(-c(opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    possession_test_model <-
      possession_test %>%
      filter(season_jumps > 0 & opp_jumps > 0 & opp_season_jumps == 0) %>%
      select(-c(opp_season_jumps, opp_season_all_jumps_avg, opp_season_last_10_avg, opp_season_last_25_avg, opp_season_last_50_avg))
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_this_season_opp_jumps_not_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }
  
  #### Models for when Player has had jumps this season, and opponent has jumped this season
  if (count_jumps_this_season_opp_jumps_this_season > 0) {
    
    possession_train_model <-
      possession_train %>%
      filter(season_jumps > 0 & opp_season_jumps > 0)
    
    possession_test_model <-
      possession_test %>%
      filter(season_jumps > 0 & opp_season_jumps > 0)
    
    preds <- function_to_run_models(possession_train_model, possession_test_model, "jumps_this_season_opp_jumps_this_season")
    
    daily_model_preds <- rbind.data.frame(daily_model_preds, preds)
    
  }

  test_df_exp_win_master = rbind.data.frame(test_df_exp_win_master, daily_model_preds)
}

write.csv(test_df_exp_win_master, "data/02_curated/nba_first_to_score/model_ml.csv.gz", row.names = FALSE)

# If reading in latest version (skipping full run)
test_df_exp_win_master <- 
  fread("data/02_curated/nba_first_to_score/model_ml.csv.gz") %>%
  mutate(game_date = as.Date(game_date),
         game_id = as.character(game_id))

test_df_exp_win_master_all_combined <- 
  test_df_exp_win_master %>%
  rowwise() %>%
  mutate(win_tip_prob_all = mean(c(iterative_win_tip, 
                                   #win_tip_prob_ranger,
                                   win_tip_prob_glmnet,
                                   win_tip_prob_earth)))

brier_score_test_df_loop <-
  test_df_exp_win_master_all_combined %>%
  mutate(brier_score_iterative = (iterative_win_tip - won_tip)^2,
         #brier_score_win_tip_ranger = (win_tip_prob_ranger - won_tip)^2,
         brier_score_win_tip_glmnet = (win_tip_prob_glmnet - won_tip)^2,
         brier_score_win_tip_earth = (win_tip_prob_earth - won_tip)^2,
         brier_score_win_tip_all = (win_tip_prob_all - won_tip)^2)

brier_by_season <-
  brier_score_test_df_loop %>%
  group_by(season) %>%
  summarise(brier_score_iterative = mean(brier_score_iterative),
            #brier_score_win_tip_ranger = mean(brier_score_win_tip_ranger),
            brier_score_win_tip_glmnet = mean(brier_score_win_tip_glmnet),
            brier_score_win_tip_earth = mean(brier_score_win_tip_earth),
            brier_score_win_tip_all = mean(brier_score_win_tip_all))
 
message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_iterative), 5))
#message("brier score of ranger win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_ranger), 5))
message("brier score of glmnet win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_glmnet), 5))
message("brier score of earth win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_earth), 5))
message("brier score of all win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_all), 5))

test_buckets_win_tip_loop <-
  test_df_exp_win_master_all_combined %>%
  mutate(exp_win_prob = cut(win_tip_prob_all, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

iterative_auc_loop <- roc(won_tip ~ iterative_win_tip, data = test_df_exp_win_master_all_combined)
iterative_auc_loop$auc
plot(iterative_auc_loop)

# ranger_auc_loop <- roc(won_tip ~ win_tip_prob_ranger, data = test_df_exp_win_master_all_combined)
# ranger_auc_loop$auc
# plot(ranger_auc_loop)

glmnet_auc_loop <- roc(won_tip ~ win_tip_prob_glmnet, data = test_df_exp_win_master_all_combined)
glmnet_auc_loop$auc
plot(glmnet_auc_loop)

earth_auc_loop <- roc(won_tip ~ win_tip_prob_earth, data = test_df_exp_win_master_all_combined)
earth_auc_loop$auc
plot(earth_auc_loop)

all_auc_loop <- roc(won_tip ~ win_tip_prob_all, data = test_df_exp_win_master_all_combined)
all_auc_loop$auc
plot(all_auc_loop)

one_row_per_jump_home <-
  test_df_exp_win_master_all_combined %>%
  filter(home == TRUE) %>%
  drop_na() %>%
  select(season, game_date, game_id, matchup, period, period_clock,
         jumper, opp_jumper, home, won_tip,
         iterative_win_tip,# win_tip_prob_ranger,
         win_tip_prob_glmnet,
         win_tip_prob_earth,
         win_tip_prob_all)

one_row_per_jump_away <-
  test_df_exp_win_master_all_combined %>%
  filter(home == FALSE) %>%
  drop_na() %>%
  select(season, game_date, game_id, matchup, period, period_clock,
         jumper, opp_jumper,
         iterative_win_tip, #win_tip_prob_ranger, 
         win_tip_prob_glmnet,
         win_tip_prob_earth,
         win_tip_prob_all) %>%
  rename(opp_jumper = jumper,
         jumper = opp_jumper,
         iterative_win_tip_opp = iterative_win_tip,
         #win_tip_prob_ranger_opp = win_tip_prob_ranger,
         win_tip_prob_glmnet_opp = win_tip_prob_glmnet,
         win_tip_prob_earth_opp = win_tip_prob_earth,
         win_tip_prob_all_opp = win_tip_prob_all)

one_row_per_jump_joined <-
  one_row_per_jump_home %>%
  left_join(one_row_per_jump_away,
            by = c("season", "game_date", "game_id", "matchup", "period", "period_clock", "jumper", "opp_jumper")) %>%
  mutate(final_win_prob_iterative = (iterative_win_tip + (1 - iterative_win_tip_opp)) / 2,
         #final_win_prob_ranger = (win_tip_prob_ranger + (1 - win_tip_prob_ranger_opp)) / 2,
         final_win_prob_glmnet = (win_tip_prob_glmnet + (1 - win_tip_prob_glmnet_opp)) / 2,
         final_win_prob_earth = (win_tip_prob_earth + (1 - win_tip_prob_earth_opp)) / 2,
         final_win_prob_all = (win_tip_prob_all + (1 - win_tip_prob_all_opp)) / 2) %>%
  drop_na()

brier_score_test_df_loop <-
  one_row_per_jump_joined %>%
  mutate(brier_score_iterative = (final_win_prob_iterative - won_tip)^2,
         #brier_score_win_tip_ranger = (final_win_prob_ranger - won_tip)^2,
         brier_score_win_tip_glmnet = (final_win_prob_glmnet - won_tip)^2,
         brier_score_win_tip_earth = (final_win_prob_earth - won_tip)^2,
         brier_score_win_tip_all = (final_win_prob_all - won_tip)^2)

message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_iterative), 5))
#message("brier score of ranger win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_ranger), 5))
message("brier score of glmnet win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_glmnet), 5))
message("brier score of earth win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_earth), 5))
message("brier score of all win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_all), 5))

test_buckets_win_tip_loop <-
  one_row_per_jump_joined %>%
  mutate(exp_win_prob = cut(final_win_prob_all, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

iterative_auc_loop <- roc(won_tip ~ iterative_win_tip, data = one_row_per_jump_joined)
iterative_auc_loop$auc
plot(iterative_auc_loop)

# ranger_auc_loop <- roc(won_tip ~ win_tip_prob_ranger, data = one_row_per_jump_joined)
# ranger_auc_loop$auc
# plot(ranger_auc_loop)

glmnet_auc_loop <- roc(won_tip ~ win_tip_prob_glmnet, data = one_row_per_jump_joined)
glmnet_auc_loop$auc
plot(glmnet_auc_loop)

earth_auc_loop <- roc(won_tip ~ win_tip_prob_earth, data = one_row_per_jump_joined)
earth_auc_loop$auc
plot(earth_auc_loop)

all_auc_loop <- roc(won_tip ~ win_tip_prob_all, data = one_row_per_jump_joined)
all_auc_loop$auc
plot(all_auc_loop)


############ See how it compares by season
season_analysis <-
  brier_score_test_df_loop %>%
  group_by(season) %>%
  summarise(iterative =  round(mean(brier_score_iterative), 5),
            #ranger =  round(mean(brier_score_win_tip_ranger), 5),
            glmnet =  round(mean(brier_score_win_tip_glmnet), 5),
            earth =  round(mean(brier_score_win_tip_earth), 5),
            all =  round(mean(brier_score_win_tip_all), 5))

## Join back to entire data set
final_output <-
  possession_binded %>%
  mutate(game_date = as.Date(game_date),
         game_id = as.character(as.integer(game_id))) %>%
  left_join(one_row_per_jump_joined, 
            by = c("season", "game_date", "game_id", "matchup", "period", "period_clock")) %>%
  drop_na() %>%
  select(-c(iterative_win_tip, #win_tip_prob_ranger, 
            win_tip_prob_glmnet, win_tip_prob_earth, win_tip_prob_all,
            iterative_win_tip_opp, #win_tip_prob_ranger_opp,
            win_tip_prob_glmnet_opp, win_tip_prob_earth_opp, win_tip_prob_all_opp,
            final_win_prob_iterative, #final_win_prob_ranger, 
            final_win_prob_glmnet, final_win_prob_earth))

write.csv(final_output, "data/02_curated/nba_first_to_score/win_tip_outputs.csv.gz", row.names = FALSE)












# ########################### Random Train/Test split ######################################
# possession_final_cols <-
#   possession_models_joined %>%
#   select(-c(game_date, game_id, matchup, person_id, team_abbrev, 
#             opp_person_id, opp_team_abbrev, possession, score_first)) #%>%
#   # select(-c(season_last_10_avg,
#   #           season_last_25_avg, season_last_50_avg, opp_season_last_10_avg,
#   #           opp_season_last_25_avg, opp_season_last_50_avg)) %>%
#   # select(-c(season_jumps, opp_season_jumps, season_all_jumps_avg, opp_season_all_jumps_avg))
# 
# ## Adding random number for training
# set.seed(052121)
# possession_final_df <-
#   possession_final_cols %>%
#   rowwise() %>%
#   mutate(train_or_test = if_else(runif(1) > .75, "Test", "Train")) %>%
#   drop_na()
# 
# training_data <-
#   possession_final_df %>%
#   filter(train_or_test == "Train") %>%
#   select(-train_or_test)
# 
# testing_data <-
#   possession_final_df %>%
#   filter(train_or_test == "Test") %>%
#   select(-train_or_test)
# 
# rec <-
#   recipe(training_data) %>%
#   update_role(won_tip, new_role = 'outcome') %>%
#   update_role(-all_outcomes(), new_role = 'predictor') %>%
#   update_role(ends_with('jumper'), new_role = 'jumper') %>%
#   update_role(season, new_role = 'season') %>%
#   prep()
# 
# baked_train <- bake(rec, training_data)
# baked_test <- bake(rec, testing_data)
# 
# ## subset predictors
# predictor_vars <- rec$term_info$variable[rec$term_info$role == 'predictor']
# 
# predictors_train <- 
#   baked_train %>%
#   select(any_of(predictor_vars))
# 
# predictors_test <-
#   baked_test %>%
#   select(any_of(predictor_vars))
# 
# ## subset outcomes
# outcome_vars <- rec$term_info$variable[rec$term_info$role == 'outcome']
# 
# outcomes <- 
#   baked_train %>%
#   select(any_of(outcome_vars))
# 
# mod_ranger <- ranger(x = predictors_train, y = as.factor(outcomes$won_tip), 
#                      num.trees = 500, probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)
# 
# preds_ranger <- 
#   cbind.data.frame(baked_test, predict(mod_ranger, predictors_test, type = 'response')$predictions) %>%
#   rename(win_tip_prob_ranger = `TRUE`) %>%
#   select(-c(`FALSE`)) %>%
#   mutate(won_tip = as.logical(won_tip))
# 
# mod_glmnet <- cv.glmnet(x = as.matrix(predictors_train), y = outcomes$won_tip, family = 'binomial', alpha = 1)
# 
# preds_glmnet <- 
#   cbind.data.frame(baked_test, win_tip_prob_glmnet = as.numeric(predict(mod_glmnet, as.matrix(predictors_test), type = 'response'))) %>%
#   mutate(won_tip = as.logical(won_tip))
# 
# mod_earth <- earth(x = predictors_train, y = outcomes$won_tip, degree = 2, glm = list(family = binomial), nfold = 3)
# 
# preds_earth <- 
#   cbind.data.frame(baked_test, predict(mod_earth, predictors_test, type = 'response')) %>%
#   rename(win_tip_prob_earth = `outcomes$won_tip`) %>%
#   mutate(won_tip = as.logical(won_tip))
# 
# preds_all <-
#   preds_ranger %>%
#   left_join(preds_glmnet) %>%
#   left_join(preds_earth) %>%
#   rowwise() %>%
#   mutate(win_tip_prob_all = mean(c(win_tip_prob_ranger, win_tip_prob_glmnet, win_tip_prob_earth)))
# 
# brier_score_test_df <-
#   preds_all %>%
#   mutate(brier_score_iterative = (iterative_win_tip - won_tip)^2,
#          brier_score_win_tip_ranger = (win_tip_prob_ranger - won_tip)^2,
#          brier_score_win_tip_glmnet = (win_tip_prob_glmnet - won_tip)^2,
#          brier_score_win_tip_earth = (win_tip_prob_earth - won_tip)^2,
#          brier_score_win_tip_all = (win_tip_prob_all - won_tip)^2)
# 
# message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df$brier_score_iterative), 5))
# message("brier score of ranger win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_ranger), 5))
# message("brier score of glmnet win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_glmnet), 5))
# message("brier score of earth win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_earth), 5))
# message("brier score of all win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_all), 5))
# 
# test_buckets_win_tip_ranger <-
#   preds_ranger %>%
#   mutate(exp_win_prob = cut(win_tip_prob_ranger, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
#   group_by(exp_win_prob) %>%
#   summarise(jumps = n(),
#             true_win_percent = mean(won_tip),
#             .groups = 'drop')
# 
# test_buckets_win_tip_glmnet <-
#   preds_glmnet %>%
#   mutate(exp_win_prob = cut(win_tip_glmnet, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
#   group_by(exp_win_prob) %>%
#   summarise(jumps = n(),
#             true_win_percent = mean(won_tip),
#             .groups = 'drop')
# 
# test_buckets_win_tip_earth <-
#   preds_earth %>%
#   mutate(exp_win_prob = cut(win_tip_earth, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
#   group_by(exp_win_prob) %>%
#   summarise(jumps = n(),
#             true_win_percent = mean(won_tip),
#             .groups = 'drop')
# 
# 



