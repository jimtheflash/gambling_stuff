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
# earliest_train_data_date <- "2018-09-01"
# train_test_date_split <- "2020-02-01"

# Load previous run of this model
model_ftts_file_path <- "data/02_curated/nba_first_to_score/model_ftts.csv.gz"

# Initializing df that stores test data predictions
# If prior version of model exists, use that file
# Otherwise, start from scratch
# This allows process to not iterate through every day - only those that we don't have data for already
if (file.exists(model_ftts_file_path)){
  current_model_ftts <- fread("data/02_curated/nba_first_to_score/model_ftts.csv.gz",
                              colClasses =  c('game_date' = 'Date', 
                                              'game_id' = 'character'))
  max_date_model_ftts <- max(current_model_ftts$game_date)
}else{
  # Start training at midway 19-20 season season
  max_date_model_ftts <- as.Date("2020-01-31")
}

model_win_tip <- fread("data/02_curated/nba_first_to_score/win_tip_outputs.csv.gz",
                       colClasses = c('game_date' = 'Date',
                                      'game_id' = 'character',
                                      'home_team_person_id' = 'character',
                                      'away_team_person_id' = 'character'))
min_date_model_win_tip <- min(model_win_tip$game_date)

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- min_date_model_win_tip
# Setting date we want to start logging test data on
# Default uses today's date so that all completed games are used in calculating ratings
train_test_date_split <- max_date_model_ftts + days(1)

line_info_df <- 
  fread("./data/nba_lines/all_historical_lines.csv") %>%
  mutate(game_date = as.Date(game_date),
         team_id = as.character(team_id))

# Reads in list of teams to join on team id
team_list <-
  fread("./data/01_raw/nba_teams/current.csv") %>%
  select(TEAM_ID, ABBREVIATION) %>%
  mutate(TEAM_ID = as.character(TEAM_ID))

model_relevant_cols <-
  model_win_tip %>%
  select(season, game_date, game_id, matchup, home_team_abbrev,
         away_team_abbrev, possession, score_first, home_won_tip,
         home_score_first, final_win_prob_all)

# Original model gave slight advantage to home team to score first
score_first_original <-
  model_relevant_cols %>%
  mutate(exp_score_first_all = (final_win_prob_all*.61) + ((1 - final_win_prob_all)*.41))

# # Determines Brier Score for score first backtesting
# brier_score_first_df <-
#   score_first_original %>%
#   mutate(brier_score_all = (exp_score_first_all - home_score_first)^2)
# 
# message("brier score of score first all is equal to ", round(mean(brier_score_first_df$brier_score_all), 5))

score_first_home <-
  model_relevant_cols %>%
  select(season, game_date, game_id, matchup, 
         team_abbrev = home_team_abbrev, opp_team_abbrev = away_team_abbrev,
         possession, won_tip = home_won_tip, score_first = home_score_first,
         win_tip_prob = final_win_prob_all) %>%
  mutate(home = TRUE)

score_first_away <-
  model_relevant_cols %>%
  select(season, game_date, game_id, matchup, 
         team_abbrev = away_team_abbrev, opp_team_abbrev = home_team_abbrev,
         possession, won_tip = home_won_tip, score_first = home_score_first,
         win_tip_prob = final_win_prob_all) %>%
  mutate(home = FALSE,
         won_tip = !won_tip,
         score_first = !score_first,
         win_tip_prob = 1 - win_tip_prob)

score_first_df <-
  rbind.data.frame(score_first_home, score_first_away) %>%
  left_join(team_list, by = c("team_abbrev" = "ABBREVIATION")) %>%
  rename(team_id = TEAM_ID)

score_first_game_lines_all <-
  score_first_df %>%
  left_join(line_info_df, by = c("game_date", "team_id")) %>%
  mutate(score_first = as.factor(score_first))

score_first_future_games <-
  score_first_game_lines_all %>%
  filter(is.na(won_tip))

### Removing cases when line only exists for one of two teams in game
score_first_game_lines_no_na <-
  score_first_df %>%
  left_join(line_info_df, by = c("game_date", "team_id")) %>%
  mutate(score_first = as.factor(score_first)) %>%
  drop_na()

single_count_game_ids <-
  score_first_game_lines_no_na %>%
  group_by(game_id) %>%
  count() %>%
  filter(n == 1)

score_first_game_lines_no_na <-
  score_first_game_lines_no_na %>%
  filter(!game_id %in% single_count_game_ids$game_id)

### Adding back in future games
score_first_game_lines <-
  rbind.data.frame(score_first_game_lines_no_na, score_first_future_games)

################# Day by Day Run #####################
# Set of dates to loop through for test data
unique_dates <-
  score_first_game_lines %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  filter(game_date >= train_test_date_split)

score_first_relevant_cols <-
  score_first_game_lines %>%
  select(-c(bookmaker_spread_line_delta_perc, bookmaker_moneyline_delta_perc, bookmaker_total_line_delta_perc,
            bookmaker_spread_line_delta, bookmaker_moneyline_delta, bookmaker_total_line_delta,
            open_bookmaker_moneyline_line, open_bookmaker_implied_pts, open_bookmaker_total_line,
            close_bookmaker_moneyline_line, close_bookmaker_implied_pts,
            close_bookmaker_total_line,
            open_bookmaker_implied_pts_allowed, close_bookmaker_implied_pts_allowed,
            open_bookmaker_spread_line))

function_to_run_models <- function(score_first_train_model, score_first_test_model, model_desc){
  
  set.seed(060321)
  
  ## subset predictors, outcomes, extras
  outcome_vars <- "score_first"
  game_info_vars <- c("season", "game_date", "game_id", "matchup", 
                      "team_abbrev", "team_id", "opp_team_abbrev", "possession", "won_tip")
  predictor_vars <- colnames(subset(score_first_train_model, select = !names(score_first_train_model) %in% c(game_info_vars, outcome_vars)))
  
  predictors_train <- 
    score_first_train_model %>%
    select(any_of(predictor_vars))
  
  predictors_test <-
    score_first_test_model %>%
    select(any_of(predictor_vars))
  
  outcomes_train <- 
    score_first_train_model %>%
    select(any_of(outcome_vars))
  
  outcomes_test <- 
    score_first_test_model %>%
    select(any_of(outcome_vars))
  
  game_info_train <-
    score_first_train_model %>%
    select(any_of(game_info_vars))
  
  game_info_test <-
    score_first_test_model %>%
    select(any_of(game_info_vars))
  
  model_ranger <- ranger(x = predictors_train, y = outcomes_train$score_first, 
                         probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)
  
  preds_ranger <- 
    cbind.data.frame(game_info_test,
                     predictors_test, 
                     outcomes_test,
                     predict(model_ranger, predictors_test, type = 'response')$predictions) %>%
    rename(score_first_prob_ranger = `TRUE`) %>%
    select(-c(`FALSE`)) %>%
    mutate(score_first = as.logical(score_first))
  
  model_glmnet <- cv.glmnet(x = as.matrix(predictors_train), y = outcomes_train$score_first, family = 'binomial')
  
  preds_glmnet <- 
    cbind.data.frame(game_info_test,
                     predictors_test, 
                     outcomes_test,
                     score_first_prob_glmnet = as.numeric(predict(model_glmnet, as.matrix(predictors_test), type = 'response'))) %>%
    mutate(score_first = as.logical(score_first))
  
  model_earth <- earth(x = predictors_train, y = outcomes_train$score_first, degree = 2, glm = list(family = binomial), nfold = 3)
  
  preds_earth <- 
    cbind.data.frame(game_info_test,
                     predictors_test, 
                     outcomes_test,
                     predict(model_earth, predictors_test, type = 'response')) %>%
    rename(score_first_prob_earth = `TRUE`) %>%
    mutate(score_first = as.logical(score_first))
  
  preds <-
    preds_ranger %>%
    cbind.data.frame(score_first_prob_glmnet = preds_glmnet$score_first_prob_glmnet, 
                     score_first_prob_earth = preds_earth$score_first_prob_earth,
                     model = model_desc)
  
  return(preds)
  
}

# If no values in unique_dates (i.e. doing current day run instead of historical testing)
# Set unique_dates to current date so loop runs once to get player ratings
if (nrow(unique_dates) == 0) {
  unique_dates <- tibble(game_date = Sys.Date())
}

# Initializing df that stores test data predictions
if (exists('current_model_ftts')){
  test_df_score_first_master <- current_model_ftts
}else{
  test_df_score_first_master <- tibble()
}

for (i in 1:length(unique_dates$game_date)) {
  message(unique_dates$game_date[i])
  
  # Splitting data into train and test sets
  score_first_train <-
    score_first_relevant_cols %>%
    filter(game_date < unique_dates$game_date[i])

  score_first_test <-
    score_first_relevant_cols %>%
    filter(game_date == unique_dates$game_date[i]) 
  
  score_first_train_model <- score_first_train
  score_first_test_model <- score_first_test
  
  if (nrow(score_first_test_model) > 0) {
    preds <- function_to_run_models(score_first_train_model, score_first_test_model, "all")
  }else{
    preds <- tibble()
  }
  
  test_df_score_first_master = rbind.data.frame(test_df_score_first_master, preds)
}

predictions_historical <-
  test_df_score_first_master %>%
  filter(!is.na(won_tip))

predictions_today <-
  test_df_score_first_master %>%
  filter(is.na(won_tip))

write.csv(predictions_historical, "data/02_curated/nba_first_to_score/model_ftts.csv.gz", row.names = FALSE)
write.csv(predictions_today, "data/02_curated/nba_first_to_score/today_ftts.csv.gz", row.names = FALSE)

test_df_score_first_master_mutated <-
  test_df_score_first_master %>%
  mutate(score_first_prob_iterative = if_else(home == TRUE, 
                                              (win_tip_prob*.61) + ((1 - win_tip_prob)*.41),
                                              ((1 - win_tip_prob)*.39) + (win_tip_prob*.59)))

# Currently not including markov
test_df_score_first_master_all_combined <- 
  test_df_score_first_master_mutated %>%
  rowwise() %>%
  mutate(score_first_prob_all = mean(c(#score_first_prob_iterative, 
                                       #score_first_prob_ranger,
                                       score_first_prob_glmnet,
                                       score_first_prob_earth)))

one_row_per_game_home <-
  test_df_score_first_master_all_combined %>%
  filter(home == TRUE) %>%
  select(season, game_date, game_id, matchup, home_score_first = score_first,
         score_first_prob_iterative, score_first_prob_ranger, score_first_prob_glmnet,
         score_first_prob_earth, score_first_prob_all)

one_row_per_game_away <-
  test_df_score_first_master_all_combined %>%
  filter(home == FALSE) %>%
  select(season, game_date, game_id, matchup,
         score_first_prob_iterative, score_first_prob_ranger, score_first_prob_glmnet,
         score_first_prob_earth, score_first_prob_all) %>%
  rename(score_first_prob_iterative_opp = score_first_prob_iterative,
         score_first_prob_ranger_opp = score_first_prob_ranger,
         score_first_prob_glmnet_opp = score_first_prob_glmnet,
         score_first_prob_earth_opp = score_first_prob_earth,
         score_first_prob_all_opp = score_first_prob_all)

one_row_per_game_joined <-
  one_row_per_game_home %>%
  left_join(one_row_per_game_away,
            by = c("season", "game_date", "game_id", "matchup")) %>%
  mutate(final_score_first_prob_iterative = (score_first_prob_iterative + (1 - score_first_prob_iterative_opp)) / 2,
         final_score_first_prob_ranger = (score_first_prob_ranger + (1 - score_first_prob_ranger_opp)) / 2,
         final_score_first_prob_glmnet = (score_first_prob_glmnet + (1 - score_first_prob_glmnet_opp)) / 2,
         final_score_first_prob_earth = (score_first_prob_earth + (1 - score_first_prob_earth_opp)) / 2,
         final_score_first_prob_all = (score_first_prob_all + (1 - score_first_prob_all_opp)) / 2)

## Join back to entire data set
final_output <-
  model_win_tip %>%
  mutate(game_date = as.Date(game_date),
         game_id = as.character(as.integer(game_id))) %>%
  left_join(one_row_per_game_joined, by = c("season", "game_date", "game_id", "matchup", "home_score_first")) %>%
  filter(!is.na(final_score_first_prob_all)) %>%
  select(-c(score_first_prob_iterative, score_first_prob_ranger, score_first_prob_glmnet,
            score_first_prob_earth, score_first_prob_all,
            score_first_prob_iterative_opp, score_first_prob_ranger_opp, score_first_prob_glmnet_opp,
            score_first_prob_earth_opp, score_first_prob_all_opp,
            final_score_first_prob_iterative, final_score_first_prob_ranger, final_score_first_prob_glmnet,
            final_score_first_prob_earth))

write.csv(final_output, "data/02_curated/nba_first_to_score/score_first_outputs.csv.gz", row.names = FALSE)

########## Historical Testing Performance ###############
test_df_score_first_master_all_historical <-
  test_df_score_first_master_all_combined %>%
  drop_na()

brier_score_test_df_loop <-
  test_df_score_first_master_all_historical %>%
  mutate(brier_score_score_first_iterative = (score_first_prob_iterative - score_first)^2,
         brier_score_score_first_ranger = (score_first_prob_ranger - score_first)^2,
         brier_score_score_first_glmnet = (score_first_prob_glmnet - score_first)^2,
         brier_score_score_first_earth = (score_first_prob_earth - score_first)^2,
         brier_score_score_first_all = (score_first_prob_all - score_first)^2)

brier_by_season <-
  brier_score_test_df_loop %>%
  group_by(season) %>%
  summarise(brier_score_score_first_iterative = mean(brier_score_score_first_iterative),
            brier_score_score_first_ranger = mean(brier_score_score_first_ranger),
            brier_score_score_first_glmnet = mean(brier_score_score_first_glmnet),
            brier_score_score_first_earth = mean(brier_score_score_first_earth),
            brier_score_score_first_all = mean(brier_score_score_first_all))
 
message("brier score of iterative score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_iterative), 5))
message("brier score of ranger score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_ranger), 5))
message("brier score of glmnet score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_glmnet), 5))
message("brier score of earth score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_earth), 5))
message("brier score of all score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_all), 5))

test_buckets_score_first_loop <-
  test_df_score_first_master_all_historical %>%
  mutate(exp_win_prob = cut(score_first_prob_all, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(obs = n(),
            true_score_first_percent = mean(score_first),
            .groups = 'drop')

iterative_auc_loop <- roc(score_first ~ score_first_prob_iterative, data = test_df_score_first_master_all_historical)
iterative_auc_loop$auc
plot(iterative_auc_loop)

ranger_auc_loop <- roc(score_first ~ score_first_prob_ranger, data = test_df_score_first_master_all_historical)
ranger_auc_loop$auc
plot(ranger_auc_loop)

glmnet_auc_loop <- roc(score_first ~ score_first_prob_glmnet, data = test_df_score_first_master_all_historical)
glmnet_auc_loop$auc
plot(glmnet_auc_loop)

earth_auc_loop <- roc(score_first ~ score_first_prob_earth, data = test_df_score_first_master_all_historical)
earth_auc_loop$auc
plot(earth_auc_loop)

all_auc_loop <- roc(score_first ~ score_first_prob_all, data = test_df_score_first_master_all_historical)
all_auc_loop$auc
plot(all_auc_loop)


brier_score_test_df_loop <-
  one_row_per_game_joined %>%
  drop_na() %>%
  mutate(brier_score_score_first_iterative = (final_score_first_prob_iterative - home_score_first)^2,
         brier_score_score_first_ranger = (final_score_first_prob_ranger - home_score_first)^2,
         brier_score_score_first_glmnet = (final_score_first_prob_glmnet - home_score_first)^2,
         brier_score_score_first_earth = (final_score_first_prob_earth - home_score_first)^2,
         brier_score_score_first_all = (final_score_first_prob_all - home_score_first)^2)

message("brier score of iterative score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_iterative), 5))
message("brier score of ranger score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_ranger), 5))
message("brier score of glmnet score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_glmnet), 5))
message("brier score of earth score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_earth), 5))
message("brier score of all score first is equal to ", round(mean(brier_score_test_df_loop$brier_score_score_first_all), 5))

test_buckets_score_first_loop <-
  one_row_per_game_joined %>%
  drop_na() %>%
  mutate(exp_win_prob = cut(final_score_first_prob_all, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(obs = n(),
            true_score_first_percent = mean(home_score_first),
            .groups = 'drop')

iterative_auc_loop <- roc(home_score_first ~ score_first_prob_iterative, data = one_row_per_game_joined)
iterative_auc_loop$auc
plot(iterative_auc_loop)

ranger_auc_loop <- roc(home_score_first ~ score_first_prob_ranger, data = one_row_per_game_joined)
ranger_auc_loop$auc
plot(ranger_auc_loop)

glmnet_auc_loop <- roc(home_score_first ~ score_first_prob_glmnet, data = one_row_per_game_joined)
glmnet_auc_loop$auc
plot(glmnet_auc_loop)

earth_auc_loop <- roc(home_score_first ~ score_first_prob_earth, data = one_row_per_game_joined)
earth_auc_loop$auc
plot(earth_auc_loop)

all_auc_loop <- roc(home_score_first ~ score_first_prob_all, data = one_row_per_game_joined)
all_auc_loop$auc
plot(all_auc_loop)

############ See how it compares by season
season_analysis <-
  brier_score_test_df_loop %>%
  group_by(season) %>%
  summarise(iterative =  round(mean(brier_score_score_first_iterative), 5),
            ranger =  round(mean(brier_score_score_first_ranger), 5),
            glmnet =  round(mean(brier_score_score_first_glmnet), 5),
            earth =  round(mean(brier_score_score_first_earth), 5),
            all =  round(mean(brier_score_score_first_all), 5))

