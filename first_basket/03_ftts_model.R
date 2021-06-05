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

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2018-09-01"
# Setting date we want to start logging test data on
# Default uses today's date so that all completed games are used in calculating ratings
train_test_date_split <- "2020-02-01"

line_info_df <- 
  fread("./data/nba_lines/all_historical_lines.csv") %>%
  mutate(game_date = as.Date(game_date),
         team_id = as.character(team_id))

# Reads in list of teams to join on team id
team_list <-
  fread("./data/01_raw/nba_teams/current.csv") %>%
  select(TEAM_ID, ABBREVIATION) %>%
  mutate(TEAM_ID = as.character(TEAM_ID))

team_ratings <-
  fread("./data/02_curated/nba_team_ratings/dunksandthrees.csv") %>%
  mutate(date = as.Date(date)) %>%
  separate(wl, c("Wins", "Losses")) %>%
  mutate(games_played = as.integer(Wins) + as.integer(Losses))

model_ml <- fread("data/02_curated/nba_first_to_score/win_tip_outputs.csv.gz",
                  colClasses = c('game_date' = 'Date',
                                 'game_id' = 'character',
                                 'home_team_person_id' = 'character',
                                 'away_team_person_id' = 'character'))

model_relevant_cols <-
  model_ml %>%
  select(season, game_date, game_id, matchup, home_team_abbrev,
         away_team_abbrev, possession, score_first, home_won_tip,
         home_score_first, final_win_prob_all)

score_first_original <-
  model_relevant_cols %>%
  mutate(exp_score_first_all = (final_win_prob_all*.61) + ((1 - final_win_prob_all)*.41))

# Determines Brier Score for score first backtesting
brier_score_first_df <-
  score_first_original %>%
  mutate(brier_score_all = (exp_score_first_all - home_score_first)^2)

message("brier score of score first all is equal to ", round(mean(brier_score_first_df$brier_score_all), 5))

score_first_home <-
  model_relevant_cols %>%
  select(season, game_date, game_id, matchup, 
         team_abbrev = home_team_abbrev, opp_team_abbrev = away_team_abbrev,
         possession, won_tip = home_won_tip, score_first = home_score_first,
         win_tip_prob = final_win_prob_all) %>%
  mutate(home = TRUE) %>%
  left_join(select(team_ratings, team, anet, aortg, adrtg, pace, games_played, date), by = c("team_abbrev" = "team", "game_date" = "date")) %>%
  rename(team_anet = anet, team_aortg = aortg, team_adrtg = adrtg, team_pace = pace, team_games_played = games_played) %>%
  left_join(select(team_ratings, team, anet, aortg, adrtg, pace, games_played, date), by = c("opp_team_abbrev" = "team", "game_date" = "date")) %>%
  rename(opp_anet = anet, opp_aortg = aortg, opp_adrtg = adrtg, opp_pace = pace, opp_games_played = games_played)

score_first_away <-
  model_relevant_cols %>%
  select(season, game_date, game_id, matchup, 
         team_abbrev = away_team_abbrev, opp_team_abbrev = home_team_abbrev,
         possession, won_tip = home_won_tip, score_first = home_score_first,
         win_tip_prob = final_win_prob_all) %>%
  left_join(select(team_ratings, team, anet, aortg, adrtg, pace, games_played, date), by = c("team_abbrev" = "team", "game_date" = "date")) %>%
  rename(team_anet = anet, team_aortg = aortg, team_adrtg = adrtg, team_pace = pace, team_games_played = games_played) %>%
  left_join(select(team_ratings, team, anet, aortg, adrtg, pace, games_played, date), by = c("opp_team_abbrev" = "team", "game_date" = "date")) %>%
  rename(opp_anet = anet, opp_aortg = aortg, opp_adrtg = adrtg, opp_pace = pace, opp_games_played = games_played) %>%
  mutate(home = FALSE,
         won_tip = !won_tip,
         score_first = !score_first,
         win_tip_prob = 1 - win_tip_prob)

score_first_df <-
  rbind.data.frame(score_first_home, score_first_away) %>%
  left_join(team_list, by = c("team_abbrev" = "ABBREVIATION")) %>%
  rename(team_id = TEAM_ID) %>%
  drop_na()

score_first_game_lines <-
  score_first_df %>%
  left_join(line_info_df, by = c("game_date", "team_id")) %>%
  mutate(score_first = as.factor(score_first)) %>%
  drop_na()

### Removing cases when line only exists for one of two teams in game
score_first_game_lines_no_na <-
  score_first_df %>%
  left_join(line_info_df, by = c("game_date", "team_id")) %>%
  mutate(score_first = as.factor(score_first))

single_count_game_ids <-
  score_first_game_lines %>%
  group_by(game_id) %>%
  count() %>%
  filter(n == 1)

score_first_game_lines <-
  score_first_game_lines %>%
  filter(!game_id %in% single_count_game_ids$game_id)

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
            team_aortg, team_adrtg, team_pace, opp_aortg, opp_adrtg, opp_pace,
            open_bookmaker_moneyline_line, open_bookmaker_implied_pts, open_bookmaker_total_line,
            close_bookmaker_moneyline_line, close_bookmaker_implied_pts,
            close_bookmaker_total_line,
            open_bookmaker_implied_pts_allowed, close_bookmaker_implied_pts_allowed,
            open_bookmaker_spread_line,
            team_anet, 
            team_games_played, 
            opp_anet, 
            opp_games_played
            ))

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
test_df_score_first_master <- tibble()

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
  
  preds <- function_to_run_models(score_first_train_model, score_first_test_model, "all")

  test_df_score_first_master = rbind.data.frame(test_df_score_first_master, preds)
}

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

brier_score_test_df_loop <-
  test_df_score_first_master_all_combined %>%
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
  test_df_score_first_master_all_combined %>%
  mutate(exp_win_prob = cut(score_first_prob_all, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(obs = n(),
            true_score_first_percent = mean(score_first),
            .groups = 'drop')

iterative_auc_loop <- roc(score_first ~ score_first_prob_iterative, data = test_df_score_first_master_all_combined)
iterative_auc_loop$auc
plot(iterative_auc_loop)

ranger_auc_loop <- roc(score_first ~ score_first_prob_ranger, data = test_df_score_first_master_all_combined)
ranger_auc_loop$auc
plot(ranger_auc_loop)

glmnet_auc_loop <- roc(score_first ~ score_first_prob_glmnet, data = test_df_score_first_master_all_combined)
glmnet_auc_loop$auc
plot(glmnet_auc_loop)

earth_auc_loop <- roc(score_first ~ score_first_prob_earth, data = test_df_score_first_master_all_combined)
earth_auc_loop$auc
plot(earth_auc_loop)

all_auc_loop <- roc(score_first ~ score_first_prob_all, data = test_df_score_first_master_all_combined)
all_auc_loop$auc
plot(all_auc_loop)

one_row_per_game_home <-
  test_df_score_first_master_all_combined %>%
  filter(home == TRUE) %>%
  drop_na() %>%
  select(season, game_date, game_id, matchup, home_score_first = score_first,
         score_first_prob_iterative, score_first_prob_ranger, score_first_prob_glmnet,
         score_first_prob_earth, score_first_prob_all)

one_row_per_game_away <-
  test_df_score_first_master_all_combined %>%
  filter(home == FALSE) %>%
  drop_na() %>%
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
         final_score_first_prob_all = (score_first_prob_all + (1 - score_first_prob_all_opp)) / 2) %>%
  drop_na()

brier_score_test_df_loop <-
  one_row_per_game_joined %>%
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

## Join back to entire data set
final_output <-
  model_ml %>%
  mutate(game_date = as.Date(game_date),
         game_id = as.character(as.integer(game_id))) %>%
  left_join(one_row_per_game_joined, by = c("season", "game_date", "game_id", "matchup", "home_score_first")) %>%
  drop_na() %>%
  select(-c(score_first_prob_iterative, score_first_prob_ranger, score_first_prob_glmnet,
            score_first_prob_earth, score_first_prob_all,
            score_first_prob_iterative_opp, score_first_prob_ranger_opp, score_first_prob_glmnet_opp,
            score_first_prob_earth_opp, score_first_prob_all_opp,
            final_score_first_prob_iterative, final_score_first_prob_ranger, final_score_first_prob_glmnet,
            final_score_first_prob_earth))

write.csv(final_output, "data/02_curated/nba_first_to_score/score_first_outputs.csv.gz", row.names = FALSE)












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
#   mutate(brier_score_score_first_iterative = (iterative_win_tip - won_tip)^2,
#          brier_score_win_tip_ranger = (win_tip_prob_ranger - won_tip)^2,
#          brier_score_win_tip_glmnet = (win_tip_prob_glmnet - won_tip)^2,
#          brier_score_win_tip_earth = (win_tip_prob_earth - won_tip)^2,
#          brier_score_win_tip_all = (win_tip_prob_all - won_tip)^2)
# 
# message("brier score of iterative score first is equal to ", round(mean(brier_score_test_df$brier_score_score_first_iterative), 5))
# message("brier score of ranger score first is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_ranger), 5))
# message("brier score of glmnet score first is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_glmnet), 5))
# message("brier score of earth score first is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_earth), 5))
# message("brier score of all score first is equal to ", round(mean(brier_score_test_df$brier_score_win_tip_all), 5))
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



