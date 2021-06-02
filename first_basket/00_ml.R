library(tidyverse)
library(lubridate)
library(data.table)
library(markovchain)
library(reshape2)
library(scales)
library(recipes)
library(ranger)
library(pROC)
library(glmnet)
library(earth)

# Parameters

# Example - for full run where you use all history and start the test data at 2019-2020 season
# earliest_train_data_date <- "2015-09-01"
# train_test_date_split <- "2017-09-01"

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2015-09-01"
# Setting date we want to start logging test data on
# Default uses today's date so that all completed games are used in calculating ratings
train_test_date_split <- "2017-09-01"

#model_markov <- fread("data/02_curated/nba_first_to_score/model_markov.csv.gz")
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

# Relevant gamelogs
gamelogs_15_16 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2015-16.csv',
                           colClasses = 'character')
gamelogs_16_17 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2016-17.csv',
                           colClasses = 'character')
gamelogs_17_18 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2017-18.csv',
                           colClasses = 'character')
gamelogs_18_19 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2018-19.csv',
                           colClasses = 'character')
gamelogs_19_20 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2019-20.csv',
                           colClasses = 'character')
gamelogs_20_21 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv',
                           colClasses = 'character')

gamelogs <-
  rbind.data.frame(gamelogs_15_16,
                   gamelogs_16_17,
                   gamelogs_17_18,
                   gamelogs_18_19,
                   gamelogs_19_20,
                   gamelogs_20_21)

# Filtering gamelogs to only include data since desired train data start date
gamelogs <-
  gamelogs %>%
  filter(GAME_DATE >= earliest_train_data_date)

unique_games <- unique(gamelogs$GAME_ID)

# Initializing list
possession_list <- list()

home_tip_win_parameter <- .508

for (g in unique_games) {
  
  # Unique game in loop
  gamelog <-
    gamelogs %>%
    filter(GAME_ID == g)
  
  # Relevant info of game
  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])
  
  csv_path <- paste0('./data/nba_pbp/', g, '.csv')
  
  # Finding all jump balls in game, noting if it is opening tip
  pbp_jumps <-
    read.csv(csv_path,
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(grepl("jump ball", tolower(HOMEDESCRIPTION)) | HOMEDESCRIPTION == " ") %>%
    mutate(opening_jump = if_else(PERIOD == "1" & PCTIMESTRING == "12:00", TRUE, FALSE))
  
  pbp <-
    pbp_jumps %>%
    left_join(team_list, by = c("PLAYER3_ID" = "TEAM_ID")) %>%
    mutate(PLAYER3_TEAM_ABBREVIATION = coalesce(PLAYER3_TEAM_ABBREVIATION, ABBREVIATION)) %>%
    select(-ABBREVIATION)
  
  score_first_df <-
    read.csv(csv_path,
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(SCORE != '' & !is.na(SCORE) & !is.na(EVENTNUM)) %>%
    filter(row_number() == min(row_number()))
  
  home_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(pbp$PLAYER1_NAME)
  home_team_person_id <- as.character(pbp$PLAYER1_ID)
  away_team_abbrev <- as.character(pbp$PLAYER2_TEAM_ABBREVIATION)
  away_team_jumper <- as.character(pbp$PLAYER2_NAME)
  away_team_person_id <- as.character(pbp$PLAYER2_ID)
  possession <- as.character(pbp$PLAYER3_TEAM_ABBREVIATION)
  score_first <- as.character(score_first_df$PLAYER1_TEAM_ABBREVIATION)
  opening_jump <- as.character(pbp$opening_jump)
  
  # Final output of relevant info about jumps
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    opening_jump = opening_jump,
    home_team_jumper = home_team_jumper,
    home_team_person_id = home_team_person_id,
    home_team_abbrev = home_team_abbrev,
    away_team_jumper = away_team_jumper,
    away_team_person_id = away_team_person_id,
    away_team_abbrev = away_team_abbrev,
    possession = possession,
    score_first = score_first
  )
  
  possession_list[[g]] <- output
  
}

# Removing cases where first possession could not be determined from pbp
# Adding indicator if home team won tip
possession_binded <-
  bind_rows(possession_list) %>%
  filter(!is.na(possession)) %>%
  mutate(home_won_tip = if_else(home_team_abbrev == possession, TRUE, FALSE),
         home_score_first = if_else(home_team_abbrev == score_first, TRUE, FALSE))

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
  select(season, game_date, game_id, matchup,
         person_id = home_team_person_id, jumper = home_team_jumper, height = home_team_height, weight = home_team_weight, team_abbrev = home_team_abbrev,
         opp_person_id = away_team_person_id, opp_jumper = away_team_jumper, opp_height = away_team_height, opp_weight = away_team_weight, opp_team_abbrev = away_team_abbrev,
         opening_jump, possession, height_diff = home_height_diff, weight_diff = home_weight_diff, won_tip = home_won_tip, score_first = home_score_first) %>%
  mutate(home = TRUE)
  
possession_away <-
  possession_player_info %>%
  select(season, game_date, game_id, matchup,
         person_id = away_team_person_id, jumper = away_team_jumper, height = away_team_height, weight = away_team_weight, team_abbrev = away_team_abbrev,
         opp_person_id = home_team_person_id, opp_jumper = home_team_jumper, opp_height = home_team_height, opp_weight = home_team_weight, opp_team_abbrev = home_team_abbrev,
         opening_jump, possession, height_diff = home_height_diff, weight_diff = home_weight_diff, won_tip = home_won_tip, score_first = home_score_first) %>%
  mutate(home = FALSE) %>%
  mutate(height_diff = -height_diff,
         weight_diff = -weight_diff,
         won_tip = !won_tip,
         score_first = !score_first)

possession_df <-
  rbind.data.frame(possession_home, possession_away)

rollmean_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    mean,
    .before = p,
    ...
  )
}

### Add TOP for order?
### Add flag for center court tip vs mid game jump (start + OT)
possession_df_vars <-
  possession_df %>%
  arrange(jumper, game_date) %>%
  group_by(jumper) %>%
  mutate(jumps = replace_na(lag(row_number()), 0),
         all_jumps_avg = lag(cummean(won_tip)),
         last_10_avg = lag(rollmean_p(won_tip, 9, .complete = FALSE)),
         last_25_avg = lag(rollmean_p(won_tip, 24, .complete = FALSE)),
         last_50_avg = lag(rollmean_p(won_tip, 49, .complete = FALSE)),
         last_75_avg = lag(rollmean_p(won_tip, 74, .complete = FALSE)),
         last_100_avg = lag(rollmean_p(won_tip, 99, .complete = FALSE))) %>%
  arrange(opp_jumper, game_date) %>%
  group_by(opp_jumper) %>%
  mutate(opp_jumps = replace_na(lag(row_number()), 0),
         opp_all_jumps_avg = lag(cummean(!won_tip)),
         opp_last_10_avg = lag(rollmean_p(!won_tip, 9, .complete = FALSE)),
         opp_last_25_avg = lag(rollmean_p(!won_tip, 24, .complete = FALSE)),
         opp_last_50_avg = lag(rollmean_p(!won_tip, 49, .complete = FALSE)),
         opp_last_75_avg = lag(rollmean_p(!won_tip, 74, .complete = FALSE)),
         opp_last_100_avg = lag(rollmean_p(!won_tip, 99, .complete = FALSE))) %>%
  ungroup() %>%
  mutate(won_tip = as.factor(won_tip))


### Adding data from other models
model_iterative_split <-
  model_iterative %>%
  select(season, game_date, game_id, matchup, opening_jump, person_id = home_team_person_id, 
         team_abbrev = home_team_abbrev,
         opp_person_id = away_team_person_id, 
         opp_team_abbrev = away_team_abbrev, 
         iterative_rating = home_exp_win_adj, 
         opp_iterative_rating = away_exp_win_adj,
         iterative_win_tip = final_exp_win_adj) %>%
  bind_rows(
    model_iterative %>%
    select(season, game_date, game_id, matchup, opening_jump, person_id = away_team_person_id, 
           team_abbrev = away_team_abbrev, opp_person_id = home_team_person_id, 
           opp_team_abbrev = home_team_abbrev,
           iterative_rating = away_exp_win_adj, 
           opp_iterative_rating = home_exp_win_adj,
           iterative_win_tip = final_exp_win_adj) %>%
    mutate(iterative_win_tip = 1 - iterative_win_tip)) %>%
  mutate(game_id = as.character(game_id),
         game_date = as.Date(game_date),
         person_id = as.character(person_id),
         opp_person_id = as.character(opp_person_id))

# model_markov_split <-
#   model_markov %>%
#   select(season, game_date, game_id, matchup, opening_jump, person_id = home_team_person_id, 
#          team_abbrev = home_team_abbrev, opp_person_id = away_team_person_id, 
#          opp_team_abbrev = away_team_abbrev,
#          markov_rating = home_team_rating, 
#          opp_markov_rating = away_team_rating, 
#          #markov_height_rating = home_height_rating,
#          #opp_markov_height_rating = away_height_rating, 
#          markov_win_tip = win_tip_prob) %>%
#   bind_rows(
#     model_markov %>%
#       select(season, game_date, game_id, matchup, opening_jump, person_id = away_team_person_id, 
#              team_abbrev = away_team_abbrev, opp_person_id = home_team_person_id, 
#              opp_team_abbrev = home_team_abbrev,
#              markov_rating = away_team_rating, 
#              opp_markov_rating = home_team_rating,
#              #markov_height_rating = away_height_rating,
#              #opp_markov_height_rating = home_height_rating,
#              markov_win_tip = win_tip_prob) %>%
#       mutate(markov_win_tip = 1 - markov_win_tip)) %>%
#   mutate(game_id = as.character(game_id),
#          game_date = as.Date(game_date),
#          person_id = as.character(person_id),
#          opp_person_id = as.character(opp_person_id))

possession_models_joined <-
  possession_df_vars %>%
  mutate(game_id = as.character(as.integer(game_id)),
         person_id = as.character(as.integer(person_id)),
         opp_person_id = as.character(as.integer(opp_person_id)),
         opening_jump = as.logical(opening_jump)) %>%
  left_join(model_iterative_split) %>%
  #left_join(model_markov_split) %>%
  distinct()


################# Day by Day Run #####################
possession_relevant_dates <-
  possession_models_joined %>%
  filter(game_date >= '2017-09-01') %>%
  drop_na()

# Set of dates to loop through for test data
unique_dates <-
  possession_relevant_dates %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  filter(game_date >= '2018-09-01')

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
    possession_relevant_dates %>%
    filter(game_date < unique_dates$game_date[i]) %>%
    select(-c(season, game_date, game_id, matchup, person_id, team_abbrev, 
              opp_person_id, opp_team_abbrev, possession, score_first))
  
  possession_test <-
    possession_relevant_dates %>%
    filter(game_date == unique_dates$game_date[i]) %>%
    select(-c(season, game_date, game_id, matchup, person_id, team_abbrev, 
              opp_person_id, opp_team_abbrev, possession, score_first))

  rec_loop <-
    recipe(possession_train) %>%
    update_role(won_tip, new_role = 'outcome') %>%
    update_role(-all_outcomes(), new_role = 'predictor') %>%
    update_role(ends_with('jumper'), new_role = 'jumper') %>%
    prep()
  
  baked_train_loop <- bake(rec_loop, possession_train)
  baked_test_loop <- bake(rec_loop, possession_test)
  
  ## subset predictors
  predictor_vars_loop <- rec_loop$term_info$variable[rec_loop$term_info$role == 'predictor']
  
  predictors_train_loop <- 
    baked_train_loop %>%
    select(any_of(predictor_vars_loop))
  
  predictors_test_loop <-
    baked_test_loop %>%
    select(any_of(predictor_vars_loop))
  
  ## subset outcomes
  outcome_vars_loop <- rec_loop$term_info$variable[rec_loop$term_info$role == 'outcome']
  outcomes_loop <- 
    baked_train_loop %>%
    select(any_of(outcome_vars_loop))
  
  mod_ranger_loop <- ranger(x = predictors_train_loop, y = outcomes_loop$won_tip, probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)
  
  preds_ranger_loop <- 
    cbind.data.frame(baked_test_loop, predict(mod_ranger_loop, predictors_test_loop, type = 'response')$predictions) %>%
    rename(win_tip_prob_ranger = `TRUE`) %>%
    select(-c(`FALSE`)) %>%
    mutate(won_tip = as.logical(won_tip))
  
  mod_glmnet_loop <- cv.glmnet(x = as.matrix(predictors_train_loop), y = outcomes_loop$won_tip, family = 'binomial')
  
  preds_glmnet_loop <- 
    cbind.data.frame(baked_test_loop, win_tip_prob_glmnet = as.numeric(predict(mod_glmnet_loop, as.matrix(predictors_test_loop), type = 'response'))) %>%
    mutate(won_tip = as.logical(won_tip))
  
  mod_earth_loop <- earth(x = predictors_train_loop, y = outcomes_loop$won_tip, degree = 2, glm = list(family = binomial), nfold = 3)
  
  preds_earth_loop <- 
    cbind.data.frame(baked_test_loop, predict(mod_earth_loop, predictors_test_loop, type = 'response')) %>%
    rename(win_tip_prob_earth = `TRUE`) %>%
    mutate(won_tip = as.logical(won_tip))
  
  preds_binded_loop <-
    cbind.data.frame(preds_ranger_loop, preds_glmnet_loop$win_tip_prob_glmnet, preds_earth_loop$win_tip_prob_earth)

  test_df_exp_win_master = rbind.data.frame(test_df_exp_win_master, preds_binded_loop)
  
  preds_win_prob <-
    preds_binded_loop
  
  rec_loop <-
    recipe(possession_train) %>%
    update_role(won_tip, new_role = 'outcome') %>%
    update_role(-all_outcomes(), new_role = 'predictor') %>%
    update_role(ends_with('jumper'), new_role = 'jumper') %>%
    prep()

}

test_df_exp_win_master_renamed <-
  test_df_exp_win_master %>%
  rename(win_tip_prob_glmnet = `preds_glmnet_loop$win_tip_prob_glmnet`,
         win_tip_prob_earth = `preds_earth_loop$win_tip_prob_earth`)

# Currently not including markov
test_df_exp_win_master_all_combined <- 
  test_df_exp_win_master_renamed %>%
  rowwise() %>%
  mutate(win_tip_prob_all = mean(c(#iterative_win_tip, #markov_win_tip,
                                   win_tip_prob_ranger, win_tip_prob_glmnet,
                                   win_tip_prob_earth)))

brier_score_test_df_loop <-
  test_df_exp_win_master_all_combined %>%
  mutate(brier_score_iterative = (iterative_win_tip - won_tip)^2,
         #brier_score_markov = (markov_win_tip - won_tip)^2,
         brier_score_win_tip_ranger = (win_tip_prob_ranger - won_tip)^2,
         brier_score_win_tip_glmnet = (win_tip_prob_glmnet - won_tip)^2,
         brier_score_win_tip_earth = (win_tip_prob_earth - won_tip)^2,
         brier_score_win_tip_all = (win_tip_prob_all - won_tip)^2)

message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_iterative), 5))
#message("brier score of markov win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_markov), 5))
message("brier score of ranger win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_ranger), 5))
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

# markov_auc_loop <- roc(won_tip ~ markov_win_tip, data = test_df_exp_win_master_all_combined)
# markov_auc_loop$auc
# plot(markov_auc_loop)

ranger_auc_loop <- roc(won_tip ~ win_tip_prob_ranger, data = test_df_exp_win_master_all_combined)
ranger_auc_loop$auc
plot(ranger_auc_loop)

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
  select(jumper, opp_jumper, won_tip, jumps, opp_jumps, 
         iterative_win_tip, win_tip_prob_ranger, win_tip_prob_glmnet,
         win_tip_prob_earth, win_tip_prob_all)

one_row_per_jump_away <-
  test_df_exp_win_master_all_combined %>%
  filter(home == FALSE) %>%
  drop_na() %>%
  select(jumper, opp_jumper, jumps, opp_jumps, 
         iterative_win_tip, win_tip_prob_ranger, win_tip_prob_glmnet,
         win_tip_prob_earth, win_tip_prob_all) %>%
  rename(opp_jumper = jumper,
         jumper = opp_jumper,
         opp_jumps = jumps,
         jumps = opp_jumps,
         iterative_win_tip_opp = iterative_win_tip,
         win_tip_prob_ranger_opp = win_tip_prob_ranger,
         win_tip_prob_glmnet_opp = win_tip_prob_glmnet,
         win_tip_prob_earth_opp = win_tip_prob_earth,
         win_tip_prob_all_opp = win_tip_prob_all) 

one_row_per_jump_joined <-
  one_row_per_jump_home %>%
  left_join(one_row_per_jump_away) %>%
  mutate(final_win_prob_iterative = (iterative_win_tip + (1 - iterative_win_tip_opp)) / 2,
         final_win_prob_ranger = (win_tip_prob_ranger + (1 - win_tip_prob_ranger_opp)) / 2,
         final_win_prob_glment = (win_tip_prob_glmnet + (1 - win_tip_prob_glmnet_opp)) / 2,
         final_win_prob_earth = (win_tip_prob_earth + (1 - win_tip_prob_earth_opp)) / 2,
         final_win_prob_all = (win_tip_prob_all + (1 - win_tip_prob_all_opp)) / 2) %>%
  drop_na()

brier_score_test_df_loop <-
  one_row_per_jump_joined %>%
  mutate(brier_score_iterative = (final_win_prob_iterative - won_tip)^2,
         brier_score_win_tip_ranger = (final_win_prob_ranger - won_tip)^2,
         brier_score_win_tip_glmnet = (final_win_prob_glment - won_tip)^2,
         brier_score_win_tip_earth = (final_win_prob_earth - won_tip)^2,
         brier_score_win_tip_all = (final_win_prob_all - won_tip)^2)

message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_iterative), 5))
message("brier score of ranger win tips is equal to ", round(mean(brier_score_test_df_loop$brier_score_win_tip_ranger), 5))
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

ranger_auc_loop <- roc(won_tip ~ win_tip_prob_ranger, data = one_row_per_jump_joined)
ranger_auc_loop$auc
plot(ranger_auc_loop)

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





#### add current season jump count field







########################### Random Train/Test split ######################################

possession_final_cols <-
  possession_models_joined %>%
  select(-c(season, game_date, game_id, matchup, person_id, team_abbrev, 
            opp_person_id, opp_team_abbrev, possession, score_first))

## Adding random number for training
set.seed(051221)
possession_final_df <-
  possession_final_cols %>%
  rowwise() %>%
  mutate(train_or_test = if_else(runif(1) > .8, "Test", "Train")) %>%
  drop_na()

training_data <-
  possession_final_df %>%
  filter(train_or_test == "Train") %>%
  select(-train_or_test)

testing_data <-
  possession_final_df %>%
  filter(train_or_test == "Test") %>%
  select(-train_or_test)

rec <-
  recipe(training_data) %>%
  update_role(won_tip, new_role = 'outcome') %>%
  update_role(-all_outcomes(), new_role = 'predictor') %>%
  update_role(ends_with('jumper'), new_role = 'jumper') %>%
  prep()

baked_train <- bake(rec, training_data)
baked_test <- bake(rec, testing_data)

## subset predictors
predictor_vars <- rec$term_info$variable[rec$term_info$role == 'predictor']

predictors_train <- 
  baked_train %>%
  select(any_of(predictor_vars))

predictors_test <-
  baked_test %>%
  select(any_of(predictor_vars))

## subset outcomes
outcome_vars <- rec$term_info$variable[rec$term_info$role == 'outcome']

outcomes <- 
  baked_train %>%
  select(any_of(outcome_vars))

mod_ranger <- ranger(x = predictors_train, y = outcomes$won_tip, probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)

preds_ranger <- 
  cbind.data.frame(baked_test, predict(mod_ranger, predictors_test, type = 'response')$predictions) %>%
  rename(win_tip_prob = `TRUE`) %>%
  select(-c(`FALSE`)) %>%
  mutate(won_tip = as.logical(won_tip))

mod_glmnet <- cv.glmnet(x = as.matrix(predictors_train), y = outcomes$won_tip, family = 'binomial')

preds_glmnet <- 
  cbind.data.frame(baked_test, win_tip_prob = as.numeric(predict(mod_glmnet, as.matrix(predictors_test), type = 'response'))) %>%
  mutate(won_tip = as.logical(won_tip))

mod_earth <- earth(x = predictors_train, y = outcomes$won_tip, degree = 2, glm = list(family = binomial), nfold = 3)

preds_earth <- 
  cbind.data.frame(baked_test, predict(mod_earth, predictors_test, type = 'response')) %>%
  rename(win_tip_prob = `TRUE`) %>%
  mutate(won_tip = as.logical(won_tip))

test_buckets_win_tip_ranger <-
  preds_ranger %>%
  mutate(exp_win_prob = cut(win_tip_prob, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

test_buckets_win_tip_glmnet <-
  preds_glmnet %>%
  mutate(exp_win_prob = cut(win_tip_prob, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

test_buckets_win_tip_earth <-
  preds_earth %>%
  mutate(exp_win_prob = cut(win_tip_prob, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

brier_score_test_df <-
  preds_earth %>%
  mutate(brier_score_win_tip = (win_tip_prob - won_tip)^2,
         brier_score_markov = (markov_win_tip - won_tip)^2,
         brier_score_iterative = (iterative_win_tip - won_tip)^2) %>%
  filter(!is.na(brier_score_win_tip))

message("brier score of ensemble win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip), 5))
message("brier score of markov win tips is equal to ", round(mean(brier_score_test_df$brier_score_markov), 5))
message("brier score of iterative win tips is equal to ", round(mean(brier_score_test_df$brier_score_iterative), 5))

markov_auc <- roc(won_tip ~ markov_win_tip, data = preds_earth)
markov_auc$auc
plot(markov_auc)

ensemble_auc <- roc(won_tip ~ win_tip_prob, data = preds_earth)
ensemble_auc$auc
plot(ensemble_auc)

iterative_auc <- roc(won_tip ~ iterative_win_tip, data = preds_earth)
iterative_auc$auc
plot(iterative_auc)
###






###### Model with just win probs? ########
win_prob_only_model <-
  test_df_exp_win_master_all_combined %>%
  select(jumper, opp_jumper, opening_jump, won_tip, home,
         iterative_win_tip, win_tip_prob_ranger, win_tip_prob_glmnet,
         win_tip_prob_earth, win_tip_prob_all)

## Adding random number for training
set.seed(051221)
win_prob_only_model_data_split <-
  win_prob_only_model %>%
  rowwise() %>%
  mutate(train_or_test = if_else(runif(1) > .8, "Test", "Train")) %>%
  drop_na()

training_data <-
  win_prob_only_model_data_split %>%
  filter(train_or_test == "Train") %>%
  select(-train_or_test)

testing_data <-
  win_prob_only_model_data_split %>%
  filter(train_or_test == "Test") %>%
  select(-train_or_test)

rec <-
  recipe(training_data) %>%
  update_role(won_tip, new_role = 'outcome') %>%
  update_role(-all_outcomes(), new_role = 'predictor') %>%
  update_role(ends_with('jumper'), new_role = 'jumper') %>%
  prep()

baked_train <- bake(rec, training_data)
baked_test <- bake(rec, testing_data)

## subset predictors
predictor_vars <- rec$term_info$variable[rec$term_info$role == 'predictor']

predictors_train <- 
  baked_train %>%
  select(any_of(predictor_vars))

predictors_test <-
  baked_test %>%
  select(any_of(predictor_vars))

## subset outcomes
outcome_vars <- rec$term_info$variable[rec$term_info$role == 'outcome']

outcomes <- 
  baked_train %>%
  select(any_of(outcome_vars))

mod_ranger <- ranger(x = predictors_train, y = outcomes$won_tip, probability = TRUE, importance = 'impurity_corrected', keep.inbag = TRUE)

preds_ranger <- 
  cbind.data.frame(baked_test, predict(mod_ranger, predictors_test, type = 'response')$predictions) %>%
  rename(final_win_tip_ranger = `2`) %>%
  select(-c(`1`)) %>%
  mutate(won_tip = as.logical(won_tip))

mod_glmnet <- cv.glmnet(x = as.matrix(predictors_train), y = outcomes$won_tip, family = 'binomial')

preds_glmnet <- 
  cbind.data.frame(baked_test, final_win_tip_glmnet = as.numeric(predict(mod_glmnet, as.matrix(predictors_test), type = 'response'))) %>%
  mutate(won_tip = as.logical(won_tip))

mod_earth <- earth(x = predictors_train, y = outcomes$won_tip, degree = 2, glm = list(family = binomial), nfold = 3)

preds_earth <- 
  cbind.data.frame(baked_test, predict(mod_earth, predictors_test, type = 'response')) %>%
  rename(final_win_tip_earth = `outcomes$won_tip`) %>%
  mutate(won_tip = as.logical(won_tip))

test_buckets_win_tip_ranger <-
  preds_ranger %>%
  mutate(exp_win_prob = cut(final_win_tip_ranger, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

test_buckets_win_tip_glmnet <-
  preds_glmnet %>%
  mutate(exp_win_prob = cut(final_win_tip_glmnet, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

test_buckets_win_tip_earth <-
  preds_earth %>%
  mutate(exp_win_prob = cut(final_win_tip_earth, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(won_tip),
            .groups = 'drop')

brier_score_test_df <-
  preds_earth %>%
  mutate(brier_score_win_tip = (final_win_tip_earth - won_tip)^2,
         brier_score_iterative = (iterative_win_tip - won_tip)^2) %>%
  filter(!is.na(brier_score_win_tip))

message("brier score of ensemble win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip), 5))



