library(tidyverse)
library(lubridate)
library(data.table)
library(markovchain)
library(reshape2)
library(scales)

# Parameters

# Example - for full run where you use all history and start the test data at 2019-2020 season
# earliest_train_data_date <- "2015-09-01"
# train_test_date_split <- "2018-09-01"

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2015-09-01"
# Setting date we want to start logging test data on
# Default uses today's date so that all completed games are used in calcualting ratings
train_test_date_split <- "2017-09-01"

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
  mutate(height_diff = home_team_height - away_team_height)

# Organizes column order
possession_df <-
  possession_player_info %>%
  select(season, game_date, game_id, matchup, opening_jump,
         home_team_person_id, home_team_jumper, home_team_height, home_team_weight, home_team_abbrev,
         away_team_person_id, away_team_jumper, away_team_height, away_team_weight, away_team_abbrev,
         possession, height_diff, home_won_tip, home_score_first) %>%
  filter(!is.na(home_team_height),
         !is.na(away_team_height))

# Set of dates to loop through for test data
unique_dates <-
  possession_df %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  filter(game_date >= train_test_date_split)

# If no values in unique_dates (i.e. doing current day run instead of historical testing)
# Set unique_dates to current date so loop runs once to get player ratings
if (nrow(unique_dates) == 0) {
  unique_dates <- tibble(game_date = Sys.Date())
}

# Initializing df that stores test data predictions
test_df_exp_win_master <- tibble()

# Function that loops through every day in our test dates
# At each day, the function resets the train data to include everything up to current date
# Test data becomes the list of games for that day
for (i in 1:length(unique_dates$game_date)) {
  message(unique_dates$game_date[i])
  
  # Splitting data into train and test sets
  possession_train <-
    possession_df %>%
    filter(game_date < unique_dates$game_date[i])
  
  possession_test <-
    possession_df %>%
    filter(game_date == unique_dates$game_date[i])
  
  #### Training Data ####
  player_markov <-
    possession_train %>%
    select(player = home_team_jumper, opponent = away_team_jumper, win = home_won_tip) %>%
    bind_rows(possession_train %>%
                select(player = away_team_jumper, opponent = home_team_jumper, win = home_won_tip) %>%
                mutate(win = !win)) %>%
    group_by(player, opponent) %>%
    summarise(wins = n() + sum(win)*10000, # (1,alpha) voting scheme where alpha = 10000
              .groups = 'drop') 
  
  # Reshaping data
  player_matrix <- dcast(melt(player_markov, id = c("player", "opponent")), opponent~player)
  # Turning to matrix and setting nulls to 0
  player_matrix_df <- as.matrix(player_matrix[,-1])
  player_matrix_df[is.na(player_matrix_df)] <- 0
  # Adding row names as players
  dimnames(player_matrix_df) <- list(player_matrix[, 1], player_matrix[,1])
  
  # Adding small value so every player maps to every other player
  player_matrix_adj <- player_matrix_df + .00001
  
  # Normalizing rows so they sum to 1
  player_matrix_normalized <-player_matrix_adj/rowSums(player_matrix_adj)
  #player_matrix_normalized[is.na(player_matrix_normalized)] <- 0
  
  # Finding steady state rankings
  player_markov_test <-
    new("markovchain",
        states = colnames(player_matrix_normalized),
        transitionMatrix = player_matrix_normalized)
  
  steady_state <- t(steadyStates(player_markov_test))

  steady_state_df <- 
    tibble(
      player = rownames(steady_state),
      rating = steady_state
    )
  
  # Adding jumper rating 
  df_with_jumper_rating <-
    possession_train %>%
    left_join(steady_state_df, by = c("home_team_jumper" = "player")) %>%
    rename(home_team_rating = rating) %>%
    left_join(steady_state_df, by = c("away_team_jumper" = "player")) %>%
    rename(away_team_rating = rating)
  
  # Grabbing player, height, and rating from data
  player_height_ratings <-
    df_with_jumper_rating %>%
    distinct(jumper = home_team_jumper, height = home_team_height, rating = home_team_rating) %>%
    bind_rows(df_with_jumper_rating %>%
                distinct(jumper = away_team_jumper, height = away_team_height, rating = away_team_rating)) %>%
    distinct(jumper, height, rating)
  
  # Model to predict rating based on height
  height_model <- lm(rating ~ height, data = player_height_ratings)
  
  # Data frame that stores height rating
  height_df <- cbind.data.frame(player_height_ratings, height_rating = predict(height_model, newdata = player_height_ratings))
  
  # Adding height rating to main training df
  df_for_train <-
    df_with_jumper_rating %>%
    left_join(height_df, by = c("home_team_jumper" = "jumper", "home_team_height" = "height", "home_team_rating" = "rating")) %>%
    rename(home_height_rating = height_rating) %>%
    left_join(height_df, by = c("away_team_jumper" = "jumper", "away_team_height" = "height", "away_team_rating" = "rating")) %>%
    rename(away_height_rating = height_rating)
  
  # Model to predict chance of winning tip
  model_win_tip <- glm(home_won_tip ~ home_team_rating + away_team_rating +
                         home_height_rating + away_height_rating + 
                         home_team_weight + away_team_weight,
                       data = df_for_train, 
                       family = 'binomial')
  
  model_score_first <- glm(home_score_first ~ home_team_rating + away_team_rating + 
                             home_height_rating + away_height_rating + 
                             home_team_weight + away_team_weight,
                           data = df_for_train, 
                           family = 'binomial')
  
  predictions_train <-
    cbind.data.frame(df_for_train, 
                     win_tip_prob = predict(model_win_tip, newdata = df_for_train, type = 'response'),
                     score_first_prob = predict(model_score_first, newdata = df_for_train, type = 'response'))
  
  train_buckets <-
    predictions_train %>%
    mutate(exp_win_prob = cut(win_tip_prob, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  
  # Determines Brier Score for win tip backtesting
  brier_score_train_df <-
    predictions_train %>%
    mutate(brier_score_win_tip = (win_tip_prob - home_won_tip)^2,
           brier_score_score_first= (score_first_prob - home_score_first)^2) %>%
    filter(!is.na(brier_score_win_tip), !is.na(brier_score_score_first))
  
  brier_score_win_tip <- mean(brier_score_train_df$brier_score_win_tip)
  message("brier score of win tips is equal to ", round(brier_score_win_tip, 5))
  
  brier_score_score_first <- mean(brier_score_train_df$brier_score_score_first)
  message("brier score of score first is equal to ", round(brier_score_score_first, 5))
  
  #### Testing Data ####
  df_for_test <-
    possession_test %>%
    left_join(steady_state_df, by = c("home_team_jumper" = "player")) %>%
    rename(home_team_rating = rating) %>%
    left_join(steady_state_df, by = c("away_team_jumper" = "player")) %>%
    rename(away_team_rating = rating) %>%
    left_join(height_df, by = c("home_team_jumper" = "jumper", "home_team_height" = "height", "home_team_rating" = "rating")) %>%
    rename(home_height_rating = height_rating) %>%
    left_join(height_df, by = c("away_team_jumper" = "jumper", "away_team_height" = "height", "away_team_rating" = "rating")) %>%
    rename(away_height_rating = height_rating)
  
  predictions_today <-
    cbind.data.frame(df_for_test, 
                     win_tip_prob = predict(model_win_tip, newdata = df_for_test, type = 'response'),
                     score_first_prob = predict(model_score_first, newdata = df_for_test, type = 'response'))
  
  test_df_exp_win_master <- rbind.data.frame(test_df_exp_win_master, predictions_today)
  
}

# Adding score first percentage (given tip win prediction)
test_df_exp_win_master <-
  test_df_exp_win_master %>%
  filter(!is.na(win_tip_prob)) %>%
  mutate(exp_score_first = (win_tip_prob*.61) + ((1 - win_tip_prob)*.41))

write.csv(test_df_exp_win_master, "data/02_curated/nba_first_to_score/model_markov.csv.gz", row.names = FALSE)

# # Views performance of model on test data
# # Is split into buckets of predicted win probability, and compares to performance of those predictions
# # Ideally, the average win percentage of a bucket should match the bucket it is in
# test_buckets_win_tip <-
#   test_df_exp_win_master %>%
#   mutate(exp_win_prob = cut(win_tip_prob, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
#   group_by(exp_win_prob) %>%
#   summarise(jumps = n(),
#             true_win_percent = mean(home_won_tip),
#             .groups = 'drop')
# 
# test_buckets_score_model <-
#   test_df_exp_win_master %>%
#   mutate(exp_score_first_prob = cut(score_first_prob, breaks = seq(0, 100, by = 0.025), right = FALSE)) %>%
#   group_by(exp_score_first_prob) %>%
#   summarise(jumps = n(),
#             tip_win_percent = mean(home_won_tip),
#             first_score_percent = mean(home_score_first),
#             .groups = 'drop')
# 
# test_buckets_score_myself <-
#   test_df_exp_win_master %>%
#   mutate(exp_score_first_prob = cut(exp_score_first, breaks = seq(0, 100, by = 0.025), right = FALSE)) %>%
#   group_by(exp_score_first_prob) %>%
#   summarise(jumps = n(),
#             tip_win_percent = mean(home_won_tip),
#             first_score_percent = mean(home_score_first),
#             .groups = 'drop')
# 
# 
# # Determines Brier Score on backtesting
# brier_score_test_df <-
#   test_df_exp_win_master %>%
#   mutate(brier_score_win_tip = (win_tip_prob - home_won_tip)^2,
#          brier_score_score_first_model = (score_first_prob - home_score_first)^2,
#          brier_score_score_first_myself = (exp_score_first - home_score_first)^2) %>%
#   filter(!is.na(brier_score_win_tip), !is.na(brier_score_score_first_model), !is.na(brier_score_score_first_myself))
# 
# brier_score_by_season <-
#   brier_score_test_df %>%
#   group_by(season) %>%
#   summarise(brier_score_win_tip = mean(brier_score_win_tip),
#             brier_score_score_first_model = mean(brier_score_score_first_model),
#             brier_score_score_first_myself = mean(brier_score_score_first_myself))
# 
# message("brier score of win tips is equal to ", round(mean(brier_score_test_df$brier_score_win_tip), 5))
# message("brier score of score first model is equal to ", round(mean(brier_score_test_df$brier_score_score_first_model), 5))
# message("brier score of score first myself is equal to ", round(mean(brier_score_test_df$brier_score_score_first_myself), 5))
# 
# 
# library(pROC)
# g <- roc(home_won_tip ~ win_tip_prob, data = test_df_exp_win_master)
# g$auc
# plot(g)
# 
# # Score First Rates based on Win Tip, home/away
# score_first_rates_by_season <-
#   test_df_exp_win_master %>%
#   group_by(season, home_won_tip, home_score_first) %>%
#   summarise(score_first = n()) %>%
#   group_by(season) %>%
#   mutate(season_tips = sum(score_first)) %>%
#   group_by(season, home_won_tip) %>%
#   mutate(tip_wins = sum(score_first),
#          tip_wins_pct = tip_wins/season_tips) %>%
#   ungroup() %>%
#   select(season, home_won_tip, home_score_first, season_tips, tip_wins, tip_wins_pct, score_first) %>%
#   mutate(score_first_pct = score_first/tip_wins)
# 
# score_first_rates <-
#   test_df_exp_win_master %>%
#   group_by(home_won_tip, home_score_first) %>%
#   summarise(score_first = n()) %>%
#   ungroup() %>%
#   mutate(tips = sum(score_first)) %>%
#   group_by(home_won_tip) %>%
#   mutate(tip_wins = sum(score_first),
#          tip_wins_pct = tip_wins/tips) %>%
#   ungroup() %>%
#   select(home_won_tip, home_score_first, tips, tip_wins, tip_wins_pct, score_first) %>%
#   mutate(score_first_pct = score_first/tip_wins)

# ## Write out main file
# write.csv(player_list_df, "data/02_curated/nba_first_to_score/jump_ball_ratings.csv.gz", row.names = FALSE)
# 
# ## Write out archive file
# yyyy <- as.character(year(Sys.Date()))
# mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
# dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)
# 
# if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
#   message("directory already exists")
# }else{
#   dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
# }
# 
# write.csv(player_list_df, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "jump_ball_ratings.csv.gz"), row.names = FALSE)
# 


#### SCRATCH #####
# When do player ratings stay the same?
# jumps_counts_players_home <-
#   test_df_exp_win_master %>%
#   select(person_id = home_team_person_id, jumps = home_jumps, exp_win = home_exp_win, exp_win_adj = home_exp_win_adj)

# jumps_counts_players_away <-
#   test_df_exp_win_master %>%
#   select(person_id = away_team_person_id, jumps = away_jumps, exp_win = away_exp_win, exp_win_adj = away_exp_win_adj)

# jump_counts_players <-
#   rbind.data.frame(jumps_counts_players_home, jumps_counts_players_away) %>%
#   arrange(person_id, jumps) %>%
#   distinct() %>%
#   group_by(person_id) %>%
#   mutate(difference = abs(exp_win - lag(exp_win)),
#          difference_adj = abs(exp_win_adj - lag(exp_win_adj)))

# difference_by_jumps <-
#   jump_counts_players %>%
#   group_by(jumps) %>%
#   summarise(avg_diff = mean(difference, na.rm = T),
#             avg_diff_adj = mean(difference_adj, na.rm = T))

# plot(difference_by_jumps$jumps, difference_by_jumps$avg_diff)
# plot(difference_by_jumps$jumps, difference_by_jumps$avg_diff_adj)