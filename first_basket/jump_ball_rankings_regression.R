library(tidyverse)
library(randomForest)

# Parameters

# Example - for full run where you use all history and start the test data at 2019-2020 season
# earliest_train_data_date <- "2016-09-01"
# train_test_date_split <- "2018-09-01"

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2016-09-01"
# Setting date we want to start logging test data on 
# Default uses today's date so that all completed games are used in calcualting ratings
train_test_date_split <- "2018-09-01"

# Read in all player csvs
file_list <- list.files(path="./data/nba_player_info/")
player_info_df <- tibble()

for (i in 1:length(file_list)){
  # Read in file for each player
  temp_player <- read.csv(paste0("./data/nba_player_info/", file_list[i]))
  # Bind the new data to the dataset
  player_info_df <- rbind.data.frame(player_info_df, temp_player) 
}

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
  rbind.data.frame(gamelogs_16_17, 
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
  pbp <- 
    read.csv(csv_path, 
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(grepl("jump ball", tolower(HOMEDESCRIPTION))) %>%
    mutate(opening_jump = if_else(PERIOD == "1" & PCTIMESTRING == "12:00", TRUE, FALSE))
  
  home_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(pbp$PLAYER1_NAME)
  home_team_person_id <- as.character(pbp$PLAYER1_ID)
  away_team_abbrev <- as.character(pbp$PLAYER2_TEAM_ABBREVIATION)
  away_team_jumper <- as.character(pbp$PLAYER2_NAME)
  away_team_person_id <- as.character(pbp$PLAYER2_ID)
  possession <- as.character(pbp$PLAYER3_TEAM_ABBREVIATION)
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
    possession = possession
  )
  
  possession_list[[g]] <- output
  
}

# Removing cases where first possession could not be determined from pbp
# Adding indicator if home team won tip
possession_binded <- 
  bind_rows(possession_list) %>%
  filter(!is.na(possession)) %>%
  mutate(home_won_tip = if_else(home_team_abbrev == possession, 1, 0))
  
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
         possession, height_diff, home_won_tip)

possession_df <- na.omit(possession_df)

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
           #game_date >= unique_dates$game_date[i]) #- lubridate::years(2))
  
  # Table that stores win rates for each jumper
  jump_balls_df <-
    possession_train %>%
    select(jumper = home_team_jumper, team_abbrev = home_team_abbrev, opening_jump, home_won_tip) %>%
    bind_rows(possession_train %>% 
                select(jumper = away_team_jumper, team_abbrev = away_team_abbrev, opening_jump, home_won_tip) %>%
                #Reverse home_win_tip for away players
                mutate(home_won_tip = if_else(home_won_tip == TRUE, FALSE, TRUE))) %>%
    group_by(jumper) %>%
    summarise(jumps = n(), 
              wins = sum(home_won_tip), 
              win_rate = wins/jumps,
              .groups = 'drop')
  
  jump_balls_for_join <-
    jump_balls_df %>%
    select(jumper, jumps, win_rate)
  
  # For each jump, adding both player's overall win rate and total jump count
  rating_loop_df_prep <-
    possession_train %>%
    left_join(jump_balls_for_join, by = c("home_team_jumper" = "jumper")) %>%
    rename(home_win_rate = win_rate, home_jumps = jumps) %>%
    left_join(jump_balls_for_join, by = c("away_team_jumper" = "jumper")) %>%
    rename(away_win_rate = win_rate, away_jumps = jumps)
  
  # Creating new dataframe for iterative process, run this line when restarting iteration to reset dataframe
  rating_loop_df <- 
    rating_loop_df_prep %>%
    select(home_won_tip, home_team_height, home_win_rate, home_jumps, away_team_height, away_win_rate, away_jumps, height_diff)
  
  possession_test <-
    possession_df %>%
    filter(game_date == unique_dates$game_date[i]) %>%
    left_join(jump_balls_for_join, by = c("home_team_jumper" = "jumper")) %>%
    rename(home_jumps = jumps, home_win_rate = win_rate) %>%
    left_join(jump_balls_for_join, by = c("away_team_jumper" = "jumper")) %>%
    rename(away_win_rate = win_rate, away_jumps = jumps) %>%
    select(home_won_tip, home_team_height, home_win_rate, home_jumps, away_team_height, away_win_rate, away_jumps, height_diff) %>%
    filter(!is.na(away_win_rate))
  
  # model_1 <- glm(home_won_tip ~ home_team_height + home_win_rate*home_jumps + away_team_height + away_win_rate*away_jumps + height_diff, 
  #              data = rating_loop_df, family = "binomial")
  # 
  # model_2 <- glm(home_won_tip ~ home_win_rate*home_jumps  + away_win_rate*away_jumps + height_diff, 
  #              data = rating_loop_df, family = "binomial")
  
  model <- randomForest(home_won_tip ~ home_team_height + home_win_rate + home_jumps + away_team_height + away_win_rate + away_jumps + height_diff, 
                        data = rating_loop_df, norm.values = TRUE)
  
  # model_3 <- randomForest(home_won_tip ~ home_team_height + home_win_rate + home_jumps + away_team_height + away_win_rate + away_jumps + height_diff, 
  #                         data = rating_loop_df, 
  #                         xtest = names(possession_test[,names(possession_test)!='home_won_tip'],
  #                         ytest = as.factor(possession_test[,'home_won_tip']),
  #                         "prob") #norm.votes = TRUE)
  
  train_results <- cbind.data.frame(rating_loop_df, home_exp_win = predict(model, newdata = rating_loop_df, type = 'response'))
  
  fitted_results <- cbind.data.frame(possession_test, home_exp_win = predict(model, newdata = possession_test, type = 'response'))
  
  # Viewing performance of fitted win probabilities by expected win proabibility buckets (every 10 percent are grouped)
  train_buckets <-
    train_results %>%
    mutate(exp_win_prob = cut(home_exp_win, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  
  test_df_exp_win_master <- rbind.data.frame(test_df_exp_win_master, fitted_results)
  
}

# Views performance of model on test data
# Is split into buckets of predicted win probability, and compares to performance of those predictions
# Ideally, the average win percentage of a bucket should match the bucket it is in
test_buckets <-
  test_df_exp_win_master %>%
  mutate(exp_win_prob = cut(home_exp_win, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(home_won_tip),
            .groups = 'drop') 

# Determines Brier Score for backtesting
brier_score_df <-
  test_df_exp_win_master %>%
  mutate(brier_score = (home_won_tip - home_exp_win)^2) %>%
  filter(!is.na(brier_score))

brier_score <- mean(brier_score_df$brier_score)
message("brier_score is equal to ", round(brier_score, 2))

brier_buckets <-
  brier_score_df %>%
  mutate(home_jumps_buckets = cut(home_jumps, breaks = seq(0, 250, by = 10), right = FALSE),
         away_jumps_buckets = cut(away_jumps, breaks = seq(0, 250, by = 10), right = FALSE)) %>%
  group_by(home_jumps_buckets, away_jumps_buckets) %>%
  summarise(jumps = n(),
            brier_score = mean(brier_score),
            .groups = 'drop') 


