library(tidyverse)
library(lubridate)

# Parameters

# Example - for full run where you use all history and start the test data at 2019-2020 season
# earliest_train_data_date <- "2015-09-01"
# train_test_date_split <- "2018-09-01"

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2018-09-01"
# Setting date we want to start logging test data on 
# Default uses today's date so that all completed games are used in calcualting ratings
train_test_date_split <- Sys.Date()

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
  pbp <- 
    read.csv(csv_path, 
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(grepl("jump ball", tolower(HOMEDESCRIPTION))) %>%
    mutate(opening_jump = if_else(PERIOD == "1" & PCTIMESTRING == "12:00", TRUE, FALSE))
  
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
         home_team_person_id, home_team_jumper, home_team_height, home_team_abbrev,
         away_team_person_id, away_team_jumper, away_team_height, away_team_abbrev,
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
  
  # Table that stores win rates for each jumper
  jump_balls_df <-
    possession_train %>%
    select(jumper = home_team_jumper, team_abbrev = home_team_abbrev, opening_jump, home_won_tip) %>%
    bind_rows(possession_train %>% 
                select(jumper = away_team_jumper, team_abbrev = away_team_abbrev, opening_jump, home_won_tip) %>%
                #Reverse home_win_tip for away players
                mutate(home_won_tip = if_else(home_won_tip, FALSE, TRUE))) %>%
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
  rating_loop_df <- rating_loop_df_prep
  
  # Iterative process of updating player ratings to adjust for opponent strength
  # Output is a player's probability of beating a 50% opponent
  # Loop repeats until there is no change in ratings
  j <- 0
  change <- 1
  last_iter <- 100
  while(change > 0.00001){
    if (dim(rating_loop_df)[1] == 0) {
      break
    }
    j <- j + 1
    
    #message(j)
    #message(change)
    
    # Scores a player's jump performance for each game 
    # If player wins tip, score is opponent win rate + 0.5
    # If player loses tip, score is (opponent win rate - 1) + 0.5
    # +0.5 term helps put value on scale similar to default 0/1 (which assumes every player is 50% win rate)
    # This rewards beating good jumpers and penalizes losing to bad jumpers
    # Now, instead of 0/1 for a loss/win, is more continuous scale
    rating_loop_df <- 
      rating_loop_df %>% 
      mutate(home_win_rate_adj = away_win_rate - 1 + home_won_tip + 0.5,
             away_win_rate_adj = home_win_rate - home_won_tip + 0.5)
    
    # Table that stores win rates for each jumper
    jumper_loop_df <-
      rating_loop_df %>%
      select(jumper = home_team_jumper, jumps = home_jumps, 
             win_rate = home_win_rate, adj_win_rate = home_win_rate_adj) %>%
      bind_rows(rating_loop_df %>% 
                  select(jumper = away_team_jumper, jumps = away_jumps, 
                         win_rate = away_win_rate, adj_win_rate = away_win_rate_adj)) %>%
      group_by(jumper) %>%
      summarise(jumps = mean(jumps),
                adj_win_rate = sum(adj_win_rate)/jumps, 
                .groups = 'drop') %>%
      select(jumper, adj_win_rate)
    
    # Adding new player adj win rates to data frame
    rating_loop_df <-
      rating_loop_df %>%
      left_join(jumper_loop_df, by = c("home_team_jumper" = "jumper")) %>%
      rename(home_win_rate_new = adj_win_rate) %>%
      left_join(jumper_loop_df, by = c("away_team_jumper" = "jumper")) %>%
      rename(away_win_rate_new = adj_win_rate)
    
    # Tracking change in player ratings from previous iteration, for help with when to stop loop
    new_iter <- mean(abs(rating_loop_df$home_win_rate - rating_loop_df$home_win_rate_new))
    change <- abs(last_iter - new_iter)
    last_iter <- new_iter
    #message(change)
    
    # Updating player ratings with new adj rating, and removing those additional columns
    # This resets the data frame back to its original structure for the next loop
    rating_loop_df <- 
      rating_loop_df %>% 
      mutate(home_win_rate = home_win_rate_new, 
             away_win_rate = away_win_rate_new) %>%
      select(-c(home_win_rate_adj, away_win_rate_adj,
                home_win_rate_new, away_win_rate_new))
  }
  
  # Test min/max scaling and see if it makes any difference (if values go outside bounds)
  # Some players perform well/poorly enough to get expected win percentages outside [0,1], so they are capped here
  rating_loop_capped <-
    rating_loop_df %>%
    mutate(home_win_rate = case_when(home_win_rate > 1 ~ 1,
                                     home_win_rate < 0 ~ 0,
                                     TRUE ~ home_win_rate),
           away_win_rate = case_when(away_win_rate > 1 ~ 1,
                                     away_win_rate < 0 ~ 0,
                                     TRUE ~ away_win_rate))
  
  # Finding the expected win probability for the home jumper for each jump
  # Formula P(WPa, WPb) = (WPa(1-WPb) / (WPa(1-WPb) + WPb(1-WPa)))
  expected_win_prob <-
    rating_loop_capped %>%
    mutate(home_exp_win = (home_win_rate*(1-away_win_rate)*home_tip_win_parameter) / 
                          ((home_win_rate*(1-away_win_rate)*home_tip_win_parameter) + (away_win_rate*(1-home_win_rate)*(1-home_tip_win_parameter))))
  
  # Some player's expected win percentage is 0% against an average jumper - when they jump vs each other this leads to NaN values
  # Resetting these cases to 50%
  expected_win_prob <-
    expected_win_prob %>%
    mutate(home_exp_win = if_else(is.nan(home_exp_win), 0.508, home_exp_win))
  
  # Table that stores wins rates for each height
  height_rating_df <-
    expected_win_prob %>%
    group_by(home_team_height) %>%
    summarise(tips = n(), 
              wins = sum(home_won_tip),
              .groups = 'drop') %>%
    rename(height = home_team_height) %>%
    bind_rows(expected_win_prob %>%
                group_by(away_team_height) %>%
                summarise(tips = n(), 
                          wins = tips - sum(home_won_tip),
                          .groups = 'drop') %>%
                rename(height = away_team_height)) %>%
    group_by(height) %>%
    summarise(tips = sum(tips), 
              wins = sum(wins), 
              height_win_rate = wins/tips,
              .groups = 'drop')
  
  loess_win_rate_95 <- loess(height_win_rate ~ height, data = height_rating_df, span = 0.95)
  smoothed95 <- predict(loess_win_rate_95)
  
  smoothed_height_win_rate <-
    cbind.data.frame(height_rating_df, smoothed_height_win_rate = smoothed95)
  
  expected_win_with_height <-
    expected_win_prob %>%
    left_join(select(smoothed_height_win_rate, height, smoothed_height_win_rate), by = c("home_team_height" = "height")) %>%
    rename(home_height_win = smoothed_height_win_rate) %>%
    left_join(select(smoothed_height_win_rate, height, smoothed_height_win_rate), by = c("away_team_height" = "height")) %>%
    rename(away_height_win = smoothed_height_win_rate) 
  
  expected_win_with_height_jumps_count <-
    expected_win_with_height %>%
    mutate(home_win_rate_adj = case_when(home_jumps < 5 ~ home_height_win*1 + home_win_rate*0,
                                         home_jumps < 10 ~ home_height_win*.9 + home_win_rate*.1,
                                         home_jumps < 25 ~ home_height_win*.75 + home_win_rate*.25,
                                         home_jumps < 50 ~ home_height_win*.5 + home_win_rate*.5,
                                         home_jumps < 75 ~ home_height_win*.25 + home_win_rate*.75,
                                         home_jumps < 100 ~ home_height_win*.1 + home_win_rate*.9,
                                         TRUE ~ home_win_rate),
           away_win_rate_adj = case_when(away_jumps < 5 ~ away_height_win*1 + away_win_rate*0,
                                         away_jumps < 10 ~ away_height_win*.9 + away_win_rate*.1,
                                         away_jumps < 25 ~ away_height_win*.75 + away_win_rate*.25,
                                         away_jumps < 50 ~ away_height_win*.5 + away_win_rate*.5,
                                         away_jumps < 75 ~ away_height_win*.25 + away_win_rate*.75,
                                         away_jumps < 100 ~ away_height_win*.1 + away_win_rate*.9,
                                         TRUE ~ away_win_rate)) %>%
    mutate(home_exp_win_adj = (home_win_rate_adj*(1-away_win_rate_adj)*home_tip_win_parameter) / 
                              ((home_win_rate_adj*(1-away_win_rate_adj)*home_tip_win_parameter) + (away_win_rate_adj*(1-home_win_rate_adj)*(1-home_tip_win_parameter))))
  
  # Viewing performance of fitted win probabilities by expected win proabibility buckets (every 10 percent are grouped)
  train_buckets <-
    expected_win_with_height_jumps_count %>%
    mutate(exp_win_prob = cut(home_exp_win, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  
  # Viewing performance of fitted win probabilities by expected win proabibility buckets (every 10 percent are grouped)
  train_buckets_height <-
    expected_win_with_height_jumps_count %>%
    mutate(exp_win_prob = cut(home_exp_win_adj, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  
  # Creating list of players and their expected win rates
  player_list_df <-
    expected_win_with_height_jumps_count %>%
    distinct(jumper = home_team_jumper, height = home_team_height, jumps = home_jumps, 
             exp_win = home_win_rate, exp_win_adj = home_win_rate_adj) %>%
    bind_rows(expected_win_with_height_jumps_count %>% 
                distinct(jumper = away_team_jumper, height = away_team_height, jumps = away_jumps, 
                         exp_win = away_win_rate, exp_win_adj = away_win_rate_adj)) %>%
    distinct() %>%
    arrange(jumper)
  
  # Finding predicted tip win probability on test 
  test_df <-
    possession_test %>%
    left_join(player_list_df, by = c("home_team_jumper" = "jumper")) %>%
    rename(home_height = height, home_jumps = jumps, home_exp_win = exp_win, home_exp_win_adj = exp_win_adj) %>%
    left_join(player_list_df, by = c("away_team_jumper" = "jumper")) %>%
    rename(away_height = height, away_jumps = jumps, away_exp_win = exp_win, away_exp_win_adj = exp_win_adj)

  # Filtering out players without jumps prior to this game
  test_df_filtered <-
    test_df %>%
    filter(!is.na(home_exp_win),
           !is.na(away_exp_win))
  
  test_df_exp_win <-
    test_df_filtered %>%
    mutate(exp_win = (home_exp_win*(1-away_exp_win)*home_tip_win_parameter) / 
                     ((home_exp_win*(1-away_exp_win)*home_tip_win_parameter) + (away_exp_win*(1-home_exp_win)*(1-home_tip_win_parameter))),
           exp_win_adj = (home_exp_win_adj*(1-away_exp_win_adj)*home_tip_win_parameter) / 
                         ((home_exp_win_adj*(1-away_exp_win_adj)*home_tip_win_parameter) + (away_exp_win_adj*(1-home_exp_win_adj)*(1-home_tip_win_parameter))))
  
  test_df_exp_win <-
    test_df_exp_win %>%
    mutate(final_exp_win = if_else(is.nan(exp_win), 0.5, exp_win),
           final_exp_win_adj = if_else(is.nan(exp_win_adj), 0.5, exp_win_adj))
  
  test_df_exp_win_master <- rbind.data.frame(test_df_exp_win_master, test_df_exp_win)
  
}

# Adding score first percentage (given tip win prediction)
test_df_exp_win_master <-
  test_df_exp_win_master %>%
  mutate(exp_score_first = (final_exp_win_adj*(2/3)) + ((1 - final_exp_win_adj)*(1/3)))

# Views performance of model on test data
# Is split into buckets of predicted win probability, and compares to performance of those predictions
# Ideally, the average win percentage of a bucket should match the bucket it is in
test_buckets <-
  test_df_exp_win_master %>%
  mutate(exp_win_prob = cut(final_exp_win, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(home_won_tip),
            .groups = 'drop') 

test_buckets_height <-
  test_df_exp_win_master %>%
  mutate(exp_win_prob = cut(final_exp_win_adj, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
  group_by(exp_win_prob) %>%
  summarise(jumps = n(),
            true_win_percent = mean(home_won_tip),
            .groups = 'drop') 

test_buckets_height_score <-
  test_df_exp_win_master %>%
  mutate(exp_score_first_prob = cut(exp_score_first, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(exp_score_first_prob) %>%
  summarise(jumps = n(),
            tip_win_percent = mean(home_won_tip),
            first_score_percent = mean(home_score_first),
            .groups = 'drop') 

test_score_first_by_exp_tip <-
  test_df_exp_win_master %>%
  mutate(exp_win_tip_prob = cut(final_exp_win_adj, breaks = seq(0, 100, by = 0.1), right = FALSE)) %>%
  group_by(exp_win_tip_prob) %>%
  summarise(jumps = n(),
            tip_win_percent = mean(home_won_tip),
            exp_first_score_percent = mean(exp_score_first),
            true_first_score_percent = mean(home_score_first),
            .groups = 'drop') 

# Determines Brier Score for win tip backtesting
brier_score_df <-
  test_df_exp_win_master %>%
  mutate(brier_score_normal = (final_exp_win - home_won_tip)^2,
         brier_score_height = (final_exp_win_adj - home_won_tip)^2) %>%
  filter(!is.na(brier_score_normal), !is.na(brier_score_height))

brier_score <- mean(brier_score_df$brier_score_normal)
message("brier score of win tips is equal to ", round(brier_score, 5))

brier_score <- mean(brier_score_df$brier_score_height)
message("brier score of height win tips is equal to ", round(brier_score, 5))

brier_buckets_tip <-
  brier_score_df %>%
  mutate(home_jumps_buckets = cut(home_jumps, breaks = seq(0, 450, by = 50), right = FALSE),
         away_jumps_buckets = cut(away_jumps, breaks = seq(0, 450, by = 50), right = FALSE)) %>%
  group_by(home_jumps_buckets, away_jumps_buckets) %>%
  summarise(jumps = n(),
            brier_avg = mean(brier_score_height),
            .groups = 'drop') 

# Determines Brier Score for score first backtesting
brier_score_first_df <-
  test_df_exp_win_master %>%
  mutate(brier_score_prob = (exp_score_first - home_score_first)^2) %>%
  filter(!is.na(brier_score_prob))

brier_score <- mean(brier_score_first_df$brier_score_prob)
message("brier score of score first is equal to ", round(brier_score, 5))

brier_buckets_score <-
  brier_score_first_df %>%
  mutate(home_jumps_buckets = cut(home_jumps, breaks = seq(0, 450, by = 50), right = FALSE),
         away_jumps_buckets = cut(away_jumps, breaks = seq(0, 450, by = 50), right = FALSE)) %>%
  group_by(home_jumps_buckets, away_jumps_buckets) %>%
  summarise(jumps = n(),
            brier_avg = mean(brier_score_prob),
            .groups = 'drop') 

## Write out main file
write.csv(player_list_df, "data/02_curated/nba_first_to_score/jump_ball_ratings.csv.gz", row.names = FALSE)

## Write out archive file
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(player_list_df, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "jump_ball_ratings.csv.gz"), row.names = FALSE)



#### SCRATCH #####
# When do player ratings stay the same?
jumps_counts_players_home <-
  test_df_exp_win_master %>%
  select(person_id = home_team_person_id, jumps = home_jumps, exp_win = home_exp_win, exp_win_adj = home_exp_win_adj)

jumps_counts_players_away <-
  test_df_exp_win_master %>%
  select(person_id = away_team_person_id, jumps = away_jumps, exp_win = away_exp_win, exp_win_adj = away_exp_win_adj)

jump_counts_players <-
  rbind.data.frame(jumps_counts_players_home, jumps_counts_players_away) %>%
  arrange(person_id, jumps) %>%
  distinct() %>%
  group_by(person_id) %>%
  mutate(difference = abs(exp_win - lag(exp_win)),
         difference_adj = abs(exp_win_adj - lag(exp_win_adj)))

difference_by_jumps <-
  jump_counts_players %>%
  group_by(jumps) %>%
  summarise(avg_diff = mean(difference, na.rm = T),
            avg_diff_adj = mean(difference_adj, na.rm = T))

plot(difference_by_jumps$jumps, difference_by_jumps$avg_diff)
plot(difference_by_jumps$jumps, difference_by_jumps$avg_diff_adj)




