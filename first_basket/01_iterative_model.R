library(tidyverse)
library(lubridate)
library(data.table)

# Parameters

# Example - for full run where you use all history and start the test data at 2017-18 season
# earliest_train_data_date <- "2015-09-01"
# train_test_date_split <- "2017-09-01"

# Load previous run of this model
current_iterative <- fread("data/02_curated/nba_first_to_score/model_iterative.csv.gz",
                           colClasses =  c('game_date' = 'Date'))
max_date_iterative <- max(current_iterative$game_date)

# Load list of game logs
possession_binded <- fread("data/02_curated/nba_first_to_score/jump_ball_dataset.csv.gz",
                           colClasses = c('game_id' = 'character', 'home_team_person_id' = 'character', 'away_team_person_id' = 'character'))

# Current model uses last two completed seasons and current season as train data
# To run model for current day's ratings, train_test_date_split is set to current day
# Sets furthest date that train data goes back
earliest_train_data_date <- "2015-09-01"
# Setting date we want to start logging test data on
# Default uses day after max date from prior run, so any data that has occurred since then is added
train_test_date_split <- max_date_iterative + days(1)

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
  select(jumper = DISPLAY_FIRST_LAST, person_id = PERSON_ID,
         height = HEIGHT, weight = WEIGHT)

# Adds in player height and weight
possession_player_info <-
  possession_binded %>%
  left_join(player_info_mutated, by = c("home_team_jumper" = "jumper", "home_team_person_id" = "person_id")) %>%
  rename(home_team_height = height, home_team_weight = weight) %>%
  left_join(player_info_mutated, by = c("away_team_jumper" = "jumper", "away_team_person_id" = "person_id")) %>%
  rename(away_team_height = height, away_team_weight = weight) %>%
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

home_tip_win_parameter <- .508

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
if (exists('current_iterative')){
  test_df_exp_win_master <- current_iterative
}else{
  test_df_exp_win_master <- tibble()
}

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
  
  # Table that stores wins rates for each height
  height_rating_df <-
    possession_train %>%
    group_by(home_team_height) %>%
    summarise(tips = n(),
              wins = sum(home_won_tip),
              .groups = 'drop') %>%
    rename(height = home_team_height) %>%
    bind_rows(possession_train %>%
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
  
  smoothed_height_win_rate <- cbind.data.frame(height_rating_df, smoothed_height_win_rate = smoothed95)
  
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
  
  # Viewing performance of fitted win probabilities by expected win probability buckets (every 10 percent are grouped)
  train_buckets <-
    expected_win_with_height_jumps_count %>%
    mutate(exp_win_prob = cut(home_exp_win, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  
  # Viewing performance of fitted win probabilities by expected win probability buckets (every 10 percent are grouped)
  train_buckets_height <-
    expected_win_with_height_jumps_count %>%
    mutate(exp_win_prob = cut(home_exp_win_adj, breaks = seq(0, 100, by = 0.10), right = FALSE)) %>%
    group_by(exp_win_prob) %>%
    summarise(jumps = n(),
              true_win_percent = mean(home_won_tip),
              .groups = 'drop')
  

  # Creating list of players and their expected win rates
  player_list_active_df <-
    expected_win_with_height_jumps_count %>%
    distinct(jumper = home_team_jumper, height = home_team_height, jumps = home_jumps,
             exp_win = home_win_rate, exp_win_adj = home_win_rate_adj) %>%
    bind_rows(expected_win_with_height_jumps_count %>%
                distinct(jumper = away_team_jumper, height = away_team_height, jumps = away_jumps,
                         exp_win = away_win_rate, exp_win_adj = away_win_rate_adj)) %>%
    distinct() %>%
    arrange(jumper)
  
  # Includes players who don't have a logged jump
  player_list_df <-
    player_info_mutated %>%
    left_join(player_list_active_df, by = c("jumper", "height")) %>%
    left_join(select(smoothed_height_win_rate, height, smoothed_height_win_rate), by = "height") %>%
    mutate(exp_win_adj = coalesce(exp_win_adj, smoothed_height_win_rate)) %>%
    select(-smoothed_height_win_rate)
  
  # Finding predicted tip win probability on test
  test_df <-
    possession_test %>%
    left_join(player_list_df, by = c("home_team_jumper" = "jumper", "home_team_person_id" = "person_id", "home_team_height" = "height")) %>%
    rename(home_team_weight = weight, home_jumps = jumps, home_exp_win = exp_win, home_exp_win_adj = exp_win_adj) %>%
    left_join(player_list_df, by = c("away_team_jumper" = "jumper", "away_team_person_id" = "person_id", "away_team_height" = "height")) %>%
    rename(away_team_weight = weight, away_jumps = jumps, away_exp_win = exp_win, away_exp_win_adj = exp_win_adj) %>%
    mutate(home_jumps = replace_na(home_jumps, 0),
           home_exp_win = replace_na(home_exp_win, 0),
           away_jumps = replace_na(away_jumps, 0),
           away_exp_win = replace_na(away_exp_win, 0))
  
  test_df_exp_win <-
    test_df %>%
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

write.csv(test_df_exp_win_master, "data/02_curated/nba_first_to_score/model_iterative.csv.gz", row.names = FALSE)

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

