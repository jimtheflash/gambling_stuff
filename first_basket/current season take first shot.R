# This script outputs a data frame that shows the percentage of first shots
# that a player has taken for their team during JUST this season
# out of the number of starts they have made

library(tidyverse)

# Relevant gamelogs
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                     colClasses = 'character')

unique_games <- unique(gamelogs$GAME_ID)

first_shot_list <- list()
starters_list <- list()

for (g in unique_games) {
  
  # Unique game in loops
  gamelog <- gamelogs %>%
    filter(GAME_ID == g)
  
  # Relevant info of game
  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])
  
  csv_path_pbp <- paste0('./data/nba_pbp/', g, '.csv')
  csv_path_box <- paste0('./data/nba_boxscores/', g, '.csv')
  
  # PBP data after opening jump
  pbp <- 
    read.csv(csv_path_pbp, 
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(row_number() > 2)
  
  # Looks for first shot by home team
  # Filters data to first time score isn't an empty string, or there was a miss
  home_first_shot <-
    pbp %>%
    filter(!is.na(HOMEDESCRIPTION),
           SCORE != '' | row_number() == min(grep("MISS", HOMEDESCRIPTION))) %>%
    filter(row_number() == 1) %>%
    select(PLAYER1_TEAM_ABBREVIATION, PLAYER1_NAME)
  
  # Looks for first shot by away team
  # Filters data to first time score isn't an empty string, or there was a miss
  away_first_shot <-
    pbp %>%
    filter(!is.na(VISITORDESCRIPTION),
           SCORE != '' | row_number() == min(grep("MISS", VISITORDESCRIPTION))) %>%
    filter(row_number() == 1) %>%
    select(PLAYER1_TEAM_ABBREVIATION, PLAYER1_NAME)
  
  home_team_abbrev <- as.character(home_first_shot$PLAYER1_TEAM_ABBREVIATION)
  home_first_shot <- as.character(home_first_shot$PLAYER1_NAME)
  away_team_abbrev <- as.character(away_first_shot$PLAYER1_TEAM_ABBREVIATION)
  away_first_shot <- as.character(away_first_shot$PLAYER1_NAME)
  
  # Output table of first shots of game
  first_shot_output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    home_team_abbrev = home_team_abbrev,
    home_first_shot = home_first_shot,
    away_team_abbrev = away_team_abbrev,
    away_first_shot = away_first_shot
  )
  
  first_shot_list[[g]] <- first_shot_output
  
  # Grabbing starters for each game
  box_score <-
    read.csv(csv_path_box, 
             colClasses = 'character',
             na.strings = c('')) %>%
    filter(!is.na(START_POSITION)) %>%
    select(player = PLAYER_NAME, team_abbrev = TEAM_ABBREVIATION)
  
  starters_list[[g]] <- box_score
  
}

# Table of starters by team and number of starts
starters_df <- 
  bind_rows(starters_list) %>%
  group_by(player, team_abbrev) %>%
  summarise(starts = n(),
            .groups = 'drop')

# Table of first shots taken by player for each team
shots_df <- 
  bind_rows(first_shot_list) %>%
  group_by(team_abbrev = home_team_abbrev, player = home_first_shot) %>%
  summarise(shots = n()) %>%
  bind_rows(bind_rows(first_shot_list) %>%
              group_by(team_abbrev = away_team_abbrev, player = away_first_shot) %>%
              summarise(shots = n())) %>%
  group_by(team_abbrev, player) %>%
  summarise(shots = sum(shots))

# Joining first shots by team and starts to get percentage of first shots by start by player
# Adds the observed odds of a player shooting a first shot given they are a starter
first_shot_odds_df <-
  starters_df %>%
  left_join(shots_df) %>%
  mutate(shots = if_else(is.na(shots), as.integer(0), shots),
         percentage = shots/starts,
         odds = case_when(percentage > .5 ~ (percentage / (1 - (percentage))) * -100,
                          TRUE ~ (100/percentage) - 100)) %>%
  arrange(team_abbrev, odds)
  
view(first_shot_odds_df)
