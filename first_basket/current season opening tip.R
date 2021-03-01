# This script outputs a data frame that shows a player's performance 
# on JUST opening tips for the current season

library(tidyverse)

# Relevant gamelogs
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                     colClasses = 'character')

unique_games <- unique(gamelogs$GAME_ID)

first_possession_list <- list()

for (g in unique_games) {
  
  # Unique game in loop
  gamelog <- gamelogs %>%
    filter(GAME_ID == g)
  
  # Relevant info of game
  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])
  
  csv_path <- paste0('./data/nba_pbp/', g, '.csv')
  
  # Finding all opening tips in game
  pbp <- read.csv(csv_path, 
                  colClasses = 'character',
                  na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(row_number() == 2)
  
  home_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(pbp$PLAYER1_NAME)
  away_team_abbrev <- as.character(pbp$PLAYER2_TEAM_ABBREVIATION)
  away_team_jumper <- as.character(pbp$PLAYER2_NAME)
  first_possession <- as.character(pbp$PLAYER3_TEAM_ABBREVIATION)
  
  # Final output of relevant info about tip
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    home_team_abbrev = home_team_abbrev,
    home_team_jumper = home_team_jumper,
    away_team_abbrev = away_team_abbrev,
    away_team_jumper = away_team_jumper,
    first_possession = first_possession
  )

  first_possession_list[[g]] <- output
  
}

# Removing cases where first possession could not be determined from pbp
# Adding indicator if home team won tip
first_possession_df <- 
  bind_rows(first_possession_list) %>%
  filter(!is.na(first_possession)) %>%
  mutate(home_win_tip = if_else(home_team_abbrev == first_possession, TRUE, FALSE))

# Creates table of each jumper and their opening jump stats for this season
jump_balls_df <-
  first_possession_df %>%
  select(jumper = home_team_jumper, team_abbrev = home_team_abbrev, home_win_tip) %>%
  bind_rows(first_possession_df %>% 
              select(jumper = away_team_jumper, team_abbrev = away_team_abbrev, home_win_tip) %>%
              #Reverse home_win_tip for away players
              mutate(home_win_tip = if_else(home_win_tip, FALSE, TRUE))) %>%
  group_by(jumper) %>%
  summarise(jumps = n(), 
            wins = sum(home_win_tip), 
            win_rate = wins/jumps,
            .groups = 'drop')

view(jump_balls_df)

