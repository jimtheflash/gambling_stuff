library(tidyverse)

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
train_test_date_split <- Sys.Date() #"2018-09-01"

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
    filter((grepl("jump ball", tolower(HOMEDESCRIPTION)) & PERIOD == "1" & PCTIMESTRING == "12:00") | 
             (SCORE != '' & !is.na(SCORE) & !is.na(EVENTNUM)))
  
  win_tip <- 
    pbp %>%
    filter(row_number() == 1) %>%
    mutate(team_win_tip = PLAYER3_TEAM_ABBREVIATION)
  
  score_first <-
    pbp %>%
    filter(row_number() == 2) %>%
    mutate(team_score_first = PLAYER1_TEAM_ABBREVIATION)

  home_team_abbrev <- as.character(win_tip$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(win_tip$PLAYER1_NAME)
  home_team_person_id <- as.character(win_tip$PLAYER1_ID)
  away_team_abbrev <- as.character(win_tip$PLAYER2_TEAM_ABBREVIATION)
  away_team_jumper <- as.character(win_tip$PLAYER2_NAME)
  away_team_person_id <- as.character(win_tip$PLAYER2_ID)
  possession <- as.character(win_tip$team_win_tip)
  score_first <- as.character(score_first$team_score_first)
  
  # Final output of relevant info about jumps
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
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
         home_score_first = if_else(home_team_abbrev == score_first, TRUE, FALSE),
         won_tip_and_score_first = if_else(home_won_tip == home_score_first, TRUE, FALSE))

test_df <-
  possession_binded %>%
  group_by(season, home_won_tip) %>%
  summarise(tips = n(),
            score_first = sum(home_score_first),
            score_first_percent = mean(home_score_first))


