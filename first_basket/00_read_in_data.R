library(data.table)
library(tidyverse)
library(lubridate)

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

# Reads in list of teams to join on team id
team_list <-
  fread("./data/01_raw/nba_teams/current.csv") %>%
  select(TEAM_ID, ABBREVIATION) %>%
  mutate(TEAM_ID = as.character(TEAM_ID))

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
    mutate(opening_jump = if_else(PERIOD == "1" & PCTIMESTRING == "12:00", TRUE, FALSE),
           center_court = if_else(opening_jump == TRUE | (PERIOD > 4 & PCTIMESTRING == "5:00"), TRUE, FALSE))
  
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
  center_court <- as.character(pbp$center_court)
  period <- as.character(pbp$PERIOD)
  period_clock <- as.character(pbp$PCTIMESTRING)
  
  # Final output of relevant info about jumps
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    opening_jump = opening_jump,
    center_court = center_court,
    period = period,
    period_clock = period_clock,
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
         period_clock = period_to_seconds(ms(period_clock)))

write.csv(possession_binded, "data/02_curated/nba_first_to_score/jump_ball_dataset.csv.gz", row.names = FALSE)
