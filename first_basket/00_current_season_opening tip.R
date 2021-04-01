# This script outputs a data frame that shows a player's performance 
# on JUST opening tips for the current season

library(tidyverse)
library(lubridate)
library(data.table)

# Relevant gamelogs
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                     colClasses = 'character')

# Reads in list of teams to join on team id
team_list <- 
  fread("./data/01_raw/nba_teams/current.csv") %>%
  select(TEAM_ID, ABBREVIATION) %>%
  mutate(TEAM_ID = as.character(TEAM_ID))

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
  pbp <- 
    read.csv(csv_path, 
                  colClasses = 'character',
                  na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(grepl("jump ball", tolower(HOMEDESCRIPTION)) | HOMEDESCRIPTION == " ",
           PERIOD == "1",
           PCTIMESTRING == "12:00") %>%
    left_join(team_list, by = c("PLAYER3_ID" = "TEAM_ID")) %>%
    mutate(PLAYER3_TEAM_ABBREVIATION = coalesce(PLAYER3_TEAM_ABBREVIATION, ABBREVIATION)) %>%
    select(-ABBREVIATION)
  
  home_team_id <- as.character(as.integer(pbp$PLAYER1_TEAM_ID))
  home_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(pbp$PLAYER1_NAME)
  away_team_id <- as.character(as.integer(pbp$PLAYER2_TEAM_ID))
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
    home_team_id = home_team_id,
    home_team_jumper = home_team_jumper,
    away_team_abbrev = away_team_abbrev,
    away_team_id = away_team_id,
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
  select(game_date, jumper = home_team_jumper, team_abbrev = home_team_abbrev, team_id = home_team_id, home_win_tip) %>%
  bind_rows(first_possession_df %>% 
              select(game_date, jumper = away_team_jumper, team_abbrev = away_team_abbrev, team_id = away_team_id, home_win_tip) %>%
              #Reverse home_win_tip for away players
              mutate(home_win_tip = if_else(home_win_tip, FALSE, TRUE))) %>%
  group_by(jumper, team_abbrev, team_id) %>%
  summarise(jumps = n(), 
            wins = sum(home_win_tip), 
            win_rate = wins/jumps,
            last_jump = max(game_date),
            .groups = 'drop')

## Write out main file
write.csv(jump_balls_df, "data/02_curated/nba_first_to_score/current_season_opening_tip.csv.gz", row.names = FALSE)

## Write out archive file
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(jump_balls_df, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "current_season_opening_tip.csv.gz"), row.names = FALSE)
