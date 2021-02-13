library(tidyverse)

gamelogs <- read_csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv')

unique_games <- unique(gamelogs$GAME_ID)

first_scorer_list <- list()

for (g in unique_games) {

  gamelog <- gamelogs %>%
    filter(GAME_ID == g)

  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])

  csv_path <- paste0('./data/nba_pbp/', g, '.csv')

  pbp_1q <- read_csv(csv_path) %>%
    filter(!is.na(SCORE)) %>%
    filter(EVENTNUM == min(EVENTNUM))

  first_scorer_player_id <- as.character(pbp_1q$PLAYER1_ID)
  first_scorer_player_name <- as.character(pbp_1q$PLAYER1_NAME)
  first_scorer_team_id <- as.character(pbp_1q$PLAYER1_TEAM_ID)
  first_scorer_team_abbrev <- as.character(pbp_1q$PLAYER1_TEAM_ABBREVIATION)

  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    first_scorer_id = first_scorer_player_id,
    first_scorer_name = first_scorer_player_name,
    first_scorer_team_id = first_scorer_team_id,
    first_scorer_team = first_scorer_team_abbrev
  )

  first_scorer_list[[g]] <- output

}

first_scorer_df <- bind_rows(first_scorer_list)

# pick a team using the matchup abbrevs to see how often they score first points and which players do it most

abbrev <- 'POR'

team_games <- first_scorer_df %>%
  filter(grepl(abbrev, matchup))

mean(team_games$first_scorer_team == abbrev)
sort(table(team_games$first_scorer_name[team_games$first_scorer_team == abbrev]))
