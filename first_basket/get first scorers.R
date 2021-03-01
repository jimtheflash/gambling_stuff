library(tidyverse)
library(data.table)

gamelogs <- fread('./data/nba_gamelogs/nba_gamelogs_2020-21.csv.gz', 
                     colClasses = 'character')

unique_games <- unique(gamelogs$GAME_ID)

first_scorer_list <- list()

for (g in unique_games) {

  gamelog <- gamelogs %>%
    filter(GAME_ID == g)

  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])

  csv_path <- paste0('./data/nba_pbp/', g, '.csv.gz')

  pbp <- fread(csv_path, 
                  colClasses = 'character',
                  na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(SCORE != '' & !is.na(SCORE) & !is.na(EVENTNUM)) %>%
    filter(EVENTNUM == min(EVENTNUM))

  first_scorer_player_id <- as.character(pbp$PLAYER1_ID)
  first_scorer_player_name <- as.character(pbp$PLAYER1_NAME)
  first_scorer_team_id <- as.character(pbp$PLAYER1_TEAM_ID)
  first_scorer_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)

  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    first_scorer_id = first_scorer_player_id,
    first_scorer_name = first_scorer_player_name,
    first_scorer_team_id = first_scorer_team_id,
    first_scorer_team_abbrev = first_scorer_team_abbrev
  )

  first_scorer_list[[g]] <- output

}

first_scorer_df <- bind_rows(first_scorer_list)

# make a table of player-teams (player could show up twice if traded or cut/signed)
player_df <- first_scorer_df %>%
  group_by(first_scorer_name, first_scorer_team_abbrev) %>%
  summarise(player_season_first_baskets = n_distinct(game_id)) %>%
  inner_join(
    gamelogs %>%
      group_by(PLAYER_NAME) %>%
      summarise(player_season_games = n_distinct(GAME_ID[MIN > 0])) %>%
      ungroup(),
    by = c('first_scorer_name' = 'PLAYER_NAME')
  ) %>%
  inner_join(
    first_scorer_df %>%
      group_by(first_scorer_team_abbrev) %>%
      summarise(team_season_first_baskets = n_distinct(game_id)) %>%
      ungroup(),
    by = 'first_scorer_team_abbrev'
  ) %>%
  inner_join(
    gamelogs %>%
      group_by(TEAM_ABBREVIATION) %>%
      summarise(team_season_games = n_distinct(GAME_ID)) %>%
      ungroup(),
    by = c('first_scorer_team_abbrev' = 'TEAM_ABBREVIATION')
  ) %>%
  arrange(desc(player_season_first_baskets)) %>%
  ungroup() %>%
  mutate(
    player_season_first_basket_rate = player_season_first_baskets / player_season_games,
    player_team_season_first_basket_proportion = player_season_first_baskets / team_season_first_baskets
  )
View(player_df)

team_df <- first_scorer_df %>%
  group_by(first_scorer_team_abbrev) %>%
  summarise(first_baskets = n_distinct(game_id)) %>%
  inner_join(
    gamelogs %>%
      group_by(TEAM_ABBREVIATION) %>%
      summarise(team_games = n_distinct(GAME_ID)) %>%
      ungroup(),
    by = c("first_scorer_team_abbrev" = "TEAM_ABBREVIATION")
    ) %>%
  ungroup() %>%
  mutate(first_basket_rate = first_baskets / team_games) %>%
  arrange(desc(first_basket_rate))
View(team_df)
# pick a team using the matchup abbrevs to see how often they score first points and which players do it most

abbrev <- 'UTA'

team_games <- first_scorer_df %>%
  filter(grepl(abbrev, matchup))

mean(team_games$first_scorer_team_abbrev == abbrev)
sort(table(team_games$first_scorer_name[team_games$first_scorer_team_abbrev == abbrev]))
