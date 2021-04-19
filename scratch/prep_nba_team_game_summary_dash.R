# load nba.modelR to get the pipeline prep tools
devtools::load_all('/Users/jim/Documents/nba.modelR/')
library(tidyverse)

# set some field types
datefields <- c('game_date')
numfields <- c('min', 'fgm', 'fga', 'fg_pct', 'fg3m', 'fg3a', 'fg3_pct', 'ftm', 'fta', 'ft_pct', 'oreb', 'dreb', 'reb', 'ast', 'tov', 'stl', 'blk', 'blka', 'pf', 'pfd', 'pts', 'nba_fantasy_pts', 'dd2', 'td3', 'fd', 'dk')

# import player-game data, tidy it up, filter some stuff, add a few fields
raw_data <- get_data(data_path = '/Users/jim/Documents/gambling_stuff/data/nba_gamelogs')

tidier_data <- tidyup_data(raw_data,
                           date_fields = datefields,
                           numeric_fields = numfields)

filtered_data <- tidier_data %>%
  filter(season_type == 'Regular Season', season_year == '2020-21')

engineered_data <- filtered_data %>%
  mutate(home_away = dplyr::if_else(grepl('@', matchup), 'away', 'home')) %>%
  group_by(game_id) %>%
  mutate(overtimes = (sum(min) - 480) / 50) %>%
  ungroup()

# summarize data for dashboards into separate data.frames for easier sorting (maybe do that in the server?)
team_games <- engineered_data %>%
  group_by(team_name, game_id) %>%
  summarise(game_date = max(game_date),
            home_away = max(home_away),
            overtimes = max(overtimes),
            team_pts = sum(pts)) %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(total_pts = sum(team_pts),
         opp_pts = total_pts - team_pts,
         margin = team_pts - opp_pts) %>%
  ungroup()

saveRDS(team_games, 'data/nba_team_games/team_games.rds')
