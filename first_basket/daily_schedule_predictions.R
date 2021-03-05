library(data.table)
library(tidyverse)

player_ratings <- fread("data/curated/nba/jump_ball_ratings.csv.gz")
opening_tip <- fread("data/curated/nba/current_season_opening_tip.csv.gz")
first_shot <- fread("data/curated/nba/current_season_first_shot.csv.gz")
schedule <- fread("data/nba_schedules/20210223.csv")

today_games <-
  schedule %>%
  select(HOME_TEAM_ID, VISITOR_TEAM_ID)

jumper_aggregates <-
  opening_tip %>%
  group_by(jumper) %>%
  summarise(jumps = sum(jumps), 
            wins = sum(wins), 
            win_rate = wins/jumps,
            .groups = 'drop')

jumper_joined <-
  opening_tip %>%
  select(jumper, team_abbrev, team_id) %>%
  left_join(jumper_aggregates)

today_games_jumper <-
  today_games %>%
  left_join(jumper_joined, by = c("HOME_TEAM_ID" = "team_id")) %>%
  left_join(select(player_ratings, jumper, exp_win)) %>%
  rename(home_jumper = jumper,
         home_team_abbrev = team_abbrev,
         home_jumps = jumps,
         home_wins = wins,
         home_win_rate = win_rate,
         home_rating = exp_win) %>%
  left_join(jumper_joined, by = c("VISITOR_TEAM_ID" = "team_id")) %>%
  left_join(select(player_ratings, jumper, exp_win)) %>%
  rename(away_jumper = jumper,
         away_team_abbrev = team_abbrev,
         away_jumps = jumps,
         away_wins = wins,
         away_win_rate = win_rate,
         away_rating = exp_win) %>%
  mutate(home_exp_win = (home_rating*(1-away_rating)) / 
                        ((home_rating*(1-away_rating)) + (away_rating*(1-home_rating))))

ordered_df <-
  today_games_jumper %>%
  select(home_team_abbrev, home_jumper, home_jumps, home_wins, home_win_rate, 
         away_team_abbrev, away_jumper, away_jumps, away_wins, away_win_rate,
         home_rating, away_rating, home_exp_win)



