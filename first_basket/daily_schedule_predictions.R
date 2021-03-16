library(data.table)
library(tidyverse)

today_date <- gsub("-", "", Sys.Date())

player_ratings <- fread("data/curated/nba/jump_ball_ratings.csv.gz")
opening_tip <- fread("data/curated/nba/current_season_opening_tip.csv.gz")
first_shot <- fread("data/curated/nba/current_season_first_shot.csv.gz")
schedule <- fread(paste0("data/nba_schedules/", today_date, ".csv"))
current_centers <- fread("data/curated/nba/current_centers.csv")
player_usage <- fread("data/curated/nba/current_season_usage_rate.csv.gz")

current_centers_concat <-
  current_centers %>%
  mutate(concat_field = paste0(team_abbrev, player))

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
                        ((home_rating*(1-away_rating)) + (away_rating*(1-home_rating))),
         away_exp_win = 1 - home_exp_win,
         winning_jumper = if_else(home_exp_win >= away_exp_win, home_jumper, away_jumper),
         exp_win = if_else(home_exp_win >= away_exp_win, home_exp_win, away_exp_win),
         odds = round(case_when(exp_win > .5 ~ (exp_win / (1 - (exp_win))) * -100,
                                TRUE ~ (100/exp_win) - 100), 0))

win_tip_df <-
  today_games_jumper %>%
  mutate(home_concat = paste0(home_team_abbrev, home_jumper),
         away_concat = paste0(away_team_abbrev, away_jumper)) %>%
  filter(home_concat %in% current_centers_concat$concat_field,
         away_concat %in% current_centers_concat$concat_field) %>%
  select(home = home_team_abbrev, home_jumper, home_wins, home_jumps, home_win_rate, 
         away = away_team_abbrev, away_jumper, away_wins, away_jumps, away_win_rate,
         home_rating, away_rating, winning_jumper, win_prob = exp_win, winner_odds = odds) %>%
  mutate(home_win_rate = round(home_win_rate, 3),
         away_win_rate = round(away_win_rate, 3),
         home_rating = round(home_rating, 3),
         away_rating = round(away_rating, 3),
         win_prob = round(win_prob, 3))

team_odds <-
  win_tip_df %>%
  mutate(team_win_tip = if_else(home_jumper == winning_jumper, win_prob, 1 - win_prob)) %>%
  select(team = home, team_win_tip) %>%
  bind_rows(win_tip_df %>%
              mutate(team_win_tip = if_else(away_jumper == winning_jumper, win_prob, 1 - win_prob)) %>%
              select(team = away, team_win_tip))

first_shot_odds_df <-
  first_shot %>%
  rename(team = team_abbrev) %>%
  filter(team %in% team_odds$team) %>%
  left_join(team_odds) %>%
  mutate(game_percentage = percentage * team_win_tip,
         game_odds = case_when(game_percentage > .5 ~ (game_percentage / (1 - (game_percentage))) * -100,
                               TRUE ~ (100/game_percentage) - 100)) %>%
  left_join(select(player_usage, PLAYER_NAME, TEAM_ABBREVIATION, USG), by = c("team" = "TEAM_ABBREVIATION", "player" = "PLAYER_NAME")) %>%
  mutate(game_usage = ((percentage * team_win_tip) + (USG * team_win_tip)) / 2,
         usage_odds = case_when(game_usage > .5 ~ (game_usage / (1 - (game_usage))) * -100,
                               TRUE ~ (100/game_usage) - 100)) %>%
  arrange(team, usage_odds)



# calculate_jump_odds <- function(player_1, player_2, player_list_df){
#   
#   player_1_data <-
#     player_list_df %>%
#     filter(jumper == player_1)
#   
#   player_2_data <-
#     player_list_df %>%
#     filter(jumper == player_2)
#   
#   if (nrow(player_1_data) == 0 | nrow(player_2_data) == 0) {
#     stop("Check to make sure you spelled a player's name correctly!")
#   }
#   
#   player_1_win <- 
#     round((player_1_data$exp_win*(1-player_2_data$exp_win)) / 
#             ((player_1_data$exp_win*(1-player_2_data$exp_win)) + (player_2_data$exp_win*(1-player_1_data$exp_win))), 2)
#   
#   # Always print favored player first
#   if (player_1_win >= .5) {
#     message(paste0(player_1, " wins a tip vs ", player_2, " ", player_1_win * 100, " percent of the time"))
#   }else{
#     message(paste0(player_2, " wins a tip vs ", player_1, " ", (1 - player_1_win)*100, " percent of the time"))
#   }
# }
# 
# player_1 <- "Nikola Jokic"
# player_2 <- "Joel Embiid"
# calculate_jump_odds(player_1, player_2, player_list_df)
# 
