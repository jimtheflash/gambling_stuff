library(data.table)
library(tidyverse)

today_date <- gsub("-", "", Sys.Date())

player_ratings <- fread("data/curated/nba/jump_ball_ratings.csv.gz")
opening_tip <- fread("data/curated/nba/current_season_opening_tip.csv.gz")
first_shot <- fread("data/curated/nba/current_season_first_shot.csv.gz")
schedule <- fread(paste0("data/nba_schedules/", today_date, ".csv"))
current_centers <- fread("data/curated/nba/current_centers.csv")
current_starters <- fread("data/curated/nba/current_starters.csv")
player_usage <- fread("data/curated/nba/current_season_usage_rate.csv.gz")

home_tip_win_parameter <- .508

first_shot_concat <-
  first_shot %>%
  mutate(concat_field = paste0(team_abbrev, player))

current_centers_concat <-
  current_centers %>%
  rename(center = player)

current_lineups <-
  current_starters %>%
  left_join(current_centers_concat)

current_lineups_concat <-
  current_lineups %>%
  mutate(starter_concat_field = paste0(team_abbrev, player),
         center_concat_field = paste0(team_abbrev, center))

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
  left_join(select(player_ratings, jumper, exp_win_adj)) %>%
  rename(home_jumper = jumper,
         home_team_abbrev = team_abbrev,
         home_jumps = jumps,
         home_wins = wins,
         home_win_rate = win_rate,
         home_rating = exp_win_adj) %>%
  left_join(jumper_joined, by = c("VISITOR_TEAM_ID" = "team_id")) %>%
  left_join(select(player_ratings, jumper, exp_win_adj)) %>%
  rename(away_jumper = jumper,
         away_team_abbrev = team_abbrev,
         away_jumps = jumps,
         away_wins = wins,
         away_win_rate = win_rate,
         away_rating = exp_win_adj) %>%
  mutate(home_exp_win = (home_rating*(1-away_rating)*home_tip_win_parameter) / 
                            ((home_rating*(1-away_rating)*home_tip_win_parameter) + (away_rating*(1-home_rating)*(1-home_tip_win_parameter))),
         away_exp_win = 1 - home_exp_win,
         exp_winning_jumper = if_else(home_exp_win >= away_exp_win, home_jumper, away_jumper),
         exp_win_tip = if_else(home_exp_win >= away_exp_win, home_exp_win, away_exp_win),
         exp_score_first = (exp_win_tip*(2/3)) + ((1 - exp_win_tip)*(1/3)),
         odds = round(case_when(exp_score_first > .5 ~ (exp_score_first / (1 - (exp_score_first))) * -100,
                                TRUE ~ (100/exp_score_first) - 100), 0))

win_tip_df <-
  today_games_jumper %>%
  mutate(home_concat = paste0(home_team_abbrev, home_jumper),
         away_concat = paste0(away_team_abbrev, away_jumper)) %>%
  filter(home_concat %in% current_lineups_concat$center_concat_field,
         away_concat %in% current_lineups_concat$center_concat_field) %>%
  left_join(current_lineups_concat, by = c("home_team_abbrev" = "team_abbrev", "home_concat" = "center_concat_field", "home_jumper" = "player")) %>%
  rename(home_lineup = lineup) %>%
  select(-c(position, center, starter_concat_field)) %>%
  left_join(current_lineups_concat, by = c("away_team_abbrev" = "team_abbrev", "away_concat" = "center_concat_field", "away_jumper" = "player")) %>%
  rename(away_lineup = lineup) %>%
  select(-c(position, center, starter_concat_field)) %>%
  mutate(home_win_rate = round(home_win_rate, 3),
         away_rating = round(away_rating, 3),
         home_rating = round(home_rating, 3),
         win_tip_prob = round(exp_win_tip, 3),
         team_score_first_prob = round(exp_score_first, 3),
         away_szn_open_tips = paste0(away_wins,"/",away_jumps," (",round(away_win_rate*100, 1),"%)"),
         home_szn_open_tips = paste0(home_wins,"/",home_jumps," (",round(home_win_rate*100, 1),"%)")) %>%
  select(away_team = away_team_abbrev, away_concat, away_lineup, away_jumper, away_szn_open_tips, away_rating,
         home_team = home_team_abbrev, home_concat, home_lineup, home_jumper, home_szn_open_tips, home_rating,
         exp_winning_jumper, win_tip_prob, team_score_first_prob, odds)


team_odds <-
  win_tip_df %>%
  mutate(team_win_tip = if_else(home_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first = if_else(home_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
  select(team = home_team, jumper = home_jumper, home_lineup, home_concat, away_lineup, away_concat, team_win_tip, team_score_first) %>%
  bind_rows(win_tip_df %>%
              mutate(team_win_tip = if_else(away_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
                     team_score_first = if_else(away_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
              select(team = away_team, jumper = away_jumper, home_lineup, home_concat, away_lineup, away_concat, team_win_tip, team_score_first))

first_shot_df_starters <-
  current_lineups_concat %>%
  left_join(first_shot_concat) %>%
  mutate(starts = coalesce(starts, 0),
         first_shots = coalesce(shots, 0),
         first_shot_percent = coalesce(percentage, 0)) %>%
  select(-c(odds, shots, percentage, concat_field)) %>%
  filter(team_abbrev %in% team_odds$team)

first_shot_odds_df <-
  first_shot_df_starters %>%
  rename(team = team_abbrev) %>%
  inner_join(team_odds, by = c("team", "center_concat_field" = "home_concat")) %>%
  bind_rows(first_shot_df_starters %>%
              rename(team = team_abbrev) %>%
              inner_join(team_odds, by = c("team", "center_concat_field" = "away_concat"))) %>%
  select(team, player, starts, first_shots, first_shot_percent, home_lineup, away_lineup, team_win_tip, team_score_first) %>%
  left_join(select(player_usage, PLAYER_NAME, TEAM_ABBREVIATION, USG, FG_USG, FG_PCT), by = c("team" = "TEAM_ABBREVIATION", "player" = "PLAYER_NAME")) %>%
  mutate(first_shot_usg = (first_shot_percent + FG_USG) / 2,
         first_shot_make = first_shot_usg * FG_PCT)

first_shot_new_df <-
  first_shot_odds_df %>%
  group_by(team, home_lineup, away_lineup) %>%
  mutate(starters_first_make = first_shot_make/sum(first_shot_make),
         game_first_make = starters_first_make * team_score_first,
         game_first_poss_make_odds = case_when(game_first_make > .5 ~ (game_first_make / (1 - (game_first_make))) * -100,
                                               TRUE ~ (100/game_first_make) - 100)) %>%
  arrange(team, game_first_poss_make_odds)

team_to_score_first <-
  win_tip_df %>%
  select(away_team, away_jumper:home_team, home_jumper:odds)

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
#   include hoem win parameter
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
