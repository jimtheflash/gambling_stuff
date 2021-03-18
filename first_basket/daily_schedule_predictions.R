library(data.table)
library(tidyverse)

today_date <- gsub("-", "", Sys.Date()-1)

player_ratings <- fread("data/curated/nba/jump_ball_ratings.csv.gz")
opening_tip <- fread("data/curated/nba/current_season_opening_tip.csv.gz")
first_shot <- fread("data/curated/nba/current_season_first_shot.csv.gz")
schedule <- fread(paste0("data/nba_schedules/", today_date, ".csv"))
current_centers <- fread("data/curated/nba/current_centers.csv")
current_starters <- fread("data/curated/nba/current_starters.csv")
player_usage <- fread("data/curated/nba/current_season_usage_rate.csv.gz")

first_shot_concat <-
  first_shot %>%
  mutate(concat_field = paste0(team_abbrev, player))

current_centers_concat <-
  current_centers %>%
  mutate(concat_field = paste0(team_abbrev, player))

current_starters_concat <-
  current_starters %>%
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
  mutate(home_exp_win = (home_rating*(1-away_rating)) / 
                            ((home_rating*(1-away_rating)) + (away_rating*(1-home_rating))),
         away_exp_win = 1 - home_exp_win,
         exp_winning_jumper = if_else(home_exp_win >= away_exp_win, home_jumper, away_jumper),
         exp_win = if_else(home_exp_win >= away_exp_win, home_exp_win, away_exp_win),
         exp_score_first = (exp_win*(2/3)) + ((1 - exp_win)*(1/3)),
         odds = round(case_when(exp_score_first > .5 ~ (exp_score_first / (1 - (exp_score_first))) * -100,
                                TRUE ~ (100/exp_score_first) - 100), 0))

win_tip_df <-
  today_games_jumper %>%
  mutate(home_concat = paste0(home_team_abbrev, home_jumper),
         away_concat = paste0(away_team_abbrev, away_jumper)) %>%
  filter(home_concat %in% current_centers_concat$concat_field,
         away_concat %in% current_centers_concat$concat_field) %>%
  mutate(home_win_rate = round(home_win_rate, 3),
         away_rating = round(away_rating, 3),
         home_rating = round(home_rating, 3),
         win_tip_prob = round(exp_win, 3),
         team_score_first_prob = round(exp_score_first, 3),
         away_szn_open_tips = paste0(away_wins,"/",away_jumps," (",round(away_win_rate*100, 1),"%)"),
         home_szn_open_tips = paste0(home_wins,"/",home_jumps," (",round(home_win_rate*100, 1),"%)")) %>%
  select(away_team = away_team_abbrev, away_jumper, away_szn_open_tips, away_rating,
         home_team = home_team_abbrev, home_jumper, home_szn_open_tips, home_rating,
         exp_winning_jumper, win_tip_prob, team_score_first_prob, odds)


team_odds <-
  win_tip_df %>%
  mutate(team_win_tip = if_else(home_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first = if_else(home_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
  select(team = home_team, jumper = home_jumper, team_win_tip, team_score_first) %>%
  bind_rows(win_tip_df %>%
              mutate(team_win_tip = if_else(away_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
                     team_score_first = if_else(away_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
              select(team = away_team, jumper = away_jumper, team_win_tip, team_score_first))
# 
# first_shot_df_starters <-
#   first_shot_concat %>%
#   rename(first_shots = shots,
#          first_shot_percent = percentage) %>%
#   select(-odds) %>%
#   mutate(concat_field = paste0(team_abbrev, player)) %>%
#   left_join(current_starters_concat) %>%
#   filter(team_abbrev %in% team_odds$team)

first_shot_df_starters <-
  current_starters_concat %>%
  left_join(first_shot_concat) %>%
  mutate(starts = coalesce(starts, 0),
         first_shots = coalesce(shots, 0),
         first_shot_percent = coalesce(percentage, 0)) %>%
  select(-c(odds, shots, percentage, concat_field)) %>%
  filter(team_abbrev %in% team_odds$team)

first_shot_odds_df <-
  first_shot_df_starters %>%
  rename(team = team_abbrev) %>%
  left_join(team_odds) %>%
  left_join(select(player_usage, PLAYER_NAME, TEAM_ABBREVIATION, USG, FG_USG, FG_PCT), by = c("team" = "TEAM_ABBREVIATION", "player" = "PLAYER_NAME")) %>%
  mutate(first_shot_usg = (first_shot_percent + FG_USG) / 2,
         first_shot_make = first_shot_usg * FG_PCT)
         # game_first_poss_usg_odds = case_when(game_first_shot_usg > .5 ~ (game_first_shot_usg / (1 - (game_first_shot_usg))) * -100,
         #                       TRUE ~ (100/game_first_shot_usg) - 100)) %>%

first_shot_new_df <-
  first_shot_odds_df %>%
  group_by(team, team_score_first) %>%
  mutate(starters_first_make = first_shot_make/sum(first_shot_make),
         game_first_make = starters_first_make * team_score_first,
         game_first_poss_make_odds = case_when(game_first_make > .5 ~ (game_first_make / (1 - (game_first_make))) * -100,
                                               TRUE ~ (100/game_first_make) - 100))

first_shot_score <-
  first_shot_odds_df %>%
  mutate(first_poss_end_on_made_fg = team_first_shot_usg * FG_PCT) %>%
  arrange(team, first_poss_end_on_made_fg)
         #score_basket_on_first_poss = any_poss_end_on_made_fg * team_score_first)



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
