library(data.table)
library(tidyverse)
library(lubridate)

## Set Parameter values
today_full_date <- gsub("-", "", Sys.Date())
home_tip_win_parameter <- .508

## Read Data into Environment
schedule <- fread(paste0("data/nba_schedules/", today_full_date, ".csv"))
player_ratings <- fread("data/02_curated/nba_first_to_score/jump_ball_ratings.csv.gz")
opening_tip <- fread("data/02_curated/nba_first_to_score/current_season_opening_tip.csv.gz")
first_shot <- fread("data/02_curated/nba_first_to_score/current_season_first_shot.csv.gz")
player_usage <- fread("data/02_curated/nba_first_to_score/current_season_usage_rate.csv.gz")
current_lineups <- fread("data/02_curated/nba_lineups/rotowire.csv")
current_rosters <- fread("data/02_curated/nba_rosters/current.csv.gz")

## Update Player Names (hopefully change to engineering end eventually)
player_name_changes <-
  tibble(player_api = character(),
         player_lineup = character()) %>%
  add_row(player_api = "Michael Porter Jr.", player_lineup = "Michael Porter") %>%
  add_row(player_api = "Gary Trent Jr.", player_lineup = "Gary Trent") %>%
  add_row(player_api = "James Ennis III", player_lineup = "James Ennis") %>%
  add_row(player_api = "Robert Williams III", player_lineup = "Robert Williams") %>%
  add_row(player_api = "Larry Nance Jr.", player_lineup = "Larry Nance") %>%
  add_row(player_api = "Kelly Oubre Jr.", player_lineup = "Kelly Oubre") %>%
  add_row(player_api = "Kevin Porter Jr.", player_lineup = "Kevin Porter") %>%
  add_row(player_api = "Marcus Morris Sr.", player_lineup = "Marcus Morris") %>%
  add_row(player_api = "Danuel House Jr.", player_lineup = "Danuel House") %>%
  add_row(player_api = "Tim Hardaway Jr.", player_lineup = "Tim Hardaway") %>%
  add_row(player_api = "Wendell Carter Jr.", player_lineup = "Wendell Carter") %>%
  add_row(player_api = "Dennis Smith Jr.", player_lineup = "Dennis Smith")

## List of teams playing today for a join
list_of_team_abbrev_id <-
  opening_tip %>%
  distinct(team_abbrev, team_id)

opening_tip_aggregates <-
  opening_tip %>%
  group_by(jumper) %>%
  summarise(opening_tip_jumps = sum(jumps), 
            opening_tip_wins = sum(wins), 
            opening_tip_win_rate = opening_tip_wins/opening_tip_jumps,
            last_jump = max(last_jump),
            .groups = 'drop')

first_shot_aggregates <-
  first_shot %>%
  group_by(player) %>%
  summarise(starts = sum(starts),
            shots = sum(shots),
            percentage = shots/starts,
            .groups = 'drop')

# Need to fix exp win to the height value of that player
jumper_aggregates <-
  current_rosters %>%
  left_join(player_ratings, by = c("PLAYER_NAME" = "jumper")) %>%
  rename(jumper = PLAYER_NAME) %>%
  select(jumper, jumps, exp_win_adj) %>%
  left_join(opening_tip_aggregates, by = "jumper") %>%
  left_join(select(first_shot_aggregates, player, starts), by = c("jumper" = "player")) %>%
  mutate(jumps = replace_na(jumps, 0),
         exp_win_adj = replace_na(exp_win_adj, 0),
         opening_tip_jumps = replace_na(opening_tip_jumps, 0),
         opening_tip_wins = replace_na(opening_tip_wins, 0),
         opening_tip_win_rate = replace_na(opening_tip_win_rate, 0),
         starts = replace_na(starts, 0)) %>%
  mutate(jump_rate = opening_tip_jumps/starts)

projected_starters <-
  current_lineups %>%
  filter(LINEUP_DESC != "") %>%
  left_join(player_name_changes, by = c("PLAYER_NAME" = "player_lineup")) %>%
  mutate(PLAYER_NAME = coalesce(player_api, PLAYER_NAME))

projected_jumpers <-
  current_lineups %>%
  filter(LINEUP_DESC != "") %>%
  left_join(player_name_changes, by = c("PLAYER_NAME" = "player_lineup")) %>%
  mutate(PLAYER_NAME = coalesce(player_api, PLAYER_NAME)) %>%
  left_join(jumper_aggregates, by = c("PLAYER_NAME" = "jumper")) %>%
  mutate(jump_rate = if_else(is.nan(jump_rate), 0, jump_rate)) %>%
  left_join(list_of_team_abbrev_id, by = c("TEAM_ABBREVIATION" = "team_abbrev")) %>%
  group_by(TEAM_ABBREVIATION) %>%
  # Filter to player who jumps in highest percent of starts
  filter(jump_rate == max(jump_rate)) %>%
  # If team has no players that has jumped this season, filter to Center, otherwise filter to the player who jumped last
  filter(if (sum(jump_rate) == 0) STARTING_POSITION == "C" else last_jump == max(last_jump, na.rm = T)) %>%
  # If tie on team for last jump date (trade, etc), filter by more opening season jumps
  filter(opening_tip_jumps == max(opening_tip_jumps)) %>%
  # If tie on team for opening tip jumps this year, filter by total overall jumps
  filter(jumps == max(jumps)) %>%
  # If no players with opening tip jumps this year, pick the center
  select(TEAM_ABBREVIATION, team_id, PLAYER_NAME, jumps, exp_win_adj, opening_tip_wins, opening_tip_jumps, opening_tip_win_rate)

today_games <-
  schedule %>%
  distinct(HOME_TEAM_ID, VISITOR_TEAM_ID)

####### Determining First Team To Score Probabilities #########
today_games_jumper <-
  today_games %>%
  left_join(projected_jumpers, by = c("HOME_TEAM_ID" = "team_id")) %>%
  rename(home_jumper = PLAYER_NAME,
         home_team_abbrev = TEAM_ABBREVIATION,
         home_total_jumps = jumps,
         home_opening_wins = opening_tip_wins,
         home_opening_jumps = opening_tip_jumps,
         home_opening_win_rate = opening_tip_win_rate,
         home_rating = exp_win_adj) %>%
  left_join(projected_jumpers, by = c("VISITOR_TEAM_ID" = "team_id")) %>%
  rename(away_jumper = PLAYER_NAME,
         away_team_abbrev = TEAM_ABBREVIATION,
         away_total_jumps = jumps,
         away_opening_wins = opening_tip_wins,
         away_opening_jumps = opening_tip_jumps,
         away_opening_win_rate = opening_tip_win_rate,
         away_rating = exp_win_adj) %>%
  mutate(home_exp_win = (home_rating*(1-away_rating)*home_tip_win_parameter) / 
                        ((home_rating*(1-away_rating)*home_tip_win_parameter) + (away_rating*(1-home_rating)*(1-home_tip_win_parameter))),
         away_exp_win = 1 - home_exp_win,
         exp_winning_jumper = if_else(home_exp_win >= away_exp_win, home_jumper, away_jumper),
         home_exp_score_first = (home_exp_win*.61) + (away_exp_win*.41),
         away_exp_score_first = 1 - home_exp_score_first,
         team_exp_score_first = if_else(home_exp_score_first >= away_exp_score_first, home_team_abbrev, away_team_abbrev),
         team_exp_score_first_prob = if_else(home_exp_score_first >= away_exp_score_first, home_exp_score_first, away_exp_score_first),
         team_score_first_odds = round(case_when(team_exp_score_first == home_team_abbrev ~ (home_exp_score_first / (1 - (home_exp_score_first)) * -100),
                                                 TRUE ~ (away_exp_score_first / (1 - (away_exp_score_first)) * -100)), 0)) %>%
  select(-c(HOME_TEAM_ID, VISITOR_TEAM_ID))

first_team_to_score_df <-
  today_games_jumper %>%
  mutate(away_rating = round(away_rating, 3),
         home_rating = round(home_rating, 3),
         win_tip_prob = round(if_else(home_exp_win >= away_exp_win, home_exp_win, away_exp_win), 3),
         team_exp_score_first_prob = round(team_exp_score_first_prob, 3),
         away_szn_open_tips = paste0(away_opening_wins, "/", away_opening_jumps, " (", round(away_opening_win_rate*100, 1), "%)"),
         home_szn_open_tips = paste0(home_opening_wins, "/", home_opening_jumps, " (", round(home_opening_win_rate*100, 1), "%)")) %>%
  select(away_team = away_team_abbrev, away_jumper, away_szn_open_tips, away_rating,
         home_team = home_team_abbrev, home_jumper, home_szn_open_tips, home_rating,
         exp_winning_jumper, win_tip_prob, team_exp_score_first, team_exp_score_first_prob, team_score_first_odds)


########## Determining First Player To Score Odds ############
team_odds <-
  first_team_to_score_df %>%
  mutate(team_win_tip = if_else(home_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first = if_else(home_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
  select(team = home_team, team_win_tip, team_score_first) %>%
  bind_rows(first_team_to_score_df %>%
              mutate(team_win_tip = if_else(away_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
                     team_score_first = if_else(away_jumper == exp_winning_jumper, team_score_first_prob, 1 - team_score_first_prob)) %>%
              select(team = away_team, team_win_tip, team_score_first))

first_shot_joined <-
  projected_starters %>%
  left_join(first_shot_aggregates, by = c("PLAYER_NAME" = "player")) %>%
  select(TEAM_ABBREVIATION, LINEUP_DESC, TO_PLAY_DESC, PLAYER_NAME, STARTING_POSITION, starts, shots, percentage) %>%
  mutate(starts = coalesce(starts, 0),
         shots = coalesce(shots, 0),
         percentage = coalesce(percentage, 0)) %>%
  inner_join(team_odds, by = c("TEAM_ABBREVIATION" = "team")) %>%
  select(team = TEAM_ABBREVIATION, player = PLAYER_NAME, starts, first_shots = shots, first_shot_percent = percentage, team_win_tip, team_score_first) %>%
  left_join(select(player_usage, PLAYER_NAME, TEAM_ABBREVIATION, USG, FG_USG, FG_PCT), by = c("team" = "TEAM_ABBREVIATION", "player" = "PLAYER_NAME")) %>%
  mutate(first_shot_usg = (first_shot_percent + FG_USG) / 2,
         first_shot_make = first_shot_usg * FG_PCT)

first_player_to_score_df <-
  first_shot_joined %>%
  group_by(team) %>%
  mutate(team_first_shot_make = first_shot_make/sum(first_shot_make),
         game_first_shot_make = team_first_shot_make * team_score_first,
         first_make_fg_odds = case_when(game_first_shot_make > .5 ~ (game_first_shot_make / (1 - (game_first_shot_make))) * -100,
                                               TRUE ~ (100/game_first_shot_make) - 100)) %>%
  ungroup() %>%
  mutate(first_shot_rate = paste0(first_shots, "/", starts, " (", round(first_shot_percent*100, 1), "%)"),
         FG_USG = round(FG_USG, 3),
         FG_PCT = round(FG_PCT, 3),
         first_shot_usg = round(first_shot_usg, 3),
         first_shot_make = round(first_shot_make, 3),
         team_first_shot_make = round(team_first_shot_make, 3),
         game_first_shot_make = round(game_first_shot_make, 3),
         first_make_fg_odds = round(first_make_fg_odds)) %>%
  select(team, player, first_shot_rate, FG_USG, FG_PCT, team_score_first, 
         first_shot_usg, first_shot_make, team_first_shot_make, game_first_shot_make, first_make_fg_odds) %>%
  arrange(team, first_make_fg_odds)
  
## Write out main file for first team to score
write.csv(first_team_to_score_df, "data/02_curated/nba_first_to_score/first_team_to_score.csv.gz", row.names = FALSE)

## Write out archive file for first team to score
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(first_team_to_score_df, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "first_team_to_score.csv.gz"), row.names = FALSE)

## Write out main file for first player to score
write.csv(first_player_to_score_df, "data/02_curated/nba_first_to_score/first_player_to_score.csv.gz", row.names = FALSE)

## Write out archive file for first player to score
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(first_player_to_score_df, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "first_player_to_score.csv.gz"), row.names = FALSE)


# calculate_jump_odds <- function(player_1, player_2, player_ratings){
# 
#   player_1_data <-
#     player_ratings %>%
#     filter(jumper == player_1)
# 
#   player_2_data <-
#     player_ratings %>%
#     filter(jumper == player_2)
# 
#   if (nrow(player_1_data) == 0 | nrow(player_2_data) == 0) {
#     stop("Check to make sure you spelled a player's name correctly!")
#   }
#   #include home win parameter
# 
#   player_1_win <-
#     round((player_1_data$exp_win_adj*(1-player_2_data$exp_win_adj)) /
#             ((player_1_data$exp_win_adj*(1-player_2_data$exp_win_adj)) + (player_2_data$exp_win_adj*(1-player_1_data$exp_win_adj))), 2)
# 
#   # Always print favored player first
#   if (player_1_win >= .5) {
#     message(paste0(player_1, " wins a tip vs ", player_2, " ", player_1_win * 100, " percent of the time"))
#   }else{
#     message(paste0(player_2, " wins a tip vs ", player_1, " ", (1 - player_1_win)*100, " percent of the time"))
#   }
# }
# 
# player_1 <- "Bismack Biyombo"
# player_2 <- "Brook Lopez"
# calculate_jump_odds(player_1, player_2, player_ratings)

