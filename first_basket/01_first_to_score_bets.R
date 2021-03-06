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
  add_row(player_api = "Dennis Smith Jr.", player_lineup = "Dennis Smith") %>%
  add_row(player_api = "Derrick Jones Jr.", player_lineup = "Derrick Jones") %>%
  add_row(player_api = "Vernon Carey Jr.", player_lineup = "Vernon Carey") %>%
  add_row(player_api = "Lonnie Walker IV", player_lineup = "Lonnie Walker") %>%
  add_row(player_api = "Marvin Bagley III", player_lineup = "Marvin Bagley") %>%
  add_row(player_api = "Charlie Brown Jr.", player_lineup = "Charlie Brown") %>%
  add_row(player_api = "Jaren Jackson Jr.", player_lineup = "Jaren Jackson") %>%
  add_row(player_api = "Troy Brown Jr.", player_lineup = "Troy Brown") %>%
  add_row(player_api = "Kira Lewis Jr.", player_lineup = "Kira Lewis") %>%
  add_row(player_api = "Otto Porter Jr.", player_lineup = "Otto Porter") %>%
  add_row(player_api = "Kenyon Martin Jr.", player_lineup = "Kenyon Martin") %>%
  add_row(player_api = "Harry Giles III", player_lineup = "Harry Giles") %>%
  add_row(player_api = "Kevin Knox II", player_lineup = "Kevin Knox") %>%
  add_row(player_api = "Robert Woodard II", player_lineup = "Robert Woodard")

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

# TODO: Fix to use height of jumper as projection
projected_jumpers <-
  current_lineups %>%
  filter(LINEUP_DESC != "") %>%
  left_join(player_name_changes, by = c("PLAYER_NAME" = "player_lineup")) %>%
  mutate(PLAYER_NAME = coalesce(player_api, PLAYER_NAME)) %>%
  left_join(jumper_aggregates, by = c("PLAYER_NAME" = "jumper")) %>%
  # Replace NAs from join
  mutate(jump_rate = replace_na(jump_rate, 0),
         jumps = replace_na(jumps, 0),
         exp_win_adj = replace_na(exp_win_adj, 0),
         starts = replace_na(starts, 0),
         opening_tip_jumps = replace_na(opening_tip_jumps, 0),
         opening_tip_wins = replace_na(opening_tip_wins, 0),
         opening_tip_win_rate = replace_na(opening_tip_win_rate, 0)) %>%
  mutate(jump_rate_over75 = if_else(jump_rate >= 0.75, TRUE, FALSE),
         jump_rate_over15 = if_else(jump_rate >= 0.15, TRUE, FALSE)) %>%
  left_join(list_of_team_abbrev_id, by = c("TEAM_ABBREVIATION" = "team_abbrev")) %>%
  group_by(TEAM_ABBREVIATION) %>%
  # If team has no players with jumps in more than 15% of starts, filter to Center, 
  # Otherwise, if team has multiple players with jumps in more than 75% of starts, filter to player who jumped last
  # Otherwise, filter to the player with the highest jump rate
  filter(if (sum(jump_rate_over15) == 0) STARTING_POSITION == "C" 
         else if (sum(jump_rate_over75) > 1) last_jump == max(last_jump, na.rm = T) 
         else jump_rate == max(jump_rate)) %>%
  # If tie on team for last jump date (trade, etc), filter by more opening season jumps
  filter(opening_tip_jumps == max(opening_tip_jumps)) %>%
  # If tie on team for opening tip jumps this year, filter by total overall jumps
  filter(jumps == max(jumps)) %>%
  # If no players with opening tip jumps this year, pick the center
  select(TEAM_ABBREVIATION, team_id, PLAYER_NAME, TO_PLAY_DESC,
         jumps, exp_win_adj, opening_tip_wins, opening_tip_jumps, opening_tip_win_rate)

today_games <-
  schedule %>%
  distinct(HOME_TEAM_ID, VISITOR_TEAM_ID)

####### Determining First Team To Score Probabilities #########
today_games_jumper <-
  today_games %>%
  left_join(projected_jumpers, by = c("HOME_TEAM_ID" = "team_id")) %>%
  rename(home_jumper = PLAYER_NAME,
         home_injury_status = TO_PLAY_DESC,
         home_team_abbrev = TEAM_ABBREVIATION,
         home_total_jumps = jumps,
         home_opening_wins = opening_tip_wins,
         home_opening_jumps = opening_tip_jumps,
         home_opening_win_rate = opening_tip_win_rate,
         home_rating = exp_win_adj) %>%
  left_join(projected_jumpers, by = c("VISITOR_TEAM_ID" = "team_id")) %>%
  rename(away_jumper = PLAYER_NAME,
         away_injury_status = TO_PLAY_DESC,
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
  select(away_team = away_team_abbrev, away_jumper, away_injury_status, away_szn_open_tips, away_rating,
         home_team = home_team_abbrev, home_jumper, home_injury_status, home_szn_open_tips, home_rating,
         exp_winning_jumper, win_tip_prob, team_exp_score_first, team_exp_score_first_prob, team_score_first_odds)

ftts_output <-
  first_team_to_score_df %>%
  select(team = away_team, jumper = away_jumper, season_open_tips = away_szn_open_tips,
         exp_winning_jumper, win_tip_prob, team_exp_score_first, 
         team_exp_score_first_prob, team_score_first_odds, 
         injury_status = away_injury_status, opp_injury_status = home_injury_status) %>%
  bind_rows(first_team_to_score_df %>%
              select(team = home_team, jumper = home_jumper, season_open_tips = home_szn_open_tips,
                     exp_winning_jumper, win_tip_prob, team_exp_score_first, 
                     team_exp_score_first_prob, team_score_first_odds, 
                     injury_status = home_injury_status, opp_injury_status = away_injury_status)) %>%
  mutate(win_tip_new = if_else(jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first_new = if_else(team == team_exp_score_first, team_exp_score_first_prob, 1 - team_exp_score_first_prob)) %>%
  select(team, jumper, season_open_tips, win_tip_prob = win_tip_new, team_score_first_prob = team_score_first_new, injury_status, opp_injury_status) %>%
  mutate(projected_line = round(case_when(team_score_first_prob > 0.5 ~ (team_score_first_prob / (1 - (team_score_first_prob)) * -100),
                                          TRUE ~ ((100 / team_score_first_prob) - 100)), 0),
         sport = 'nba', prop = 'first team to score') %>%
  select(sport, prop, tidyplayer = jumper, tidyteam = team, projected_line, projected_prob = team_score_first_prob,
         season_open_tips, win_tip_prob, injury_status, opp_injury_status)

########## Determining First Player To Score Odds ############
team_odds <-
  first_team_to_score_df %>%
  mutate(team_win_tip = if_else(home_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first = if_else(team_exp_score_first == home_team, team_exp_score_first_prob, 1 - team_exp_score_first_prob),
         jumper_injury_status = home_injury_status, opp_jumper_injury_status = away_injury_status) %>%
  select(team = home_team, team_win_tip, team_score_first, jumper_injury_status, opp_jumper_injury_status) %>%
  bind_rows(first_team_to_score_df %>%
              mutate(team_win_tip = if_else(away_jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
                     team_score_first = if_else(team_exp_score_first == away_team, team_exp_score_first_prob, 1 - team_exp_score_first_prob),
                     jumper_injury_status = away_injury_status, opp_jumper_injury_status = home_injury_status) %>%
              select(team = away_team, team_win_tip, team_score_first, jumper_injury_status, opp_jumper_injury_status))

first_shot_joined <-
  projected_starters %>%
  left_join(first_shot_aggregates, by = c("PLAYER_NAME" = "player")) %>%
  select(TEAM_ABBREVIATION, LINEUP_DESC, TO_PLAY_DESC, PLAYER_NAME, STARTING_POSITION, starts, shots, percentage) %>%
  mutate(starts = coalesce(starts, 0),
         shots = coalesce(shots, 0),
         percentage = coalesce(percentage, 0)) %>%
  inner_join(team_odds, by = c("TEAM_ABBREVIATION" = "team")) %>%
  select(team = TEAM_ABBREVIATION, player = PLAYER_NAME, injury_status = TO_PLAY_DESC,
         starts, first_shots = shots, first_shot_percent = percentage, team_win_tip, team_score_first,
         jumper_injury_status, opp_jumper_injury_status) %>%
  left_join(select(player_usage, PLAYER_NAME, TEAM_ABBREVIATION, USG, FG_USG, FG_PCT), by = c("team" = "TEAM_ABBREVIATION", "player" = "PLAYER_NAME")) %>%
  mutate(first_shot_usg = (first_shot_percent + FG_USG) / 2,
         first_shot_make = first_shot_usg * FG_PCT)

fpts_output <-
  first_shot_joined %>%
  group_by(team) %>%
  mutate(team_first_shot_make = first_shot_make/sum(first_shot_make),
         projected_prob = team_first_shot_make * team_score_first,
         projected_line = case_when(projected_prob > .5 ~ (projected_prob / (1 - (projected_prob))) * -100,
                                               TRUE ~ (100/projected_prob) - 100)) %>%
  ungroup() %>%
  mutate(first_shot_rate = paste0(first_shots, "/", starts, " (", round(first_shot_percent*100, 1), "%)"),
         fg_usg = round(FG_USG, 3),
         fg_pct = round(FG_PCT, 3),
         first_shot_usg = round(first_shot_usg, 3),
         first_shot_make = round(first_shot_make, 3),
         team_first_shot_make = round(team_first_shot_make, 3),
         projected_prob = round(projected_prob, 3),
         projected_line = round(projected_line),
         sport = 'nba', 
         prop = 'first player to score') %>%
  select(sport, prop, tidyplayer = player, tidyteam = team, projected_line, projected_prob,
         first_shot_rate, fg_usg, fg_pct, jumper_injury_status, opp_jumper_injury_status) %>%
  arrange(tidyteam, desc(projected_prob))
  
## Write out main file for first team to score
write.csv(ftts_output, "data/02_curated/nba_first_to_score/first_team_to_score.csv.gz", row.names = FALSE)

## Write out archive file for first team to score
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(ftts_output, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "first_team_to_score.csv.gz"), row.names = FALSE)

## Write out main file for first player to score
write.csv(fpts_output, "data/02_curated/nba_first_to_score/first_player_to_score.csv.gz", row.names = FALSE)

## Write out archive file for first player to score
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(fpts_output, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "first_player_to_score.csv.gz"), row.names = FALSE)


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

