library(betfinder)
library(data.table)
library(tidyverse)

## Read in model projections and current odds

model_ftts <- fread("data/02_curated/nba_first_to_score/first_team_to_score.csv.gz")
model_fpts <- fread("data/02_curated/nba_first_to_score/first_player_to_score.csv.gz")

dk_ftts <- get_props('draftkings', 'nba', 'ftts')
dk_fpts <- get_props('draftkings', 'nba', 'fpts')

fd_ftts <- get_props('fanduel', 'nba', 'ftts')
fd_fpts <- get_props('fanduel', 'nba', 'fpts')

pb_ftts <- get_props('pointsbet', 'nba', 'ftts')
pb_fpts <- get_props('pointsbet', 'nba', 'fpts')


############# FIRST TEAM TO SCORE ###################
## Update model table to align with odds
model_ftts_new <-
  model_ftts %>%
  select(team = away_team, jumper = away_jumper, season_open_tips = away_szn_open_tips,
         exp_winning_jumper, win_tip_prob, team_exp_score_first, team_exp_score_first_prob, team_score_first_odds) %>%
  bind_rows(model_ftts %>%
              select(team = home_team, jumper = home_jumper, season_open_tips = home_szn_open_tips,
                     exp_winning_jumper, win_tip_prob, team_exp_score_first, team_exp_score_first_prob, team_score_first_odds)) %>%
  mutate(win_tip_new = if_else(jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first_new = if_else(team == team_exp_score_first, team_exp_score_first_prob, 1 - team_exp_score_first_prob)) %>%
  select(team, jumper, season_open_tips, win_tip_prob = win_tip_new, team_score_first_prob = team_score_first_new) %>%
  mutate(projected_odds = round(case_when(team_score_first_prob > 0.5 ~ (team_score_first_prob / (1 - (team_score_first_prob)) * -100),
                                          TRUE ~ ((100 / team_score_first_prob) - 100)), 0)) %>%
  select(team, jumper, season_open_tips, win_tip_prob, projected_odds, team_score_first_prob)


## Updating by site to align team names
team_name_changes_dk <-
  tibble(team = character(),
         team_dk = character()) %>%
  add_row(team = "BKN", team_dk = "BKN Nets") %>%
  add_row(team = "CHA", team_dk = "CHA Hornets") %>%
  add_row(team = "ORL", team_dk = "ORL Magic") %>%
  add_row(team = "MIN", team_dk = "MIN Timberwolves") %>%
  add_row(team = "POR", team_dk = "POR Trail Blazers") %>%
  add_row(team = "LAC", team_dk = "LA Clippers") %>%
  add_row(team = "SAC", team_dk = "SAC Kings") %>%
  add_row(team = "ATL", team_dk = "ATL Hawks") %>%
  add_row(team = "NYK", team_dk = "NY Knicks") %>%
  add_row(team = "NOP", team_dk = "NO Pelicans")  

team_name_changes_fd_pb <-
  tibble(team = character(),
         team_fd_pb = character()) %>%
  add_row(team = "BKN", team_fd_pb = "Brooklyn Nets") %>%
  add_row(team = "CHA", team_fd_pb = "Charlotte Hornets") %>%
  add_row(team = "ORL", team_fd_pb = "Orlando Magic") %>%
  add_row(team = "MIN", team_fd_pb = "Minnesota Timberwolves") %>%
  add_row(team = "POR", team_fd_pb = "Portland Trail Blazers") %>%
  add_row(team = "LAC", team_fd_pb = "Los Angeles Clippers") %>%
  add_row(team = "SAC", team_fd_pb = "Sacramento Kings") %>%
  add_row(team = "ATL", team_fd_pb = "Atlanta Hawks") %>%
  add_row(team = "NYK", team_fd_pb = "New York Knicks") %>%
  add_row(team = "NOP", team_fd_pb = "New Orleans Pelicans")   

ftts_df <-
  model_ftts_new %>%
  left_join(team_name_changes_dk, by = "team") %>%
  left_join(team_name_changes_fd_pb, by = "team") %>%
  left_join(dk_ftts, by = c("team_dk" = "team")) %>%
  rename(DraftKings = odds) %>%
  mutate(DraftKings = as.integer(DraftKings)) %>%
  select(-opponent) %>%
  left_join(select(fd_ftts, name, currentpriceup, currentpricedown), by = c("team_fd_pb" = "name")) %>%
  mutate(fd_fraction = currentpriceup/currentpricedown) %>%
  mutate(FanDuel = round(case_when(fd_fraction < 1 ~ -100 / fd_fraction,
                                             TRUE ~ fd_fraction * 100), 0)) %>%
  select(-c(currentpriceup, currentpricedown, fd_fraction)) %>%
  left_join(select(pb_ftts, name, price), by = c("team_fd_pb" = "name")) %>%
  mutate(PointsBet = round(case_when(price < 2 ~ -100 / (price - 1),
                                   TRUE ~ (price - 1) * 100), 0)) %>%
  select(-c(team_dk, team_fd_pb, price))

ftts_pivot <-
  ftts_df %>%
  pivot_longer(!c(team, jumper, season_open_tips, win_tip_prob, team_score_first_prob, projected_odds),
               names_to = "site_name",
               values_to = "site_odds") %>%
  mutate(site_prob = round(case_when(site_odds >= 100 ~ 100 / (site_odds + 100),
                                      TRUE ~ (site_odds * -1) / ((site_odds * -1) + 100)), 3),
         edge_num = ifelse(is.na(site_odds), NA, round((team_score_first_prob - site_prob) * 100, 1)),
         edge = ifelse(is.na(site_odds), NA, paste0(round((team_score_first_prob - site_prob) * 100, 1), "%")),
         play = if_else(team_score_first_prob > site_prob, "Yes", "No")) %>%
  group_by(team) %>%
  mutate(best_play = if_else(play == "Yes" & edge_num == max(edge_num, na.rm = T), "Yes", "No")) %>%
  arrange(desc(best_play), desc(play), desc(edge_num))

ftts_output <-
  ftts_pivot %>%
  select(-edge_num)

ftts_by_book <-
  ftts_pivot %>%
  arrange(site_name, desc(edge_num)) %>%
  select(-edge_num)

ftts_plays <-
  ftts_pivot %>%
  filter(play == "Yes")

ftts_best_plays <-
  ftts_pivot %>%
  filter(best_play == "Yes")

#################### FIRST PLAYER TO SCORE ######################
player_name_changes_dk <-
  tibble(player = character(),
         player_site = character()) %>%
  add_row(player = "Bogdan Bogdanovic", player_site = "Bogdan Bogdanović") %>%
  add_row(player = "P.J. Washington", player_site = "PJ Washington") %>%
  add_row(player = "Marcus Morris Sr.", player_site = "Marcus Morris")

player_name_changes_fd <-
  tibble(player = character(),
         player_site = character()) %>%
  add_row(player = "Maurice Harkless", player_site = "Moe Harkless") %>%
  add_row(player = "P.J. Washington", player_site = "P.J Washington") %>%
  add_row(player = "Wendell Carter Jr.", player_site = "Wendell Carter") %>%
  add_row(player = "Karl-Anthony Towns", player_site = "Karl Anthony Towns") %>%
  add_row(player = "Marcus Morris Sr.", player_site = "Marcus Morris")

player_name_changes_pb <-
  tibble(player = character(),
         player_site = character()) %>%
  add_row(player = "P.J. Washington", player_site = "P.J. Washington Jr.") %>%
  add_row(player = "Wendell Carter Jr.", player_site = "Wendell Carter") %>%
  add_row(player = "Marcus Morris Sr.", player_site = "Marcus Morris") %>%
  add_row(player = "CJ McCollum", player_site = "C.J. McCollum")

fpts_joined <-
  model_fpts %>%
  left_join(player_name_changes_dk, by = "player") %>%
  mutate(player_site = coalesce(player_site, player)) %>%
  left_join(select(dk_fpts, label, oddsAmerican) , by = c("player_site" = "label")) %>%
  mutate(DraftKings = as.integer(oddsAmerican)) %>%
  select(-c(player_site, oddsAmerican)) %>%
  left_join(player_name_changes_fd, by = "player") %>%
  mutate(player_site = coalesce(player_site, player)) %>%
  left_join(select(fd_fpts, name, currentpriceup, currentpricedown), by = c("player_site" = "name")) %>%
  mutate(fd_fraction = currentpriceup/currentpricedown) %>%
  mutate(FanDuel = round(case_when(fd_fraction < 1 ~ -100 / fd_fraction,
                                   TRUE ~ fd_fraction * 100), 0)) %>%
  select(-c(player_site, currentpriceup, currentpricedown, fd_fraction)) %>%
  left_join(player_name_changes_pb, by = "player") %>%
  mutate(player_site = coalesce(player_site, player)) %>%
  left_join(select(pb_fpts, name, price) , by = c("player_site" = "name")) %>%
  mutate(PointsBet = round(case_when(price < 2 ~ -100 / (price - 1),
                                     TRUE ~ (price - 1) * 100), 0)) %>%
  select(-c(player_site, price))

fpts_pivot <-
  fpts_joined %>%
  select(-c(first_shot_usg, first_shot_make, team_first_shot_make)) %>%
  rename(player_score_first_prob = game_first_shot_make, projected_odds = first_make_fg_odds) %>%
  pivot_longer(!c(team, player, first_shot_rate, FG_USG, FG_PCT, team_score_first, 
                  player_score_first_prob, projected_odds),
               names_to = "site_name",
               values_to = "site_odds") %>%
  mutate(site_abv = case_when(site_name == "DraftKings" ~ "DK",
                              site_name == "FanDuel" ~ "FD",
                              site_name == "PointsBet" ~ "PB"),
         site_prob = round(case_when(site_odds >= 100 ~ 100 / (site_odds + 100),
                                     TRUE ~ (site_odds * -1) / ((site_odds * -1) + 100)), 3),
         edge_num = ifelse(is.na(site_odds), NA, round((player_score_first_prob - site_prob) * 100, 1)),
         edge = ifelse(is.na(site_odds), NA, paste0(round((player_score_first_prob - site_prob) * 100, 1), "%")),
         play = if_else(player_score_first_prob > site_prob, "Yes", "No")) %>%
  group_by(player) %>%
  mutate(best_play = if_else(play == "Yes" & edge_num == max(edge_num, na.rm = T), "Yes", "No")) %>%
  group_by(player, best_play) %>%
  mutate(best_play = if_else(best_play == "Yes", paste0(best_play, " - ", paste(site_abv, collapse = ', ')), "No")) %>%
  arrange(desc(edge_num)) %>%
  select(-c(site_abv, edge_num))

fpts_plays <-
  fpts_pivot %>%
  filter(play == "Yes")











         