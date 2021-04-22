#remotes::install_github('jimtheflash/betfinder')
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

## Creating empty tables if they don't exist (e.g. no props found)
create_table_function <- function(df_name, df, prop){
  if (exists(df_name)) {
    output <- df
  }else{
    if (prop == "ftts") {
      output <-
        tibble(tidyteam = character(),
               tidyopponent = character(),
               tidyamericanodds = numeric(),
               prop = character(),
               site = character(),
               sport = character(),
               timestamp = as.POSIXct(NA))
    }else{
      output <-
        tibble(tidyplayer = character(),
               tidyamericanodds = numeric(),
               prop = character(),
               site = character(),
               sport = character(),
               timestamp = as.POSIXct(NA))
    }
  }
  return(output)
}

dk_ftts <- create_table_function('dk_ftts', dk_ftts, 'ftts')
dk_fpts <- create_table_function('dk_fpts', dk_fpts, 'fpts')

fd_ftts <- create_table_function('fd_ftts', fd_ftts, 'ftts')
fd_fpts <- create_table_function('fd_fpts', fd_fpts, 'fpts')

pb_ftts <- create_table_function('pb_ftts', pb_ftts, 'ftts')
pb_fpts <- create_table_function('pb_fpts', pb_fpts, 'fpts')

############# FIRST TEAM TO SCORE ###################
## Update model table to align with odds
model_ftts_df <-
  model_ftts %>%
  select(team = away_team, jumper = away_jumper, season_open_tips = away_szn_open_tips,
         exp_winning_jumper, win_tip_prob, team_exp_score_first, 
         team_exp_score_first_prob, team_score_first_odds, injury_status = away_injury_status) %>%
  bind_rows(model_ftts %>%
              select(team = home_team, jumper = home_jumper, season_open_tips = home_szn_open_tips,
                     exp_winning_jumper, win_tip_prob, team_exp_score_first, 
                     team_exp_score_first_prob, team_score_first_odds, injury_status = home_injury_status)) %>%
  mutate(win_tip_new = if_else(jumper == exp_winning_jumper, win_tip_prob, 1 - win_tip_prob),
         team_score_first_new = if_else(team == team_exp_score_first, team_exp_score_first_prob, 1 - team_exp_score_first_prob)) %>%
  select(team, jumper, season_open_tips, win_tip_prob = win_tip_new, team_score_first_prob = team_score_first_new, injury_status) %>%
  mutate(projected_odds = round(case_when(team_score_first_prob > 0.5 ~ (team_score_first_prob / (1 - (team_score_first_prob)) * -100),
                                          TRUE ~ ((100 / team_score_first_prob) - 100)), 0)) %>%
  select(team, jumper, season_open_tips, win_tip_prob, projected_odds, team_score_first_prob, injury_status)

ftts_df <-
  model_ftts_df %>%
  left_join(dk_ftts, by = c("team" = "tidyteam")) %>%
  rename(DraftKings = tidyamericanodds) %>%
  select(-prop, -tidyopponent, -sport, -timestamp, -site) %>%
  left_join(fd_ftts, by = c("team" = "tidyteam")) %>%
  rename(FanDuel = tidyamericanodds) %>%
  select(-prop, -tidyopponent, -sport, -timestamp, -site) %>%
  left_join(pb_ftts, by = c("team" = "tidyteam")) %>%
  rename(PointsBet = tidyamericanodds) %>%
  mutate(PointsBet = round(PointsBet, 0)) %>%
  select(-prop, -tidyopponent, -sport, -timestamp, -site)

ftts_pivot <-
  ftts_df %>%
  pivot_longer(!c(team, jumper, season_open_tips, win_tip_prob, team_score_first_prob, projected_odds, injury_status),
               names_to = "site_name",
               values_to = "site_odds") %>%
  mutate(site_abv = case_when(site_name == "DraftKings" ~ "DK",
                              site_name == "FanDuel" ~ "FD",
                              site_name == "PointsBet" ~ "PB"),
         site_prob = round(case_when(site_odds >= 100 ~ 100 / (site_odds + 100),
                                      TRUE ~ (site_odds * -1) / ((site_odds * -1) + 100)), 3),
         edge_num = ifelse(is.na(site_odds), NA, round((team_score_first_prob - site_prob) * 100, 1)),
         edge = ifelse(is.na(site_odds), NA, paste0(round((team_score_first_prob - site_prob) * 100, 1), "%")),
         play = if_else(team_score_first_prob > site_prob, "Yes", "No")) %>%
  group_by(team) %>%
  mutate(best_play = if_else(play == "Yes" & edge_num == max(edge_num, na.rm = T), "Yes", "No")) %>%
  arrange(desc(best_play), desc(play), desc(edge_num)) %>%
  group_by(team, best_play) %>%
  mutate(best_play = if_else(best_play == "Yes", paste0(best_play, " - ", paste(site_abv, collapse = ', ')), "No")) %>%
  select(-site_abv) %>%
  relocate(injury_status, .after = last_col())

ftts_output <-
  ftts_pivot %>%
  select(-edge_num)

ftts_by_book <-
  ftts_pivot %>%
  arrange(site_name, desc(edge_num)) %>%
  select(-edge_num)

ftts_plays <-
  ftts_pivot %>%
  filter(play == "Yes") %>%
  select(-edge_num)

ftts_best_plays <-
  ftts_pivot %>%
  filter(str_detect(best_play, "Yes")) %>%
  select(-edge_num)

ftts_minimal <-
  ftts_plays %>%
  select(team, jumper, season_open_tips, projected_odds, site_name, site_odds, edge, play, best_play)

#################### FIRST PLAYER TO SCORE ######################
fpts_joined <-
  model_fpts %>%
  left_join(dk_fpts, by = c("player" = "tidyplayer")) %>%
  rename(DraftKings = tidyamericanodds) %>%
  select(-prop, -sport, -timestamp, -site) %>%
  left_join(fd_fpts, by = c("player" = "tidyplayer")) %>%
  rename(FanDuel = tidyamericanodds) %>%
  select(-prop, -sport, -timestamp, -site) %>%
  left_join(pb_fpts, by = c("player" = "tidyplayer")) %>%
  rename(PointsBet = tidyamericanodds) %>%
  select(-prop, -sport, -timestamp, -site)

fpts_pivot <-
  fpts_joined %>%
  select(-c(first_shot_usg, first_shot_make, team_first_shot_make)) %>%
  rename(player_score_first_prob = game_first_shot_make, projected_odds = first_make_fg_odds) %>%
  pivot_longer(!c(team, player, first_shot_rate, FG_USG, FG_PCT, team_score_first, 
                  player_score_first_prob, projected_odds, injury_status),
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
  select(-c(site_abv, edge_num)) %>%
  relocate(injury_status, .after = last_col())

fpts_plays <-
  fpts_pivot %>%
  filter(play == "Yes")

fpts_minimal <-
  fpts_plays %>%
  select(team, player, first_shot_rate, projected_odds, site_name, site_odds, edge, play, best_play)



