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
               tidyplayer = character(),
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
#Removing the tidyplayer field for jumpers
model_ftts <- 
  model_ftts %>%
  select(-tidyplayer)

ftts_df <-
  model_ftts %>%
  left_join(dk_ftts, by = c("tidyteam", "prop", "sport")) %>%
  rename(DraftKings = tidyamericanodds) %>%
  select(-timestamp, -site) %>%
  left_join(fd_ftts, by = c("tidyteam", "prop", "sport", "tidyplayer")) %>%
  rename(FanDuel = tidyamericanodds) %>%
  select(-timestamp, -site) %>%
  left_join(pb_ftts, by = c("tidyteam", "prop", "sport", "tidyplayer")) %>%
  rename(PointsBet = tidyamericanodds) %>%
  mutate(PointsBet = round(PointsBet, 0)) %>%
  select(-prop, -sport, -timestamp, -site)

ftts_pivot <-
  ftts_df %>%
  pivot_longer(!c(tidyplayer, tidyteam, season_open_tips, win_tip_prob, 
                  projected_prob, projected_line, injury_status, opp_injury_status),
               names_to = "site_name",
               values_to = "site_odds") %>%
  mutate(site_abv = case_when(site_name == "DraftKings" ~ "DK",
                              site_name == "FanDuel" ~ "FD",
                              site_name == "PointsBet" ~ "PB"),
         site_prob = round(case_when(site_odds >= 100 ~ 100 / (site_odds + 100),
                                      TRUE ~ (site_odds * -1) / ((site_odds * -1) + 100)), 3),
         edge_num = ifelse(is.na(site_odds), NA, round((projected_prob - site_prob) * 100, 1)),
         edge = ifelse(is.na(site_odds), NA, paste0(round((projected_prob - site_prob) * 100, 1), "%")),
         play = if_else(projected_prob > site_prob, "Yes", "No")) %>%
  group_by(tidyteam) %>%
  mutate(best_play = if_else(play == "Yes" & edge_num == max(edge_num, na.rm = T), "Yes", "No")) %>%
  arrange(desc(edge_num)) %>%
  group_by(tidyteam, best_play) %>%
  mutate(best_play = if_else(best_play == "Yes", paste0(best_play, " - ", paste(site_abv, collapse = ', ')), "No")) %>%
  select(-site_abv) %>%
  relocate(c(injury_status, opp_injury_status), .after = last_col())

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
  select(tidyplayer, tidyteam, projected_line, site_name, site_odds, edge, play, best_play, injury_status, opp_injury_status)

#################### FIRST PLAYER TO SCORE ######################
fpts_joined <-
  model_fpts %>%
  left_join(dk_fpts, by = c("tidyplayer", "sport", "prop")) %>%
  rename(DraftKings = tidyamericanodds) %>%
  select(-timestamp, -site) %>%
  left_join(fd_fpts, by = c("tidyplayer", "sport", "prop")) %>%
  rename(FanDuel = tidyamericanodds) %>%
  select(-timestamp, -site) %>%
  left_join(pb_fpts, by = c("tidyplayer", "sport", "prop")) %>%
  rename(PointsBet = tidyamericanodds) %>%
  select(-prop, -sport, -timestamp, -site)

fpts_pivot <-
  fpts_joined %>%
  pivot_longer(!c(tidyplayer, tidyteam, first_shot_rate, fg_usg, fg_pct, 
                  projected_prob, projected_line, jumper_injury_status, opp_jumper_injury_status),
               names_to = "site_name",
               values_to = "site_odds") %>%
  mutate(site_abv = case_when(site_name == "DraftKings" ~ "DK",
                              site_name == "FanDuel" ~ "FD",
                              site_name == "PointsBet" ~ "PB"),
         site_prob = round(case_when(site_odds >= 100 ~ 100 / (site_odds + 100),
                                     TRUE ~ (site_odds * -1) / ((site_odds * -1) + 100)), 3),
         edge_num = ifelse(is.na(site_odds), NA, round((projected_prob - site_prob) * 100, 1)),
         edge = ifelse(is.na(site_odds), NA, paste0(round((projected_prob - site_prob) * 100, 1), "%")),
         play = if_else(projected_prob > site_prob, "Yes", "No")) %>%
  group_by(tidyplayer) %>%
  mutate(best_play = if_else(play == "Yes" & edge_num == max(edge_num, na.rm = T), "Yes", "No")) %>%
  group_by(tidyplayer, best_play) %>%
  mutate(best_play = if_else(best_play == "Yes", paste0(best_play, " - ", paste(site_abv, collapse = ', ')), "No")) %>%
  arrange(desc(edge_num)) %>%
  select(-c(site_abv, edge_num)) %>%
  relocate(c(jumper_injury_status, opp_jumper_injury_status), .after = last_col())

fpts_plays <-
  fpts_pivot %>%
  filter(play == "Yes")

fpts_minimal <-
  fpts_plays %>%
  select(tidyteam, tidyplayer, first_shot_rate, projected_line, 
         site_name, site_odds, edge, play, best_play, 
         jumper_injury_status, opp_jumper_injury_status)



