library(data.table)
library(tidyverse)
library(lubridate)

## Set Parameter values
today_full_date <- gsub("-", "", Sys.Date())

## Read Data into Environment
first_shot <- fread("data/02_curated/nba_first_to_score/current_season_first_shot.csv.gz")
player_usage <- fread("data/02_curated/nba_first_to_score/current_season_usage_rate.csv.gz")
current_lineups <- fread("data/02_curated/nba_lineups/rotowire.csv")
current_rosters <- fread("data/02_curated/nba_rosters/current.csv.gz")

score_first_output <- 
  fread("data/02_curated/nba_first_to_score/score_first_outputs.csv.gz") %>%
  filter(game_date == as.Date(today_full_date, format = "%Y%m%d")) %>%
  mutate(sport = 'nba',
         prop = 'first team to score') 

first_shot_aggregates <-
  first_shot %>%
  group_by(player) %>%
  summarise(starts = sum(starts),
            shots = sum(shots),
            percentage = shots/starts,
            .groups = 'drop')

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

# If we have projected lineups for the day's games
if(nrow(current_lineups > 0)){
  
  projected_starters <-
    current_lineups %>%
    filter(LINEUP_DESC != "") %>%
    left_join(player_name_changes, by = c("PLAYER_NAME" = "player_lineup")) %>%
    mutate(PLAYER_NAME = coalesce(player_api, PLAYER_NAME))
  
  ftts_df <-
    score_first_output %>%
    left_join(select(projected_starters, PLAYER_NAME, TO_PLAY_DESC), by = c("home_team_jumper" = "PLAYER_NAME")) %>%
    rename(home_team_injury_status = TO_PLAY_DESC) %>%
    left_join(select(projected_starters, PLAYER_NAME, TO_PLAY_DESC), by = c("away_team_jumper" = "PLAYER_NAME")) %>%
    rename(away_team_injury_status = TO_PLAY_DESC)
  
  ftts_output <-
    ftts_df %>%
    select(sport, prop, jumper = away_team_jumper, team = away_team_abbrev, 
           win_tip_prob = final_win_prob_all, team_score_first_prob = final_score_first_prob_all, 
           injury_status = away_team_injury_status, opp_injury_status = home_team_injury_status) %>%
    mutate(win_tip_prob = 1 - win_tip_prob, 
           team_score_first_prob = 1 - team_score_first_prob) %>%
    bind_rows(ftts_df %>%
                select(sport, prop, jumper = home_team_jumper, team = home_team_abbrev, 
                       win_tip_prob = final_win_prob_all, team_score_first_prob = final_score_first_prob_all, 
                       injury_status = home_team_injury_status, opp_injury_status = away_team_injury_status)) %>%
    mutate(projected_line = round(case_when(team_score_first_prob > 0.5 ~ (team_score_first_prob / (1 - (team_score_first_prob)) * -100),
                                            TRUE ~ ((100 / team_score_first_prob) - 100)), 0)) %>%
    select(sport, prop, tidyplayer = jumper, tidyteam = team, projected_line, 
           projected_prob = team_score_first_prob, win_tip_prob, injury_status, opp_injury_status)
  
  ########## Determining First Player To Score Odds ############
  first_shot_joined <-
    projected_starters %>%
    left_join(first_shot_aggregates, by = c("PLAYER_NAME" = "player")) %>%
    select(TEAM_ABBREVIATION, LINEUP_DESC, TO_PLAY_DESC, PLAYER_NAME, STARTING_POSITION, starts, shots, percentage) %>%
    mutate(starts = coalesce(starts, 0),
           shots = coalesce(shots, 0),
           percentage = coalesce(percentage, 0)) %>%
    inner_join(ftts_output, by = c("TEAM_ABBREVIATION" = "tidyteam")) %>%
    select(team = TEAM_ABBREVIATION, player = PLAYER_NAME, injury_status = TO_PLAY_DESC,
           starts, first_shots = shots, first_shot_percent = percentage, team_win_tip = win_tip_prob, team_score_first = projected_prob,
           jumper_injury_status = injury_status, opp_jumper_injury_status = opp_injury_status) %>%
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
  
}
