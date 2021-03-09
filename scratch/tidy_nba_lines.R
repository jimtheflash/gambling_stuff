library(tidyverse)


# totals ------------------------------------------------------------------

tot <- list.files('./data/nba_lines', pattern = 'total')

tot_list <- list()
for (sp in tot) {
  tot_raw <- read.csv(paste0('./data/nba_lines/', sp))
  tot_list[[length(tot_list) + 1]] <- tot_raw
}

tot_df <- bind_rows(tot_list)
tot_df_long <- tot_df %>%
  transmute(game_date = lubridate::as_date(as.character(Date)),
            team = home_Team,
            home_away = 'home',
            open_bm_total_line = home_open_line,
            open_bm_total_odds = home_open_odds,
            close_bm_total_line = home_bookmaker_line,
            close_bm_total_odds = home_bookmaker_odds,
            close_pncl_total_line = home_pinnacle_line,
            close_pncl_total_odds = home_pinnacle_odds) %>%
  bind_rows(
    tot_df %>%
      transmute(game_date = lubridate::as_date(as.character(Date)),
                team = away_Team,
                home_away = 'away',
                open_bm_total_line = away_open_line,
                open_bm_total_odds = away_open_odds,
                close_bm_total_line = away_bookmaker_line,
                close_bm_total_odds = away_bookmaker_odds,
                close_pncl_total_line = away_pinnacle_line,
                close_pncl_total_odds = away_pinnacle_odds))


# spreads -----------------------------------------------------------------

spr <- list.files('./data/nba_lines', pattern = 'spread')

spr_list <- list()
for (sp in spr) {
  spr_raw <- read.csv(paste0('./data/nba_lines/', sp))
  spr_list[[length(spr_list) + 1]] <- spr_raw
}

spr_df <- bind_rows(spr_list)
spr_df_long <- spr_df %>%
  transmute(game_date = lubridate::as_date(as.character(Date)),
            team = home_Team,
            home_away = 'home',
            open_bm_spread_line = home_open_line,
            open_bm_spread_odds = home_open_odds,
            close_bm_spread_line = home_bookmaker_line,
            close_bm_spread_odds = home_bookmaker_odds,
            close_pncl_spread_line = home_pinnacle_line,
            close_pncl_spread_odds = home_pinnacle_odds) %>%
  bind_rows(
    spr_df %>%
    transmute(game_date = lubridate::as_date(as.character(Date)),
              team = away_Team,
              home_away = 'away',
              open_bm_spread_line = away_open_line,
              open_bm_spread_odds = away_open_odds,
              close_bm_spread_line = away_bookmaker_line,
              close_bm_spread_odds = away_bookmaker_odds,
              close_pncl_spread_line = away_pinnacle_line,
              close_pncl_spread_odds = away_pinnacle_odds))


# moneylines --------------------------------------------------------------

ml <- list.files('./data/nba_lines', pattern = 'moneyline')

ml_list <- list()
for (sp in ml) {
  ml_raw <- read.csv(paste0('./data/nba_lines/', sp))
  ml_list[[length(ml_list) + 1]] <- ml_raw
}

ml_df <- bind_rows(ml_list)
ml_df_long <- ml_df %>%
  transmute(game_date = lubridate::as_date(as.character(Date)),
            team = home_Team,
            home_away = 'home',
            open_bm_moneyline_line = home_open_line,
            close_bm_moneyline_line = home_bookmaker_line,
            close_pncl_moneyline_line = home_pinnacle_line) %>%
  bind_rows(
    ml_df %>%
      transmute(game_date = lubridate::as_date(as.character(Date)),
                team = away_Team,
                home_away = 'away',
                open_bm_moneyline_line = away_open_line,
                close_bm_moneyline_line = away_bookmaker_line,
                close_pncl_moneyline_line = away_pinnacle_line))


# engineer and output -----------------------------------------------------
lu <- read.csv('./data/lu/nba_team_lu.csv')

output <- ml_df_long %>%
  left_join(spr_df_long) %>%
  left_join(tot_df_long) %>%
  left_join(lu, by = c('team' = 'SBR_team')) %>%
  mutate(open_bm_implied_pts = (open_bm_total_line - open_bm_spread_line) / 2,
         close_bm_implied_pts = (close_bm_total_line - close_bm_spread_line) / 2,
         close_pncl_implied_pts = (close_pncl_total_line - close_pncl_spread_line) / 2,
         open_bm_implied_pts_allowed = open_bm_total_line - open_bm_implied_pts,
         close_bm_implied_pts_allowed = close_bm_total_line - close_bm_implied_pts,
         close_pncl_implied_pts_allowed = close_pncl_total_line - close_pncl_implied_pts) %>%
  na.omit()

write.csv(output, './data/nba_lines/tidy_lines.csv')
