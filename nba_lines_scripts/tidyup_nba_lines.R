library(tidyverse)


# totals ------------------------------------------------------------------

tot <- list.files('./data/nba_lines', pattern = 'total')

tot_list <- list()
for (sp in tot) {
  tot_raw <- read.csv(paste0('./data/nba_lines/', sp))
  tot_list[[length(tot_list) + 1]] <- tot_raw
}

tot_df <- bind_rows(tot_list)

# Replacing LA with L.A. Clippers if dates between start of 2019 season and mid Jan 2021
tot_df_LA_update <-
  tot_df %>%
  mutate(game_date = lubridate::as_date(as.character(Date)),
         home_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & home_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ home_Team),
         away_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & away_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ away_Team))

tot_df_long <- tot_df_LA_update %>%
  transmute(game_date,
            team = home_Team,
            home_away = 'home',
            open_bookmaker_total_line = home_open_line,
            # open_bookmaker_total_odds = home_open_odds,
            close_bookmaker_total_line = home_bookmaker_line,
            # close_bookmaker_total_odds = home_bookmaker_odds,
            close_pinnacle_total_line = home_pinnacle_line
            # close_pinnacle_total_odds = home_pinnacle_odds
            ) %>%
  bind_rows(
    tot_df_LA_update %>%
      transmute(game_date,
                team = away_Team,
                home_away = 'away',
                open_bookmaker_total_line = away_open_line,
                # open_bookmaker_total_odds = away_open_odds,
                close_bookmaker_total_line = away_bookmaker_line,
                # close_bookmaker_total_odds = away_bookmaker_odds,
                close_pinnacle_total_line = away_pinnacle_line
                # close_pinnacle_total_odds = away_pinnacle_odds
                ))


# spreads -----------------------------------------------------------------

spr <- list.files('./data/nba_lines', pattern = 'spread')

spr_list <- list()
for (sp in spr) {
  spr_raw <- read.csv(paste0('./data/nba_lines/', sp))
  spr_list[[length(spr_list) + 1]] <- spr_raw
}

spr_df <- bind_rows(spr_list)

# Replacing LA with L.A. Clippers if dates between start of 2019 season and mid Jan 2021
spr_df_LA_update <-
  spr_df %>%
  mutate(game_date = lubridate::as_date(as.character(Date)),
         home_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & home_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ home_Team),
         away_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & away_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ away_Team))

spr_df_long <- spr_df_LA_update %>%
  transmute(game_date,
            team = home_Team,
            home_away = 'home',
            open_bookmaker_spread_line = home_open_line,
            # open_bookmaker_spread_odds = home_open_odds,
            close_bookmaker_spread_line = home_bookmaker_line,
            # close_bookmaker_spread_odds = home_bookmaker_odds,
            close_pinnacle_spread_line = home_pinnacle_line
            # close_pinnacle_spread_odds = home_pinnacle_odds
            ) %>%
  bind_rows(
    spr_df_LA_update %>%
    transmute(game_date,
              team = away_Team,
              home_away = 'away',
              open_bookmaker_spread_line = away_open_line,
              # open_bookmaker_spread_odds = away_open_odds,
              close_bookmaker_spread_line = away_bookmaker_line,
              # close_bookmaker_spread_odds = away_bookmaker_odds,
              close_pinnacle_spread_line = away_pinnacle_line
              # close_pinnacle_spread_odds = away_pinnacle_odds
              ))


# moneylines --------------------------------------------------------------

ml <- list.files('./data/nba_lines', pattern = 'moneyline')

ml_list <- list()
for (sp in ml) {
  ml_raw <- read.csv(paste0('./data/nba_lines/', sp))
  ml_list[[length(ml_list) + 1]] <- ml_raw
}

ml_df <- bind_rows(ml_list)

# Replacing LA with L.A. Clippers if dates between start of 2019 season and mid Jan 2021
ml_df_LA_update <-
  ml_df %>%
  mutate(game_date = lubridate::as_date(as.character(Date)),
         home_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & home_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ home_Team),
         away_Team = case_when(
           game_date >= "2019-09-01" & game_date <= "2021-01-15" & away_Team == "LA" ~ "L.A. Clippers",
           TRUE ~ away_Team))

ml_df_long <- ml_df_LA_update %>%
  transmute(game_date,
            team = home_Team,
            home_away = 'home',
            open_bookmaker_moneyline_line = home_open_line,
            close_bookmaker_moneyline_line = home_bookmaker_line,
            close_pinnacle_moneyline_line = home_pinnacle_line) %>%
  bind_rows(
    ml_df_LA_update %>%
      transmute(game_date,
                team = away_Team,
                home_away = 'away',
                open_bookmaker_moneyline_line = away_open_line,
                close_bookmaker_moneyline_line = away_bookmaker_line,
                close_pinnacle_moneyline_line = away_pinnacle_line))


# engineer and output -----------------------------------------------------
lu <- read.csv('./data/lu/nba_team_lu.csv')

output <- ml_df_long %>%
  left_join(spr_df_long) %>%
  left_join(tot_df_long) %>%
  left_join(lu, by = c('team' = 'SBR_team')) %>%
  mutate(open_bookmaker_implied_pts = (open_bookmaker_total_line - open_bookmaker_spread_line) / 2,
         close_bookmaker_implied_pts = (close_bookmaker_total_line - close_bookmaker_spread_line) / 2,
         close_pinnacle_implied_pts = (close_pinnacle_total_line - close_pinnacle_spread_line) / 2,
         open_bookmaker_implied_pts_allowed = open_bookmaker_total_line - open_bookmaker_implied_pts,
         close_bookmaker_implied_pts_allowed = close_bookmaker_total_line - close_bookmaker_implied_pts,
         close_pinnacle_implied_pts_allowed = close_pinnacle_total_line - close_pinnacle_implied_pts) %>%
  mutate(bookmaker_spread_line_delta = close_bookmaker_spread_line - open_bookmaker_spread_line,
         bookmaker_spread_line_delta_perc = bookmaker_spread_line_delta / open_bookmaker_spread_line,
         bookmaker_moneyline_delta = open_bookmaker_moneyline_line - close_bookmaker_moneyline_line,
         bookmaker_moneyline_delta_perc = bookmaker_moneyline_delta / open_bookmaker_moneyline_line,
         bookmaker_total_line_delta = close_bookmaker_total_line - open_bookmaker_total_line,
         bookmaker_total_line_delta_perc = bookmaker_total_line_delta / open_bookmaker_total_line) %>%
  # get rid of the pinnacle stuff for now
  select(game_date, team_id,
         matches('bookmaker')) %>%
  na.omit()

write_csv(output, './data/nba_lines/all_historical_lines.csv')
