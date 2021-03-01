library(tidyverse)

# gamelogs_16_17 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2016-17.csv', 
#                            colClasses = 'character')
# gamelogs_17_18 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2017-18.csv', 
#                            colClasses = 'character')
gamelogs_18_19 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2018-19.csv', 
                           colClasses = 'character')
gamelogs_19_20 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2019-20.csv', 
                           colClasses = 'character')
gamelogs_20_21 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                           colClasses = 'character')

gamelogs <-
  rbind.data.frame(#gamelogs_16_17, 
                   #gamelogs_17_18, 
                   gamelogs_18_19, 
                   gamelogs_19_20, 
                   gamelogs_20_21)

unique_games <- unique(gamelogs$GAME_ID)

first_possession_list <- list()

for (g in unique_games) {
  
  gamelog <- 
    gamelogs %>%
    filter(GAME_ID == g)
  
  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])
  
  csv_path <- paste0('./data/nba_pbp/', g, '.csv')
  
  pbp <- 
    read.csv(csv_path, 
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter(row_number() == 2)
  
  home_team_abbrev <- as.character(pbp$PLAYER1_TEAM_ABBREVIATION)
  home_team_jumper <- as.character(pbp$PLAYER1_NAME)
  away_team_abbrev <- as.character(pbp$PLAYER2_TEAM_ABBREVIATION)
  away_team_jumper <- as.character(pbp$PLAYER2_NAME)
  first_possession <- as.character(pbp$PLAYER3_TEAM_ABBREVIATION)
  
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    home_team_jumper = home_team_jumper,
    home_team_abbrev = home_team_abbrev,
    away_team_jumper = away_team_jumper,
    away_team_abbrev = away_team_abbrev,
    first_possession = first_possession
  )

  first_possession_list[[g]] <- output
  
}

first_possession_df <- 
  bind_rows(first_possession_list) %>%
  filter(!is.na(first_possession))

home_count_by_jumper <-
  first_possession_df %>%
  group_by(jumper = home_team_jumper) %>%
  count()

away_count_by_jumper <-
  first_possession_df %>%
  group_by(jumper = away_team_jumper) %>%
  count()

count_by_jumper <-
  rbind.data.frame(home_count_by_jumper, away_count_by_jumper) %>%
  group_by(jumper) %>%
  summarise(jumps = sum(`n`)) %>%
  filter(jumps >= 10)

first_possession_df_filtered <-
  first_possession_df %>%
  filter(home_team_jumper %in% count_by_jumper$jumper,
         away_team_jumper %in% count_by_jumper$jumper)

# ## BT
# all_jumpers_home <-
#   first_possession_df %>%
#   distinct(jumper = home_team_jumper)
# 
# all_jumpers_away <-
#   first_possession_df %>%
#   distinct(jumper = away_team_jumper)
# 
# all_jumpers <-
#   rbind.data.frame(all_jumpers_home, all_jumpers_away) %>%
#   distinct()
# 
# jumpers_full_join <-
#   all_jumpers %>% 
#   rename(home_team_jumper = jumper) %>%
#   crossing(select(all_jumpers, away_team_jumper = jumper)) %>%
#   filter(home_team_jumper != away_team_jumper)
# 
# wins_df <-
#   first_possession_df %>%
#   group_by(home_team_jumper, away_team_jumper) %>%
#   summarise(home_wins = sum(home_win), 
#             away_wins = n() - sum(home_win))
# 
# df_for_bt <-
#   jumpers_full_join %>%
#   left_join(wins_df) %>%
#   mutate(home_team_jumper = as.factor(home_team_jumper),
#          away_team_jumper = as.factor(away_team_jumper))
# 
# df_for_bt[is.na(df_for_bt)] <- 0
# 
# bradleyterry_model <- 
#   BTm(cbind(home_wins, away_wins), home_team_jumper, away_team_jumper, data = df_for_bt)
# 
# ratings <-
#   bradleyterry_model$coefficients

jump_balls_home <-
  first_possession_df_filtered %>%
  select(jumper = home_team_jumper, home_team_abbrev, first_possession) %>%
  mutate(won_tip = if_else(home_team_abbrev == first_possession, 1, 0)) %>%
  group_by(jumper) %>%
  summarise(jumps = n(), 
            wins = sum(won_tip), 
            .groups = 'drop')

jump_balls_away <-
  first_possession_df_filtered %>%
  select(jumper = away_team_jumper, away_team_abbrev, first_possession) %>%
  mutate(won_tip = if_else(away_team_abbrev == first_possession, 1, 0)) %>%
  group_by(jumper) %>%
  summarise(jumps = n(), 
            wins = sum(won_tip),
            .groups = 'drop')

jump_balls_binded <-
  rbind.data.frame(jump_balls_home, jump_balls_away) %>%
  group_by(jumper) %>%
  summarise(jumps = sum(jumps), 
            wins = sum(wins), 
            losses = jumps - wins,
            win_rate = wins/jumps,
            .groups = 'drop')

jump_balls_df <-
  jump_balls_binded %>%
  select(jumper, win_rate)

jump_balls_rating <-
  jump_balls_binded %>%
  mutate(rating = (wins + 0.5) / (losses + 0.5)) %>%
  select(jumper, rating)

rating_loop_df_prep <-
  first_possession_df %>%
  left_join(jump_balls_rating, by = c("home_team_jumper" = "jumper")) %>%
  rename(home_rating = rating) %>%
  left_join(jump_balls_rating, by = c("away_team_jumper" = "jumper")) %>%
  rename(away_rating = rating) %>%
  mutate(home_won_tip = if_else(home_team_abbrev == first_possession, 1, 0)) %>%
  filter(!is.na(away_rating), !is.na(home_rating))

rating_loop_df <- rating_loop_df_prep

### Loop repeats until there is no change in ratings
j <- 0
change <- 1
last_iter <- 100
while(change != 0){
  if (dim(rating_loop_df)[1] == 0) {
    break
  }
  j <- j + 1
  print(j)
  
  ### Iterative Adjustments ###
  rating_loop_df <-
    rating_loop_df %>%
    mutate(home_odds = home_rating / (home_rating + away_rating),
           away_odds = away_rating / (home_rating + away_rating))
  
  ratings_jumper_home <-
    rating_loop_df %>%
    select(jumper = home_team_jumper, rating = home_rating, 
           opp_rating = away_rating, odds = home_odds, won_tip = home_won_tip)
  
  ratings_jumper_away <-
    rating_loop_df %>%
    select(jumper = away_team_jumper, rating = away_rating,
           opp_rating = home_rating, odds = away_odds, won_tip = home_won_tip) %>%
    mutate(won_tip = 1 - won_tip)
  
  ratings_jumper <-
    rbind.data.frame(ratings_jumper_home, ratings_jumper_away) %>%
    mutate(weight_factor = 1/(rating + opp_rating),
           weighted_sum = opp_rating * (weight_factor))
  
  ratings_new <-
    ratings_jumper %>%
    group_by(jumper, rating) %>%
    summarise(jumps = n(),
              wins = sum(won_tip),
              exp_wins = sum(odds),
              sos_mean = mean(opp_rating),
              sos = sum(weighted_sum)/sum(weight_factor)) %>%
    mutate(expected_win_ratio = (exp_wins + 0.5)/(jumps - exp_wins + 0.5),
           sosold = rating / expected_win_ratio,
           rating_adj = rating * sos)
              
  rating_loop_df <-
    rating_loop_df %>%
    left_join(select(ratings_new, jumper, rating_adj), by = c("home_team_jumper" = "jumper")) %>%
    rename(home_rating_adj = rating_adj) %>%
    left_join(select(ratings_new, jumper, rating_adj), by = c("away_team_jumper" = "jumper")) %>%
    rename(away_rating_adj = rating_adj)
  
  new_iter <- mean(abs(rating_loop_df$home_rating - rating_loop_df$home_rating_adj))
  change <- abs(last_iter - new_iter)
  last_iter <- new_iter
  print(change)

  ### Updating data frame with new totals after iteration ###
  rating_loop_df <- 
    rating_loop_df %>% 
    mutate(home_rating = home_rating_adj, 
           away_rating = away_rating_adj) %>%
    select(-c(home_rating_adj, away_rating_adj, home_odds, away_odds))
}





expected_win_prob <-
  rating_loop_df %>%
  mutate(home_percentage = case_when(
                             home_percentage > 1 ~ 1,
                             home_percentage < 0 ~ 0,
                             TRUE ~ home_percentage),
         away_percentage = case_when(
                             away_percentage > 1 ~ 1,
                             away_percentage < 0 ~ 0,
                             TRUE ~ away_percentage)) %>%
  mutate(home_exp_win = (home_percentage*(1-away_percentage)) / ((home_percentage*(1-away_percentage)) + (away_percentage*(1-home_percentage))))

train_buckets <-
  expected_win_prob %>%
  mutate(bin = cut(home_exp_win, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(bin) %>%
  summarise(Jumps = n(),
            Win_Percentage = mean(home_win))


#### Test data 
player_list_home <-
  expected_win_prob %>%
  distinct(jumper = home_team_jumper, exp_win = home_percentage)

player_list_away <-
  expected_win_prob %>%
  distinct(jumper = away_team_jumper, exp_win = away_percentage)

player_list_df <-
  rbind.data.frame(player_list_home, player_list_away) %>%
  distinct()

test_df <-
  first_possession_df %>%
  left_join(player_list_df, by = c("home_team_jumper" = "jumper")) %>%
  rename(home_exp_win = exp_win) %>%
  left_join(player_list_df, by = c("away_team_jumper" = "jumper")) %>%
  rename(away_exp_win = exp_win) %>%
  mutate(home_win = if_else(home_team_abbrev == first_possession, 1, 0))

# Filtering out players without jumps prior to this year
test_df_filtered <-
  test_df %>%
  filter(!is.na(home_exp_win), !is.na(away_exp_win))

test_df_exp_win <-
  test_df_filtered %>%
  mutate(exp_win = (home_exp_win*(1-away_exp_win)) / ((home_exp_win*(1-away_exp_win)) + (away_exp_win*(1-home_exp_win))))
  
test_buckets <-
  test_df_exp_win %>%
  mutate(bin = cut(exp_win, breaks = seq(0, 100, by = 0.05), right = FALSE)) %>%
  group_by(bin) %>%
  summarise(Jumps = n(),
            Win_Percentage = mean(home_win)) 


player_1 <- "Kevon Looney"
player_2 <- "Nerlens Noel"

player_1_data <-
  player_list_df %>%
  filter(jumper == player_1)

player_2_data <-
  player_list_df %>%
  filter(jumper == player_2)

player_1_win <- 
  round((player_1_data$exp_win*(1-player_2_data$exp_win)) / 
  ((player_1_data$exp_win*(1-player_2_data$exp_win)) + (player_2_data$exp_win*(1-player_1_data$exp_win))), 2)

if (player_1_win >= .5) {
  print(paste0(player_1, " wins a tip vs ", player_2, " ", player_1_win, " percent of the time"))
}else{
  print(paste0(player_2, " wins a tip vs ", player_1, " ", 1 - player_1_win, " percent of the time"))
}

