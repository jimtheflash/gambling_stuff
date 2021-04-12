library(readxl)
library(dplyr)
library(ggplot2)

########### Gamelogs ###############
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                     colClasses = 'character')

unique_games <- unique(gamelogs$GAME_ID)

# Initializing list
possession_list <- list()

for (g in unique_games) {
  
  # Unique game in loop
  gamelog <- 
    gamelogs %>%
    filter(GAME_ID == g)
  
  # Relevant info of game
  season <- as.character(gamelog$SEASON_YEAR[[1]])
  matchup <- as.character(gamelog$MATCHUP[[1]])
  gamedate <- as.Date(gamelog$GAME_DATE[[1]])
  
  csv_path <- paste0('./data/nba_pbp/', g, '.csv')
  
  # Finding all jump balls in game, noting if it is opening tip
  pbp <- 
    read.csv(csv_path, 
             colClasses = 'character',
             na.strings = c('')) %>%
    mutate(EVENTNUM = as.numeric(EVENTNUM)) %>%
    filter((grepl("jump ball", tolower(HOMEDESCRIPTION)) & PERIOD == "1" & PCTIMESTRING == "12:00") | 
             (SCORE != '' & !is.na(SCORE) & !is.na(EVENTNUM)))
  
  win_tip <- 
    pbp %>%
    filter(row_number() == 1) %>%
    mutate(team_win_tip = paste0(PLAYER3_TEAM_CITY, " ", PLAYER3_TEAM_NICKNAME))
  
  score_first <-
    pbp %>%
    filter(row_number() == 2) %>%
    mutate(team_score_first = paste0(PLAYER1_TEAM_CITY, " ", PLAYER1_TEAM_NICKNAME))
  
  home <- as.character(paste0(win_tip$PLAYER1_TEAM_CITY, " ", win_tip$PLAYER1_TEAM_NICKNAME))
  home_jumper <- as.character(win_tip$PLAYER1_NAME)
  home_person_id <- as.character(win_tip$PLAYER1_ID)
  away <- as.character(paste0(win_tip$PLAYER2_TEAM_CITY, " ", win_tip$PLAYER2_TEAM_NICKNAME))
  away_jumper <- as.character(win_tip$PLAYER2_NAME)
  away_person_id <- as.character(win_tip$PLAYER2_ID)
  possession <- as.character(win_tip$team_win_tip)
  score_first <- as.character(score_first$team_score_first)
  
  # Final output of relevant info about jumps
  output <- tibble(
    season = season,
    game_date = gamedate,
    game_id = g,
    matchup = matchup,
    home_jumper = home_jumper,
    home_person_id = home_person_id,
    home = home,
    away_jumper = away_jumper,
    away_person_id = away_person_id,
    away = away,
    possession = possession,
    score_first = score_first
  )
  
  possession_list[[g]] <- output
  
}

# Removing cases where first possession could not be determined from pbp
# Adding indicator if home team won tip
possession_binded <- 
  bind_rows(possession_list) %>%
  mutate(home = if_else(home == "LA Clippers", "Los Angeles Clippers", home),
         away = if_else(away == "LA Clippers", "Los Angeles Clippers", away),
         possession = if_else(possession == "LA Clippers", "Los Angeles Clippers", possession),
         score_first = if_else(score_first == "LA Clippers", "Los Angeles Clippers", score_first)) %>%
  rename(Home = home, Away = away, Date = game_date)



######### Prop Bet Tracker ############
bets_df <- read_excel("./scratch/Prop Bets.xlsx", sheet = "Sheet1")

bets_props <- 
  bets_df %>% 
  mutate(type = if_else(`Bet 2` == "First Team To Score", "Team", "Player"),
         decimal_odds = if_else(Odds > 0, (Odds/100) + 1, (100/-Odds) + 1),
         wager = 1,
         net = if_else(Outcome == "Win", decimal_odds - wager, -wager)) %>%
  filter(!is.na(Result)) %>%
  mutate(model = if_else(Date <= "2021-03-17", "Old", "New"))

bets_summary_all <-
  bets_props %>%
  group_by(type) %>%
  summarise(bets = n(),
            wins = sum(Outcome == "Win"),
            losses = bets - wins,
            win_rate = wins/bets,
            net = sum(net))

bets_team_all <-
  bets_props %>%
  filter(type == "Team") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))
         
bets_player_all <-
  bets_props %>%
  filter(type == "Player") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

## Team Rolling Sum
team_plot_all <-
  ggplot(data = bets_team_all, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Team To Score - All") +
  xlab("Bet Number") +
  ylab("Units")

## Player Rolling Sum
player_plot_all <-
  ggplot(data = bets_player_all, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Player To Score - All") +
  xlab("Bet Number") +
  ylab("Units")

team_plot_all
player_plot_all

## Prop Bets New Model
bets_summary_new_model <-
  bets_props %>%
  filter(model == "New") %>%
  group_by(type) %>%
  summarise(bets = n(),
            wins = sum(Outcome == "Win"),
            losses = bets - wins,
            win_rate = wins/bets,
            net = sum(net))

bets_team_new_model <-
  bets_props %>%
  filter(model == "New") %>%
  filter(type == "Team") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

bets_player_new_model<-
  bets_props %>%
  filter(model == "New") %>%
  filter(type == "Player") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

## Team Rolling Sum
team_plot_new_model <-
  ggplot(data = bets_team_new_model, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Team To Score - New Model") +
  xlab("Bet Number") +
  ylab("Units")

## Player Rolling Sum
player_plot_new_model <-
  ggplot(data = bets_player_new_model, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Player To Score - New Model") +
  xlab("Bet Number") +
  ylab("Units")

team_plot_new_model
player_plot_new_model

bets_summary_new_model_team <-
  bets_team_new_model %>%
  group_by(`Bet 3`) %>%
  summarise(bets = n(),
            wins = sum(Outcome == "Win"),
            losses = bets - wins,
            win_rate = wins/bets,
            net = sum(net))

outcomes_new_model_team <-
  bets_team_new_model %>%
  left_join(possession_binded) %>%
  mutate(pick_win_tip = if_else(Pick == possession, TRUE, FALSE),
         pick_score_first = if_else(Pick == score_first, TRUE, FALSE),
         won_tip_and_score_first = if_else(possession == score_first, TRUE, FALSE)) %>%
  group_by(pick_win_tip, pick_score_first) %>%
  count()

outcomes_new_model_team_by_favorite <-
  bets_team_new_model %>%
  left_join(possession_binded) %>%
  mutate(pick_win_tip = if_else(Pick == possession, TRUE, FALSE),
         pick_score_first = if_else(Pick == score_first, TRUE, FALSE),
         won_tip_and_score_first = if_else(possession == score_first, TRUE, FALSE)) %>%
  group_by(`Bet 3`, pick_win_tip, pick_score_first) %>%
  count()

## Team Rolling Sum By Favorites/Underdogs
bets_favorites <-
  bets_props %>%
  filter(type == "Team", `Bet 3` == "Favorite") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

bets_underdogs <-
  bets_props %>%
  filter(type == "Team", `Bet 3` == "Underdog") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

favorites_plot <-
  ggplot(data = bets_favorites, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Team To Score - Favorites") +
  xlab("Bet Number") +
  ylab("Units")

underdogs_plot <-
  ggplot(data = bets_underdogs, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Team To Score - Underdogs") +
  xlab("Bet Number") +
  ylab("Units")

favorites_plot
underdogs_plot


## From before ##
test_df <-
  bets_team %>%
  left_join(possession_binded) %>%
  mutate(pick_win_tip = if_else(Pick == possession, TRUE, FALSE),
         pick_score_first = if_else(Pick == score_first, TRUE, FALSE),
         won_tip_and_score_first = if_else(possession == score_first, TRUE, FALSE))

test_buckets <-
  test_df %>%
  group_by(pick_win_tip, pick_score_first) %>%
  count()

mean(test_df$pick_win_tip, na.rm = T)
mean(test_df$pick_score_first, na.rm = T)
mean(test_df$won_tip_and_score_first, na.rm = T)

##### PLAYER PROP ANALYSIS #####
player_return <-
  bets_player_all %>%
  group_by(Pick) %>%
  summarise(bets = n(),
            wins = sum(Outcome == "Win"),
            losses = bets - wins,
            win_rate = wins/bets,
            net = sum(net))


