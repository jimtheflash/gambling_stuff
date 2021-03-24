library(readxl)
library(dplyr)
library(ggplot2)

##### Prop Bet Tracker ######
bets_df <- read_excel("./data/curated/nba/Prop Bets.xlsx", sheet = "Sheet1")

bets_props <- 
  bets_df %>% 
  mutate(type = if_else(`Bet 2` == "First Team To Score", "Team", "Player"),
         decimal_odds = if_else(Odds > 0, (Odds/100) + 1, (100/-Odds) + 1),
         wager = 1,
         net = if_else(Outcome == "Win", decimal_odds - wager, -wager))

bets_summary <-
  bets_props %>%
  group_by(type) %>%
  summarise(bets = n(),
            wins = sum(Result == "Win"),
            losses = bets - wins,
            win_rate = wins/bets,
            net = sum(net))

bets_team <-
  bets_props %>%
  filter(type == "Team") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))
         
bets_player <-
  bets_props %>%
  filter(type == "Player") %>%
  mutate(bet_number = row_number(),
         running_sum = cumsum(net))

## Team Rolling Sum
team_plot <-
  ggplot(data = bets_team, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Team To Score") +
  xlab("Bet Number") +
  ylab("Units")

## Player Rolling Sum
player_plot <-
  ggplot(data = bets_player, aes(x= bet_number, y = running_sum)) +
  geom_line() +
  ggtitle("First Player To Score") +
  xlab("Bet Number") +
  ylab("Units")

team_plot
player_plot

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




