library(tidyverse)

# Relevant gamelogs
gamelogs_16_17 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2016-17.csv', 
                           colClasses = 'character')
gamelogs_17_18 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2017-18.csv', 
                           colClasses = 'character')
gamelogs_18_19 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2018-19.csv', 
                           colClasses = 'character')
gamelogs_19_20 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2019-20.csv', 
                           colClasses = 'character')
gamelogs_20_21 <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                           colClasses = 'character')

gamelogs <-
  rbind.data.frame(gamelogs_16_17, 
                   gamelogs_17_18, 
                   gamelogs_18_19, 
                   gamelogs_19_20, 
                   gamelogs_20_21)

gamelogs <-
  gamelogs %>%
  mutate(across(MIN:PLUS_MINUS, as.numeric))
         
player_season_aggregates <-
  gamelogs %>%
  group_by(PLAYER_ID, PLAYER_NAME, SEASON_YEAR, TEAM_ABBREVIATION, SEASON_TYPE) %>%
  summarise(MIN = sum(MIN),
            FGM = sum(FGM),
            FGA = sum(FGA),
            FG_PCT = FGM/FGA,
            FG3M = sum(FG3M),
            FG3A = sum(FG3A),
            FG3_PCT = FG3M/FG3A,
            FTM = sum(FTM),
            FTA = sum(FTA),
            FT_PCT = FTM/FTA,
            OREB = sum(OREB),
            DREB = sum(DREB),
            REB = sum(REB),
            AST = sum(AST),
            TOV = sum(TOV),
            STL = sum(STL),
            BLK = sum(BLK),
            BLKA = sum(BLKA),
            PF = sum(PF),
            PFD = sum(PFD),
            PTS = sum(PTS),
            PLUS_MINUS = sum(PLUS_MINUS))

team_season_aggregates <-
  gamelogs %>%
  group_by(SEASON_YEAR, TEAM_ABBREVIATION, SEASON_TYPE) %>%
  summarise(TM_MIN = sum(MIN),
            TM_FGM = sum(FGM),
            TM_FGA = sum(FGA),
            TM_FG_PCT = TM_FGM/TM_FGA,
            TM_FG3M = sum(FG3M),
            TM_FG3A = sum(FG3A),
            TM_FG3_PCT = TM_FG3M/TM_FG3A,
            TM_FTM = sum(FTM),
            TM_FTA = sum(FTA),
            TM_FT_PCT = TM_FTM/TM_FTA,
            TM_OREB = sum(OREB),
            TM_DREB = sum(DREB),
            TM_REB = sum(REB),
            TM_AST = sum(AST),
            TM_TOV = sum(TOV),
            TM_STL = sum(STL),
            TM_BLK = sum(BLK),
            TM_BLKA = sum(BLKA),
            TM_PF = sum(PF),
            TM_PFD = sum(PFD),
            TM_PTS = sum(PTS),
            TM_PLUS_MINUS = sum(PLUS_MINUS))

player_team_joined <-
  player_season_aggregates %>%
  left_join(team_season_aggregates) %>%
  mutate(USG = ((FGA + 0.44*FTA + TOV) * (TM_MIN / 5)) /
               (MIN * (TM_FGA + 0.44*TM_FTA + TM_TOV)),
         FG_USG = (FGA * TM_MIN / 5) / (MIN * TM_FGA))

player_usage <-
  player_team_joined %>%
  filter(SEASON_YEAR == "2020-21", SEASON_TYPE == "Regular Season") %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, SEASON_TYPE, USG, FG_USG, FG_PCT)

write.csv(player_usage, "data/curated/nba/current_season_usage_rate.csv.gz", row.names = FALSE)



