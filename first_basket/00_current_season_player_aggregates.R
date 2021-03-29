library(tidyverse)
library(lubridate)

# Relevant gamelogs
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2020-21.csv', 
                     colClasses = 'character')

gamelogs <-
  gamelogs %>%
  mutate(across(MIN:PLUS_MINUS, as.numeric))
         
player_season_aggregates <-
  gamelogs %>%
  group_by(PLAYER_ID, PLAYER_NAME, SEASON_YEAR, TEAM_ABBREVIATION, SEASON_TYPE) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(FG_PCT = FGM/FGA,
         FG3_PCT = FG3M/FG3A,
         FT_PCT = FTM/FTA)

team_season_aggregates <-
  gamelogs %>%
  group_by(SEASON_YEAR, TEAM_ABBREVIATION, SEASON_TYPE) %>%
  summarise(across(where(is.numeric), sum, .names = "TM_{.col}"),
            .groups = 'drop') %>%
  mutate(TM_FG_PCT = TM_FGM/TM_FGA,
         TM_FG3_PCT = TM_FG3M/TM_FG3A,
         TM_FT_PCT = TM_FTM/TM_FTA)

player_team_joined <-
  player_season_aggregates %>%
  left_join(team_season_aggregates, by = c("SEASON_YEAR", "TEAM_ABBREVIATION", "SEASON_TYPE")) %>%
  mutate(USG = ((FGA + 0.44*FTA + TOV) * (TM_MIN / 5)) /
               (MIN * (TM_FGA + 0.44*TM_FTA + TM_TOV)),
         FG_USG = (FGA * TM_MIN / 5) / (MIN * TM_FGA))

player_usage <-
  player_team_joined %>%
  filter(SEASON_YEAR == "2020-21", SEASON_TYPE == "Regular Season") %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, SEASON_TYPE, USG, FG_USG, FG_PCT)

## Write out main file
write.csv(player_usage, "data/02_curated/nba_first_to_score/current_season_usage_rate.csv.gz", row.names = FALSE)

## Write out archive file
yyyy <- as.character(year(Sys.Date()))
mm <- str_pad(as.character(month(Sys.Date())), 2, "left", pad = 0)
dd <- str_pad(as.character(day(Sys.Date())), 2, "left", pad = 0)

if (dir.exists(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)))) {
  message("directory already exists")
}else{
  dir.create(file.path(paste0("./data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd)), recursive = TRUE)
}

write.csv(player_usage, paste0("data/02_curated/nba_first_to_score/", yyyy, "/", mm, "/", dd, "/", "current_season_usage_rate.csv.gz"), row.names = FALSE)


