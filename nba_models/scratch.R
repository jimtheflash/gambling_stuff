# lets build some simple models to predict fanduel fantasy points,
# using a tidy framework
library(tidyverse)

# first we import the data
# (easier to do as character with base read.csv)
gamelogs <- read.csv('./data/nba_gamelogs/nba_gamelogs_2018-19.csv', 
                     colClasses = 'character')

# next we SHOULD tidy up the data

tidy_gamelogs <- gamelogs %>%
  # tidy the names
  janitor::clean_names() %>%
  # fix all those character strings (fortunately they're nicely ordered)
  mutate(across(min:dk, as.numeric)) %>%
  # change dates to dates (the timestamp is not really real)
  mutate(across(matches('date'), as.Date)) %>%
  # i have no idea what the ranks are calc'ed on, so 86 em
  select(-ends_with('rank'))

# take a peek
skimr::skim(tidy_gamelogs) %>% View()
# no NAs, distributed data, sensible ranges, yadda yadda yadda

# engineer features, first via aggregation
eng_gamelogs <- tidy_gamelogs %>%
  arrange(season_year, player_id, game_date) %>%
  group_by(season_year, player_id)
