library(tidyverse)

csv_dir <- '/Users/jim/Desktop/nba_lines/'

moneylines <- bind_rows(
  lapply(
    list.files(csv_dir, pattern = 'moneyline.csv'), 
    function(x) read.csv(paste0(csv_dir, x))
    )
  )

totals <- bind_rows(
  lapply(
    list.files(csv_dir, pattern = 'total.csv'), 
    function(x) read.csv(paste0(csv_dir, x))
    )
  )

spreads <- bind_rows(
  lapply(
    list.files(csv_dir, pattern = 'spread.csv'), 
    function(x) read.csv(paste0(csv_dir, x))
    )
  )

merged <- totals %>%
  left_join(spreads, by = c("Date", "Sport", "bet_type", "period", "away_Team", "home_Team"), suffix = c("_totals", "_spreads"))


%>%
  left_join(moneylines, by = c("Date", "Sport", "bet_type", "period", "away_Team", "home_Team"), suffix = c("", "_moneylines"))

