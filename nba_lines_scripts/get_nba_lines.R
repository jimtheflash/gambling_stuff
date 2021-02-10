# devtools::install_github("a-i-sports/bettoR")
library(bettoR)
output_dir <- '/Users/jim/Desktop/nba_lines/'
# start_date <- as.Date('2015-01-01')
start_date <- as.Date('2018-06-01')
end_date <- Sys.Date()
date_range <- gsub('-', '', as.character(seq.Date(start_date, end_date, by = "day")))
bet_types <- c("spread", "total", "moneyline")
message(Sys.time(), " initializing loop")
for (d in date_range) {
  message(Sys.time(), " trying to grab NBA lines for date ", d)
  for (b in bet_types) {
    message("getting ", b)
    Sys.sleep(3)
    line_info <- try(
      get_lines(
        sport = "NBA",
        bet_type = b,
        start_date = d
      )
    )
    if ('try-error' %in% class(line_info)) {
      message(Sys.time(), " something went amiss trying to scrape ", b, " for date ", d)
      next
    } else {
      output_string <- paste0(output_dir, 'nba_', d, '_', b, '.csv')
      write.csv(line_info, output_string, row.names = FALSE, quote = FALSE)
    }
  }
}
