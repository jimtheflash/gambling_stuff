library(bettoR)

# set the output directory
output_dir <- './data/nba_lines/'

# specify bet types to scrape
bet_types <- c("spread", "total", "moneyline")

# 2 ways to figure out which dates to scrape; either parse the filenames in the output_dir to identify the dates with missing outputs between the minimum date in the data and the current date; or specify manually
existing_files <- list.files(output_dir, pattern = 'total|spread|moneyline')
date_strings <- gsub('\\D', '', existing_files)
date_dates <- as.Date(date_strings, format = '%Y%m%d')
max_date <- max(date_dates)
start_date <- max_date + 1
## or just set it manually
# start_date <- as.Date('2015-01-01')
## end_date always the same
end_date <- Sys.Date()
## make range of dates the right format for scraping
date_range <- gsub('-', '', as.character(seq.Date(start_date, end_date, by = "day")))

for (d in date_range) {
  for (b in bet_types) {
    Sys.sleep(runif(1, min = 1.6, max = 3.2))
    line_info <- try(
      get_lines(
        sport = "NBA",
        bet_type = b,
        start_date = d
      ), silent = TRUE
    )
    if (inherits(line_info, 'try-error')) {
      next
    } else {
      output_string <- paste0(output_dir, 'nba_', d, '_', b, '.csv')
      write.csv(line_info, output_string, row.names = FALSE, quote = FALSE)
    }
  }
}
