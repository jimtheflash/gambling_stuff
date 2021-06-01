team_lu <- baseballr::teams_lu_table
team_lu <- team_lu[team_lu$league.name %in% c('American League', 'National League'), ]

lu <- list()

for (i in 1:nrow(team_lu)) {
  row_in <- team_lu[i, ]
  row_out <- list(
    name = row_in$abbreviation,
    aliases = c(row_in$name, tolower(row_in$name), gsub(' ', '', tolower(row_in$name)),
                row_in$teamName, tolower(row_in$teamName), 
                row_in$shortName, tolower(row_in$shortName), gsub(' ', '', tolower(row_in$shortName)),
                paste0(row_in$abbreviation, ' ', row_in$teamName))
  )
  lu[[i]] <- row_out
  
}

lu_df <- as.data.frame(do.call(rbind, lu))
lu_df$name <- unlist(lu_df$name)

jsonlite::write_json(lu_df, '/Users/jim/Documents/betfinder/inst/lu/mlb/team/lu.json', pretty = TRUE)