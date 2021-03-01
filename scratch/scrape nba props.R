
# fanduel -----------------------------------------------------------------

# get everything on nba main page
all_nba <- jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psmg/UK/63747.3.json')
events <- all_nba$events
## after exploring this, doesn't look to have the props, just the spread, 1st half spread, ML, total (which, not the end of the world!)

## NOT RUN
# event_markets = list()
# for (e in 1:nrow(events)) {
#   game_markets <- events[e, 'markets']
#   event_markets[[length(event_markets) + 1]] <- events[e, 'markets']
# }


## looks like each game gets an id, which may be a better way to dig in
## how about the event id's being passed to another url

game_event <- jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/973064.3.json')
game_event_market_groups <- game_event$eventmarketgroups

### ok so it's most certainly there's a link between the selectionid in embedded df's and the price, but it's sooooooooo tedious that i'm gonna have to take a pause


# drafkings ---------------------------------------------------------------

## set a timestamp so we know when these odds were refreshed
time_stamp <- Sys.time()
## get the big ol' json from dk - this has all the nba markets
nba_markets <- jsonlite::fromJSON('https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full?includePromotions=true&format=json')
## start parsing
offer_categories <- nba_markets$eventGroup$offerCategories
## under the game props offer category we want the data.frame with the offer subcats (which is nested, and looks weird if unlisted, so accessing the first element)
game_props <- offer_categories[offer_categories$name == "Game Props", ]$offerSubcategoryDescriptors[[1]]
## same deal with the first team to score sub-category
first_team_to_score <- game_props[game_props$offerSubcategory == "First Team to Score", ]$offerSubcategory$offers[[1]]
## get all the outcome data.frames into a list for a quick loop
outcomes <- do.call(rbind, first_team_to_score)$outcomes
## loop through the outcomes to make a list of data.frames to be combined
outcomes_df_list <- list()
for (o in outcomes) {
  output_df <- data.frame(
    team = o$label,
    odds = o$oddsAmerican,
    opponent = rev(o$label),
    ts = time_stamp
  )
  outcomes_df_list[[length(outcomes_df_list) + 1]] <- output_df
}
## stitch together and presto!
output <- do.call(rbind, outcomes_df_list)


# betonline ---------------------------------------------------------------


