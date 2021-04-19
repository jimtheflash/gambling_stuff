
# fanduel -----------------------------------------------------------------

# get all the nba game id's for the day
all_nba <- jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psmg/UK/63747.3.json')
events <- as.character(all_nba$events$idfoevent)

output_list <- list()
for (e in events) {
  message('getting props for event ', e)
  json_string <- paste0('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/', e, '.json')
  game_event <- jsonlite::fromJSON(json_string)
  game_event_market_groups <- game_event$eventmarketgroups
  player_props <- game_event_market_groups$markets[game_event_market_groups$name == 'All Player Props'][[1]]
  first_basket <- player_props$selections[player_props$name == 'First Basket'][[1]]
  output_list[[length(output_list) + 1]] <- first_basket
  Sys.sleep(1.123)
}
## looks like each game gets an id, which may be a better way to dig in
## how about the event id's being passed to another url

game_event <- jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/1000454.3.json')
game_event_market_groups <- game_event$eventmarketgroups

player_props <- game_event_market_groups$markets[game_event_market_groups$name == 'All Player Props'][[1]]
first_basket <- player_props$selections[player_props$name == 'First Basket'][[1]]

# drafkings ---------------------------------------------------------------

## set a timestamp so we know when these odds were refreshed
time_stamp <- Sys.time()

## get the big ol' json from dk - this has all the nba markets
nba_markets <- jsonlite::fromJSON('https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full?includePromotions=true&format=json')

## start parsing
offer_categories <- nba_markets$eventGroup$offerCategories

# player props (for first fg)
player_props <- offer_categories[offer_categories$name == 'Player Props', ]$offerSubcategoryDescriptors[[1]]$offerSubcategory
ffg <- player_props$offers[player_props$name == 'First Field Goal'][[1]]           


output_list <- list()
for (i in 1:length(ffg)) {
  outcomes <- ffg[[i]]$outcomes[[1]]
  output_list[[i]] <- outcomes
}

fpts_df <- do.call(rbind, output_list)

# under the game props offer category we want the data.frame with the offer subcats (which is nested, and looks weird if unlisted, so accessing the first element)
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

## now some stuff with points?
pts <- player_props$offers[player_props$name == 'Points'][[1]]           





# betonline ---------------------------------------------------------------


