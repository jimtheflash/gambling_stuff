
# fanduel -----------------------------------------------------------------

# get everything on nba main page
all_nba = jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psmg/UK/63747.3.json')
events = all_nba$events
## after exploring this, doesn't look to have the props, just the spread, 1st half spread, ML, total (which, not the end of the world!)

## NOT RUN
# event_markets = list()
# for (e in 1:nrow(events)) {
#   game_markets <- events[e, 'markets']
#   event_markets[[length(event_markets) + 1]] <- events[e, 'markets']
# }


## looks like each game gets an id, which may be a better way to dig in
## how about the event id's being passed to another url

test = jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/972499.3.json')
event_market_groups = test$eventmarketgroups

# drafkings ---------------------------------------------------------------

nba_markets = jsonlite::fromJSON('https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full?includePromotions=true&format=json')
offer_categories = nba_markets$eventGroup$offerCategories
game_props = offer_categories[offer_categories$name == "Game Props", ]$offerSubcategoryDescriptors
offers = game_props[[1]]$offerSubcategory$offers
for (o in offers) {
  if (o[[1]]$label != "Next Team to Score - at Score 0-0") {
    print('nahhh')
  } else {
    print('found it')
  }
}
