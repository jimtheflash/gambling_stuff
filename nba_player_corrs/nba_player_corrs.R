library(tidyverse)

boxscores <- read_csv('/Users/jim/Desktop/nba_2020-21.csv')

teams <- unique(boxscores$TEAM_NAME)

output <- list()
correl_df_list <- list()

for (team in teams) {
  
  team_games <- boxscores %>%
    filter(TEAM_NAME == team)
  
  team_leaders <- team_games %>%
    group_by(PLAYER_NAME) %>%
    summarise(games = n_distinct(GAME_ID),
              minutes = sum(MIN, na.rm = TRUE),
              score = sum(dk, na.rm = TRUE),
              max_score = max(dk, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(score))
  
  players <- unique(team_leaders$PLAYER_NAME)
  
  wide_team_games <- team_games %>%
    left_join(team_leaders) %>%
    filter(max_score >= 10,
           games >= 2,
           minutes >= 20) %>%
    select(GAME_ID, PLAYER_NAME, dk) %>%
    pivot_wider(id_cols = GAME_ID, 
                names_from = PLAYER_NAME, 
                values_from = dk, values_fill = 0) %>%
    select(-GAME_ID)
  
  plots <- list()
  
  player_pairs <- c()
  
  for (player in players) {
    
    if (!player %in% names(wide_team_games)) {
      next
    }
    
    other_players <- names(wide_team_games)[names(wide_team_games) != player]
    
    for (other_player in other_players) {
      
      player_pair <- paste(sort(c(player, other_player)), collapse = '')
      
      if (player_pair %in% player_pairs) {
        next
      }
      
      pairwise_data <- wide_team_games %>%
        select(one_of(player, other_player))
      
      pairwise_data$player <- pairwise_data[[player]]
      pairwise_data$other_player <- pairwise_data[[other_player]]
      
      title_text <- paste0()
      
      gg <- ggplot(pairwise_data, aes(player, other_player)) +
        geom_point() +
        coord_cartesian(xlim = c(0, 80), ylim = c(0, 80)) +
        labs(x = player, y = other_player)
      
      correl <- cor.test(pairwise_data$player, pairwise_data$other_player)
      
      correl_df <- data.frame(
        player = player,
        other_player = other_player,
        team = team,
        corr = correl$estimate,
        p = correl$p.value
      )
      
      pp_output <- list(
        plot_data = pairwise_data,
        scatterplot = gg,
        correlation = correl
      )
      
      correl_df_list[[player_pair]] <- correl_df
      
      plots[[player_pair]] <- pp_output
      
      player_pairs <- c(player_pairs, player_pair)
    }
    
    team_output <- list(team_leaders = team_leaders,
                        plots = plots)
    
    output[[team]] <- team_output
  }
}

all_player_correls <- bind_rows(correl_df_list)