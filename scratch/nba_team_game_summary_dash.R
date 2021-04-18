library(shiny)
library(tidyverse)

# Load data
team_games <- readRDS("../data/nba_team_games/team_games.rds")

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('NBA Team Game Dash'),
  fluidRow(
    # select dates
    column(width = 4,
            dateRangeInput(inputId = 'date_range', label = strong('select dates'),
                           start = min(team_games$game_date), end = max(team_games$game_date),
                           min = min(team_games$game_date), max = max(team_games$game_date))),
    # select metric
    column(width = 2,
           selectInput(inputId = 'metric_type', label = strong('select metric'),
                       choices = c('total', 'margin'))),
    # select viz
    column(width = 2, 
           selectInput(inputId = 'viz_type', label = strong('select viz type'),
                       choices = c('boxplot', 'density')))),
  fluidRow(
    column(width = 6,
    # filter OT
    checkboxInput(inputId = 'ot_filter', label = strong('include overtime'), value = TRUE))),
  tabsetPanel(
    tabPanel(
      title = 'League Trends',
      fluidRow(
        br(),
        plotOutput(outputId = "team_dist_plot"))
    ),
    tabPanel(
      title = 'Matchup',
      br(),
      fluidRow(
        column(width = 4,
               selectInput(inputId = 'matchup1', label = 'Team1',
                           choices = unique(team_games$team_name))),
        column(width = 4,
               selectInput(inputId = 'matchup2', label = 'Team2',
                           choices = unique(team_games$team_name))))
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output) {
  
  # set plot themes
  theme_set(theme_minimal())
  theme_update(legend.position = 'none')
  
  # Subset data
  filtered_data <- reactive({
    
    tg <- team_games
    
    if (!input$ot_filter) {
      tg <- tg %>%
        filter(overtimes == 0)
    }
    tg
    
    tg <- tg %>%
      filter(
        game_date >= as.Date(input$date_range[1]) & game_date <= as.Date(input$date_range[2])
        )
    
  })
  
  # Create output plots
  output$team_dist_plot <- renderPlot({
    
    plot_data <- filtered_data()
    
    # order factor based on metric_type
    if (input$metric_type == 'total') {
      plot_data <- plot_data %>%
        arrange(median_total) %>%
        mutate(team_name = fct_inorder(team_name))
      ## make the plot
      if (input$viz_type == 'boxplot') {
        ggplot(plot_data, aes(x = total_pts, y = team_name, fill = team_name)) +
          geom_boxplot() +
          scale_x_continuous(breaks = seq(190, 250, 10)) +
          labs(x = 'total points', y = NULL,
               title = 'Distribution of Total Points By Team')
      } else 
        if (input$viz_type == 'density') {
        ggplot(plot_data, aes(x = total_pts, y = team_name, fill = team_name)) +
          ggridges::geom_density_ridges() +
          scale_x_continuous(breaks = seq(190, 250, 10)) +
          labs(x = 'total points', y = NULL,
               title = 'Distribution of Total Points By Team')
      }
    } else 
      if (input$metric_type == 'margin') {
      plot_data <- plot_data %>%
        arrange(median_margin) %>%
        mutate(team_name = fct_inorder(team_name))
      ## make the plot
      if (input$viz_type == 'boxplot') {
        ggplot(plot_data, aes(x = margin, y = team_name, fill = team_name)) +
          geom_boxplot() +
          scale_x_continuous(breaks = seq(-30, 30, 10)) +
          labs(x = 'margin', y = NULL,
               title = 'Distribution of Margins of Victory By Team')
      } else 
        if (input$viz_type == 'density') {
        ggplot(plot_data, aes(x = margin, y = team_name, fill = team_name)) +
          ggridges::geom_density_ridges() +
          scale_x_continuous(breaks = seq(-30, 30, 10)) +
          labs(x = 'margin', y = NULL,
               title = 'Distribution of Margins of Victory By Team')
      }
    }
  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)