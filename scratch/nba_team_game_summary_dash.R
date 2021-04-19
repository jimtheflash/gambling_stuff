
# global ------------------------------------------------------------------

# libraries
library(shiny)
library(tidyverse)
library(ggridges)


# load data
team_games <- readRDS("../data/nba_team_games/team_games.rds")

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('NBA Team Game Dash'),
  br(),
  fluidRow(
    # select metric
    column(width = 2,
           selectInput(inputId = 'metric_type', label = strong('select metric'),
                       choices = c('total', 'margin'))),
    # select viz
    column(width = 2, 
           selectInput(inputId = 'viz_type', label = strong('select viz type'),
                       choices = c('boxplot', 'density')))),
  fluidRow(
    # select dates
    column(width = 5,
            dateRangeInput(inputId = 'date_range', label = strong('select dates'),
                           start = min(team_games$game_date), end = max(team_games$game_date),
                           min = min(team_games$game_date), max = max(team_games$game_date)))),
  tabsetPanel(
    tabPanel(
      title = 'League Trends',
      fluidRow(
        br(),
        plotOutput(outputId = 'team_dist_plot'))
    ),
    tabPanel(
      title = 'Compare Teams',
      br(),
      fluidRow(
        column(width = 10,
               selectizeInput(inputId = 'selected_teams', label = 'Select Teams to Compare',
                              multiple = TRUE,
                              choices = unique(team_games$team_name)))),
      br(),
      plotOutput(outputId = 'comparison_plot')
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output) {
  
  #### set plot themes ####
  theme_set(theme_minimal(base_size = 12))
  theme_update(legend.position = 'none',
               legend.title = element_blank())
  
  #### subset team-game data for distribution plots ####
  tg_data <- reactive({
    
    team_games %>%
      filter(
        game_date >= as.Date(input$date_range[1]) & game_date <= as.Date(input$date_range[2])
        )
  })
  
  #### make team-game distribution plots ####
  output$team_dist_plot <- renderPlot(res = 96, {
    
    # plot by metric
    if (input$metric_type == 'total') {
      plot_data <- tg_data() %>%
        group_by(team_name) %>%
        mutate(median_total = median(total_pts)) %>%
        ungroup() %>%
        arrange(median_total) %>%
        mutate(team_name = fct_inorder(team_name))
      ## make the specific plots
      if (input$viz_type == 'boxplot') {
        ggplot(plot_data, aes(x = total_pts, y = team_name, fill = team_name)) +
          geom_boxplot() +
          scale_x_continuous(breaks = seq(round(min(plot_data$total_pts), -1), 
                                          round(max(plot_data$total_pts), -1),
                                          10)) +
          labs(x = 'total points', y = NULL,
               title = 'Distribution of Total Points By Team')
      } else 
        if (input$viz_type == 'density') {
          ggplot(plot_data, aes(x = total_pts, y = team_name, fill = team_name)) +
            geom_density_ridges() +
            scale_x_continuous(breaks = seq(round(min(plot_data$total_pts), -1), 
                                            round(max(plot_data$total_pts), -1),
                                            10)) +
            labs(x = 'total points', y = NULL,
                 title = 'Distribution of Total Points By Team')
      }
    } else 
      if (input$metric_type == 'margin') {
      plot_data <- tg_data() %>%
        group_by(team_name) %>%
        mutate(median_margin = median(margin)) %>%
        ungroup() %>%
        arrange(median_margin) %>%
        mutate(team_name = fct_inorder(team_name))
      
      ## make the plot
      if (input$viz_type == 'boxplot') {
        ggplot(plot_data, aes(x = margin, y = team_name, fill = team_name)) +
          geom_boxplot() +
          scale_x_continuous(breaks = seq(round(min(plot_data$margin), -1), 
                                          round(max(plot_data$margin), -1),
                                          5)) +
          labs(x = 'margin', y = NULL,
               title = 'Distribution of Margins of Victory By Team')
      } else 
        if (input$viz_type == 'density') {
          ggplot(plot_data, aes(x = margin, y = team_name, fill = team_name)) +
            geom_density_ridges() +
            scale_x_continuous(breaks = seq(round(min(plot_data$margin), -1), 
                                            round(max(plot_data$margin), -1),
                                            5)) +
            labs(x = 'margin', y = NULL,
                 title = 'Distribution of Margins of Victory By Team')
      }
    }
  })
  
  #### subset team-games for comparison plots ####
  compare_data <- reactive({
    
    tg_data() %>%
      filter(
        team_name %in% input$selected_teams)
  })
  
  #### make comparison plots ####
  output$comparison_plot <- renderPlot(res = 96, {
    
    validate(need(input$selected_teams > 0, "Waiting for teams to be selected..."))
    
    plot_data <- compare_data()
    req(plot_data)
    
    if (input$metric_type == 'total') {

      if (input$viz_type == 'boxplot') {
        ggplot(plot_data, aes(x = total_pts, y = team_name, fill = team_name)) +
          geom_boxplot() +
          scale_x_continuous(breaks = seq(round(min(plot_data$total_pts), -1), 
                                          round(max(plot_data$total_pts), -1),
                                          10)) +
          labs(x = 'total points', y = NULL,
               title = 'Distribution of Total Points By Team')
        
      } else 
        if (input$viz_type == 'density') {
          ggplot(plot_data, aes(total_pts, fill = team_name)) +
            geom_density(alpha = .5) +
            scale_x_continuous(breaks = seq(round(min(plot_data$total_pts), -1), 
                                            round(max(plot_data$total_pts), -1),
                                            10)) +
            labs(x = 'total points', y = NULL,
                 title = 'Distribution of Total Points By Team') +
            theme(legend.position = 'top')
            
        }
    } else 
      if (input$metric_type == 'margin') {
          
        if (input$viz_type == 'boxplot') {
          
          ggplot(plot_data, aes(x = margin, y = team_name, fill = team_name)) +
            geom_boxplot() +
            scale_x_continuous(breaks = seq(round(min(plot_data$margin), -1), 
                                            round(max(plot_data$margin), -1),
                                            5)) +
            labs(x = 'margin', y = NULL,
                 title = 'Distribution of Margins of Victory By Team')
          
        } else 
          if (input$viz_type == 'density') {
            
            ggplot(plot_data, aes(margin, fill = team_name)) +
              geom_density(alpha = .5) +
              scale_x_continuous(breaks = seq(round(min(plot_data$margin), -1), 
                                              round(max(plot_data$margin), -1),
                                              5)) +
              labs(x = 'margin', y = NULL,
                   title = 'Distribution of Margins of Victory By Team') +
              theme(legend.position = 'top')
            
          }
      }
  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)