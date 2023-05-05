
# imports
library(tidyverse)
library(shiny)
library(bslib)
theme_set(theme_bw())

# datasets
team_info = read_csv("https://raw.githubusercontent.com/mrbarron3/hockey/main/team_info.csv") 
game_stats = read_csv("https://raw.githubusercontent.com/mrbarron3/hockey/main/game_teams_stats.csv")

# dataset manipulation
teams = team_info %>%
  select(team_id, shortName, teamName) %>%
  unite("name", shortName:teamName, sep = " ")

hockey_stats = game_stats %>% 
  full_join(teams) %>%
  filter(settled_in != "tbc") %>%
  mutate(name = str_replace(name, "Rangers ", ""), name = str_replace(name, "Islanders ", ""), name = str_replace(name, "NY", "New York"), won = str_replace(won, "TRUE", "win"), won = str_replace(won, "FALSE", "loss")) %>%
  select(name, HoA, won, goals, shots, settled_in, game_id) %>%
  distinct()

# text
descriptive_text = p("Explore NHL Game Data from the 2000-2001 season to the 2019-2020 season from 31 active and 2 defunct (Atlanta Thrashers and Phoenix Coyotes) NHL teams. This data was downloaded from an ", a("NHL Game Data repository", href="https://www.kaggle.com/datasets/martinellis/nhl-game-data?resource=download"), "created by Martin Ellis, using data originally documented by Kevin Sidwar, an NHL fan, who began documenting the still un-documented NHL stats API.")
text2 = p("Select a team, or multiple teams, from the dropdown menu as well as the game outcome of interest to view corresponding plots. Select both possible game outcomes to view all games played by the selected team/teams. Brush over the scatter plot to refine your selection. Refresh the page to eliminate your brush selection.")

# plot functions
transparent <- function() {
  theme(
      panel.background = element_rect(fill= "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 15),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill= "transparent"))
}


plot_scatter = function(df) {
  ggplot(df) +
    geom_jitter(aes(shots, goals, shape = selected, color = name), width = 0, height = 0.375, alpha = 0.7) +    
    scale_shape_manual(values = c(20, 19))+
    scale_y_continuous(breaks = seq(0, 13, by = 1), 
                       expand = c(0, 0.1, 0.1, 0.1)) +
    labs(x = "Shots on Goal", y = "Goals", color = "Team", shape = "Selected") +
    transparent()
}


plot_bar = function(df) {
  ggplot(df) +
    geom_bar(aes(HoA, fill = HoA)) +
    scale_fill_manual(values = c("#202121", "#f75252")) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0.1)) +
  labs(x = "Game Location", y = "Number of Games Played", fill = "Game Location") +
    transparent()
}


reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

# shinyApp
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "simplex",
    fg = "#261912", 
    bg = "#F2F2F2", 
    primary = "#bf7796",
    base_font = font_google("Inter"), 
    heading_font = font_google("Bebas Neue")
    
  ),
  titlePanel("NHL Game Stats Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Team(s)", sort(unique(hockey_stats$name)), c("New York Rangers", "New York Islanders"), multiple = TRUE),
      selectInput("outcome", "Game Outcome", unique(hockey_stats$won), c("win"), multiple = TRUE),
      descriptive_text,
      text2
    ) ,
    mainPanel(
      plotOutput("scatter", brush = "plot_brush"),
      plotOutput("bar"))
))


server = function(input, output) {
  selected_p <- reactiveVal(rep(TRUE, nrow(hockey_stats)))
  observeEvent(
    input$plot_brush,
    selected_p(reset_selection(hockey_stats, input$plot_brush))
    )
  
  
  output$scatter <- renderPlot({
    hockey_stats %>%
      mutate(selected = factor(selected_p(), levels = c("FALSE", "TRUE"))) %>%
      filter(
        name %in% input$team, 
        won %in% input$outcome
      ) %>%
      plot_scatter()
    }, bg = "transparent"
  )
  
  output$bar <- renderPlot({
    hockey_stats %>%
      filter(
        name %in% input$team, 
        won %in% input$outcome,
        selected_p()
      ) %>%
      plot_bar()
    }, bg = "transparent"
  )
}

shinyApp(ui, server)
