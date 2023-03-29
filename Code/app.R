library(shiny)

## Load GITHUB Environment## 
SourceURL_GE_Func <- "https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Code/General%20Environment%20Functions.R"
SourceURL_GE <- "https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/Code/Create%20General%20Environment.R"
source_url(SourceURL_GE_Func)
source_url(SourceURL_GE)

git_base_path <- 'https://raw.githubusercontent.com/twinfield10/Rebirtha-Model/main/DailyData/'
mlb_api_batters <- read.csv(paste0(git_base_path,'Lineup_Batters.csv'), stringsAsFactors = FALSE) %>%
  mutate(
    home.TeamID = as.character(home.TeamID),
    away.TeamID = as.character(away.TeamID)
  ) %>% select(-c(X))
mlb_api_pitchers <- read.csv(paste0(git_base_path,'Lineup_Pitchers.csv'), stringsAsFactors = FALSE) %>%
  mutate(
    home.TeamID = as.character(home.TeamID),
    away.TeamID = as.character(away.TeamID)
  ) %>% select(-c(X))
daily_odds <- read.csv(paste0(git_base_path,'DailyOdds.csv'), stringsAsFactors = FALSE) %>% select(-c(X))
bets_table <- read.csv(paste0(git_base_path,'BetsTable.csv'), stringsAsFactors = FALSE) %>% select(-c(X))
pretty_bets_tbl <- read.csv(paste0(git_base_path,'PrettyBetsTable.csv'), stringsAsFactors = FALSE) %>%
  mutate(
    TeamID = as.character(TeamID)
  ) %>% select(-c(X))


todaysschedule <- today_build(loc = "FULL")
tb_A <- today_build(loc = "A")
tb_H <- today_build(loc = "H")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$style(type = "text/css", "html, body { height: 100%; margin: 0; padding: 0; }"),
  tags$style(type = "text/css", ".border-top { border-top: 2px solid black; }"),
  tags$style(type = "text/css", ".full-page-header { font-size: 32px; font-weight: bold; text-align: center; }"),
  
  
  # App title ----
  titlePanel("Rebirtha Model - Daily Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2,
      uiOutput(outputId = "pkfilter"),
      # Input: Slider for the number of bins ----
      selectInput(inputId = "label_select",
                  label = "Select Game:",
                  choices = unique(todaysschedule$label)
                  ),
      br(),
      "Projection data is sourced from Baseball Prospectus'
      PECOTA database. For more info, please read more ",
      tags$a(href = "https://legacy.baseballprospectus.com/glossary/index.php?search=pecota",
             "here"), ".",
      br(),
      br(),
      "Team logos, colors, and headshots were applied using ",
      tags$a(href = "https://cran.r-project.org/web/packages/mlbplotR/index.html",
             "mlbplotR"),
      ", a package created by GitHub User ",
      tags$a(href = "https://github.com/camdenk",
             "camdenk."), ".",
      br(),
      br(),
      "Rebirtha Model created by ",
      tags$a(href = "https://twitter.com/TMWAnalytics",
             "@TMWAnalytics"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        id = "tabs",
        selected = "full",
        tabPanel(
          "All Games", value = "full",
          fluidRow(
            column(width = 12, uiOutput(outputId = "totaltbl"))
          )
        ),
        tabPanel( "Game Breakdown", value = "gm",
          fluidRow(
            column(width = 6, uiOutput(outputId = "a_game_info"), br(), br()),
            column(width = 6, uiOutput(outputId = "h_game_info"), br(), br())
          ),
          fluidRow(
            column(12, class = "full-page-header border-top", "Rebirtha Calculation and Summary")
          ),
          fluidRow(
            column(12, uiOutput(outputId = "Rebirtha_Calc"), br(), br())
          ),
          fluidRow(
            column(12, class = "full-page-header border-top", "Pitching Matchups")
          ),
          fluidRow(
            column(width = 6, uiOutput(outputId = "a_pitch_info"), uiOutput(outputId = "away_p_stats"), br(), plotOutput(outputId = "a_pitcherplot")),
            column(width = 6, uiOutput(outputId = "h_pitch_info"), uiOutput(outputId = "home_p_stats"), br(), plotOutput(outputId = "h_pitcherplot"))
          ),
          fluidRow(
            column(12, uiOutput(outputId = "away_p_calc"))
          ),
          fluidRow(
            column(12, uiOutput(outputId = "home_p_calc"), br())
          ),
          fluidRow(
            column(12, class = "full-page-header border-top", "Batting Matchups")
          ),
          fluidRow(
            column(width = 6, uiOutput(outputId = "away_lu"), br()),
            column(width = 6, uiOutput(outputId = "home_lu"), br())
          )
        )
        
      )
          
  )
)
)

server <- function(input, output, session) {
  
  # Filter the todaysschedule dataset based on the selected label
  filtered_ID <- reactive({
    subset(todaysschedule, label == input$label_select)
  })

  
  ## Generate Game Information ##
  output$a_game_info <- renderUI({
    id <- as.integer(filtered_ID()[1,1])
    
    df <- today_build(loc = "A") %>% filter(game_pk == id)
    full_name <- df %>% select(full_name) %>% pull()
    logo <- df %>% select(team_logo_espn) %>% pull()
    actual_wl <- df %>% select(Actual_Label) %>% pull()
    PECOTA_wl <- df %>% select(PECOTA_Label) %>% pull()
    ML_Away <- df %>% select(betonline_away) %>% mutate(
      betonline_away = if_else(betonline_away < 0 & !is.na(betonline_away) , as.character(betonline_away),
                               if_else(betonline_away > 0 & !is.na(betonline_away), paste0("+",as.character(betonline_away)), "No Line"))
    ) %>% pull()
    Playoff_Label <- df %>% select(PECOTA_Playoff_Proj_Label) %>% pull()
    
    
    HTML(paste0("<h1 style='text-align:center;font-size:34px;font-weight:bold;'>",full_name,"</h1>",
                "<h2 style='text-align:center;font-size:34px;font-weight:bold;'>","(",ML_Away,")","</h2>",
               "<div style='text-align: center;'><img src='",logo,"' alt='AwayLogo' height='175'></div>",
               "<h3 style='text-align:center;font-size:24px;font-weight:bold;'>",actual_wl,"</h3>",
               "<h4 style='text-align:center;font-size:20px;font-weight:bold;font-style:italic;'>", PECOTA_wl, "</h4>",
               "<h5 style='text-align:center;font-size:16px;font-weight:bold;font-style:italic;'>", Playoff_Label, "</h5>"))
  })
  
  output$h_game_info <- renderUI({
    id <- as.integer(filtered_ID()[1,1])
    
    df <- today_build(loc = "H") %>% filter(game_pk == id)
    full_name <- df %>% select(full_name) %>% pull()
    logo <- df %>% select(team_logo_espn) %>% pull()
    actual_wl <- df %>% select(Actual_Label) %>% pull()
    PECOTA_wl <- df %>% select(PECOTA_Label) %>% pull()
    ML_Home <- df %>% select(betonline_home) %>% mutate(
      betonline_home = if_else(betonline_home < 0 & !is.na(betonline_home) , as.character(betonline_home),
                               if_else(betonline_home > 0 & !is.na(betonline_home), paste0("+",as.character(betonline_home)), "No Line"))
      ) %>% pull()
    Playoff_Label <- df %>% select(PECOTA_Playoff_Proj_Label) %>% pull()
    
    
    HTML(paste0("<h1 style='text-align:center;font-size:34px;font-weight:bold;'>",full_name,"</h1>",
                "<h2 style='text-align:center;font-size:34px;font-weight:bold;'>","(",ML_Home,")","</h2>",
               "<div style='text-align: center;'><img src='",logo,"' alt='HomeLogo' height='175'></div>",
               "<h3 style='text-align:center;font-size:24px;font-weight:bold;'>",actual_wl,"</h3>",
               "<h4 style='text-align:center;font-size:20px;font-weight:bold;font-style:italic;'>", PECOTA_wl, "</h4>",
               "<h5 style='text-align:center;font-size:16px;font-weight:bold;font-style:italic;'>", Playoff_Label, "</h5>"))
  })
  
  ## Generate Pitcher Information ##
  output$a_pitch_info <- renderUI({
    id <- as.integer(filtered_ID()[1,1])
    
    df <- SP_Stats_Chart_Build(pk = id, loc = "A", type = "INFO")
    p_full_name <- df %>% select(name, throws) %>% mutate(namecombo = paste0(name, ' (',throws,')')) %>% pull()
    p_headshot <- df %>% select(headshot) %>% pull()
    Role_Fill <- df %>% select(Pos_Full) %>% pull()
    Role_Sub <- df %>% select(Pos_Sub) %>% pull()
    
    
    HTML(paste0("<h2 style='text-align:center;font-size:30px;font-weight:bold;'>",p_full_name,"</h2>",
                "<div style='text-align: center;'><img src='",p_headshot,"' alt='AwayLogo' height='125'></div>",
                "<h3 style='text-align:center;font-size:20px;font-weight:bold;'>",Role_Fill,"</h4>",
                "<h4 style='text-align:center;font-size:18px;font-weight:bold;font-style:italic;'>", Role_Sub, "</h4>"))
  })
  
  output$h_pitch_info <- renderUI({
    id <- as.integer(filtered_ID()[1,1])
    
    df <- SP_Stats_Chart_Build(pk = id, loc = "H", type = "INFO")
    p_full_name <- df %>% select(name, throws) %>% mutate(namecombo = paste0(name, ' (',throws,')')) %>% pull()
    p_headshot <- df %>% select(headshot) %>% pull()
    Role_Fill <- df %>% select(Pos_Full) %>% pull()
    Role_Sub <- df %>% select(Pos_Sub) %>% pull()
    
    
    HTML(paste0("<h2 style='text-align:center;font-size:30px;font-weight:bold;'>",p_full_name,"</h2>",
                "<div style='text-align: center;'><img src='",p_headshot,"' alt='AwayLogo' height='125'></div>",
                "<h3 style='text-align:center;font-size:20px;font-weight:bold;'>",Role_Fill,"</h4>",
                "<h4 style='text-align:center;font-size:18px;font-weight:bold;font-style:italic;'>", Role_Sub, "</h4>"))
  })
  
  ## Generate Pitcher Stats Chart ##
  output$away_p_stats <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    SP_Stats_Chart_Build(pk = id, loc = "A", type = "CHART")
  },
  width = "100%")
  
  output$home_p_stats <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    SP_Stats_Chart_Build(pk = id, loc = "H", type = "CHART")
  },
  width = "100%")
  
  ## Generate Pitcher calc Chart ##
  output$away_p_calc <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    SP_Stats_Chart_Build(pk = id, loc = "A", type = "CALC")
  },
  width = "100%")
  
  output$home_p_calc <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    SP_Stats_Chart_Build(pk = id, loc = "H", type = "CALC")
  },
  width = "100%")

  
  ## Pitcher Distro Plots
  output$a_pitcherplot <- renderPlot({
    id <- as.integer(filtered_ID()[1,1])
    APP_p_pos_plot_tm(pk = id, loc = "A")
  })
  
  output$h_pitcherplot <- renderPlot({
    id <- as.integer(filtered_ID()[1,1])
    APP_p_pos_plot_tm(pk = id, loc = "H")
  })
  
  ## GT Lineup Chart
  output$away_lu <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    gt_lu_output_tbl_build(id = id, ctype = 'A_BAT')
  },
  width = "100%")
  
  output$home_lu <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    gt_lu_output_tbl_build(id = id, ctype = 'H_BAT')
  },
  width = "100%")
  
  output$Rebirtha_Calc <- render_gt({
    id <- as.integer(filtered_ID()[1,1])
    pretty_bets_gt(pk = id)
  },
  width = "100%")
  
  
  
  output$totaltbl <- render_gt({
    pretty_total_bets()
  })
  
  



}

shinyApp(ui = ui, server = server)