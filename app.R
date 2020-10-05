  # LINK - https://lhds.shinyapps.io/board_games/

  library(shinydashboard)
  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(dplyr)  

  bgg <- read.csv("bgg_db_1806.csv", stringsAsFactors = F)
  bgg$names <- iconv(x = bgg$names, to = 'ASCII', sub = '')


  
  img1 <- paste0('https://i.pinimg.com/originals/85/',
                 'a3/0e/85a30e24f8256835385b05c4c950333b.jpg')
  img2 <- paste0("https://orig00.deviantart.net/b7e6/f/2017/",
                 "332/c/7/domino_by_tylerthemoviemaker6-dbv4y20.png")
  img3 <- "https://static.thenounproject.com/png/138946-200.png"
  img4 <- "https://static.thenounproject.com/png/139786-200.png"
  img5 <- "https://static.thenounproject.com/png/1151432-200.png"
  img6 <- "http://icons.iconarchive.com/icons/icons8/ios7/512/Gaming-Cards-icon.png"
  img7 <- "https://www.shareicon.net/data/2016/07/28/803152_game_512x512.png"
  img8 <- "https://cdn3.iconfinder.com/data/icons/sports-recreation/128/go-game-512.png"
  
  img9 <- "https://image.flaticon.com/icons/png/512/92/92767.png"
  img10 <- paste0("http://silhouettesfree.com/professions-and-occupations/",
                  "fireman/bomb-with-burning-fuse-silhouette-image.png")

  
  tab1 <- tabPanel(title = 'Board Game Statistics',
                   fluidRow(
                     column(9, br(), wellPanel(
                       htmlOutput("intro2")
                     )),
                     column(3, tags$img(src = img2,
                                      width = 90, height = 150,
                                      style = 'transform:rotate(180deg); margin-left: 50px')
                            )
                  ),
                  br(),
                  fluidRow(
                     column(6, br(), plotOutput('best_stat')),
                     column(3, selectInput(inputId = "statistic", label = "Choose Statistic",
                                           choices = c("Average Rating" = "avg_rating",
                                                       "Geek Rating" = "geek_rating",
                                                       "Number of Votes" = "num_votes",
                                                       "Owned" = "owned",
                                                       "Weight" = "weight"), selected = "owned"),
                                           
                            numericInput(inputId = "top_x", label = "Show Top:", 
                                         value = 5),
                            radioButtons(inputId = "lowvshigh", label = "With Lowest or Highest Results?",
                                         choices = c("Lowest", "Highest"), selected = "Highest"),
                            selectInput(inputId = "min_player", label = "Minimum Players",
                                                           choices = sort(unique(bgg$min_players)), selected = 0),
                            selectInput(inputId = "max_player", label = "Maximum Players",
                                                           choices = sort(unique(bgg$max_players)), selected = 200)
                            ),
                     column(3, br(), tableOutput('results'), br(),
                            wellPanel("Don't know some of the games? Use the second tab to know about them more."))
                     ),
                  br(), br()
  
  )
  
  tab2 <- tabPanel(title = "Search",
                   br(),
                   fluidRow(column(2, 
                                   tags$img(src = img3, 
                                               width = 150, height = 200,
                                               style = 'transform:rotate(30deg); margin-left: 10px'),
                                   br(),
                                   tags$img(src = img4, 
                                            width = 200, height = 200,
                                            style = 'margin-left: 50px'),
                                   br(),
                                   tags$img(src = img6, 
                                            width = 120, height = 120,
                                            style = 'margin-left: 2px'),
                                   br(), br(),
                                   tags$img(src = img5, 
                                            width = 200, height = 200,
                                            style = 'transform:rotate(345deg); margin-left: 5px'),
                                   br(), br(),
                                   tags$img(src = img7, 
                                            width = 160, height = 160,
                                            style = 'transform:rotate(15deg); margin-left: 5px')
                                   ),
                            column(6, wellPanel(textInput(inputId = "tab2_search", value = "[redacted]",
                                                          label = "Search a Game:"),
                                                htmlOutput('tab2_game_found'))
                                   ),
                            column(1),
                            column(3, wellPanel(htmlOutput("help_panel"), br(),
                                                selectInput(inputId = "tab2_search_help1",
                                                            label = "Symbols, numbers, A, B",
                                                            choices = sort(bgg$names)[1:728]),
                                                selectInput(inputId = "tab2_search_help2",
                                                            label = "C - E",
                                                            choices = sort(bgg$names)[729:1571]),
                                                selectInput(inputId = "tab2_search_help3",
                                                            label = "F - K",
                                                            choices = sort(bgg$names)[1572:2360]),
                                                selectInput(inputId = "tab2_search_help4",
                                                            label = "L - O",
                                                            choices = sort(bgg$names)[2361:3076]),
                                                selectInput(inputId = "tab2_search_help5",
                                                            label = "P - S",
                                                            choices = sort(bgg$names)[3077:4023]),
                                                selectInput(inputId = "tab2_search_help6",
                                                            label = "T - Z",
                                                            choices = sort(bgg$names)[4024:4999])
                                                ), br(),
                                   tags$img(src = img8, 
                                            width = 280, height = 280,
                                            style = 'transform:rotate(5deg); margin-left: 30px')
                                   )
                   )
  )
  
  tab3 <- tabPanel(title = "Play Here",
                   br(),  
                   fluidRow(column(3, fluidRow(tags$img(src = img10, width = 60, height = 60, 
                                                        style = 'margin-left: 100px; margin-bottom: -88px'),
                                               tags$img(src = img9, width = 150, height = 150, 
                                                      style = 'margin-left: 12px')
                                             
                   )),
                   column(6, htmlOutput('game_title')),
                   column(3, actionButton("game_rules", "GAME RULES", style = 'font-size: 40px !important; margin-top: 48px; margin-left: 42px'))
                   ),
                   column(3, 
                          wellPanel(selectInput(inputId = "character", label = "Choose the Player",
                                                choices = c(
                                                  "Square" = 15,
                                                  "Circle" = 16,
                                                  "Triangle" = 17,
                                                  "Diamond" = 18,
                                                  "Plus Sign" = 3,
                                                  "Asterisk" = 8,
                                                  "Two Triangles" = 11), selected = 17)
                          ),
                          wellPanel(radioButtons(inputId = "move_player", label = "Move Player",
                                                    choices = c("Stay where you are" = 0,
                                                                
                                                                "One step forward" = 1,
                                                                "Two step forward" = 5,
                                                            
                                                                "One step backward" = 2,
                                                                "Two step backward" = 6,
                                                                
                                                                "One step right" = 3,
                                                                "Two step right" = 7,
                                                                
                                                                "One step left" = 4,
                                                                "Two step left" = 8)     )
                                    )
                          
                          ),
                   column(6,
                          wellPanel(plotOutput('game_board'),
                                       htmlOutput('game_message'))),
                   column(3, wellPanel(htmlOutput("control_game"), br(),
                                       radioButtons(inputId = "game_levels", label = "Level / Difficulty",
                                                    choices = c(1:5)),
                                       checkboxInput(inputId = "show_bombs", label = "Show Bombs",
                                                     value = F)
                                       ),
                          fluidRow(tags$img(src = img9, width = 150, height = 150, 
                                   style = 'transform:scaleX(-1); margin-left: 60px'),
                                   tags$img(src = img10, width = 60, height = 60, 
                                            style = 'margin-left: 16px; margin-bottom: -55px')
                          )
                          )
                   
  )
  
  tab4 <- tabPanel(title = "About", br(), br(),
                   column(3),
                   column(6, wellPanel(
                     tags$p("The app ", tags$b("BOARD GAMES"), " was ceated as a Homework for Data Science course at American University of Armenia."),
                     tags$p("It intends to discover board games from all over the world including famous games as Chess, Bridge and so on."),
                     tags$p("The data was taken from", tags$a("Kaggle website.", href = "https://www.kaggle.com/mrpantherson/board-game-data", target = "_blank"), "Then it was used to create graph to explore the games and served as a database for Search tab."),
                     tags$p("The third tab was the initial idea for the app: try create a game with R. Game Rules can be found in", tags$i("Play here"), "tab."),
                     lapply(1:5, function(x) { br() } ),
                     tags$p("Instructor:", tags$b("Habet Madoyan")),
                     tags$p("TA:", tags$b("Karen Mkhitaryan")),
                     br(),
                     tags$p("Student:", tags$b("Liana Harutyunyan"))
                   ), br(), br()),
                   column(3)) 
  
  
  
  ui <- fluidPage(title = "Board Games",
                  theme = shinytheme('slate'),
                  
                  tags$head(
                    tags$style(HTML("
                                    @import url('//fonts.googleapis.com/css?family=Rancho&effect=shadow-multiple');
                                    ")),
                    tags$body(style = 'font-family: Rancho; font-size: 20px')
                    
                    ),
                  
                  
                  conditionalPanel("input.enter_app == 0",
                                   fluidRow(
                                            column(6, tags$img(src = img1,
                                                               width = 600, height = 650,
                                                               style = 'margin-top: 50px; margin-left: 50px')),
                                            column(4, 
                                                   br(), textOutput('intro'), style = ' font-size: 76px; margin-top: 50px; margin-left: 60px'
                                            ),
                                            column(2),
                                            actionButton("enter_app", "Next >>", style = 'font-size: 40px !important; margin-top: 10px; margin-left: 200px')
                                            
                                   ), br()
                  ),
                  
                  conditionalPanel("input.enter_app == 1",
                                   tabBox(width = 12,
                                          tab1, tab2, tab3, tab4)
                  )
  )
  
  
  server <- function(input, output) {
    
    output$intro <- renderText({
      paste("Did you know there are more than 5000 Board Games?")
    })
    
    # tab1
    output$intro2 <- renderText({
      paste0("<p style = \"font-size: 25px\"> First, let us explore the Database we have. ",
            "You can choose for which statistic you want to see the Top Games. </p> ",
            "<p> i.e. Want to see which game to take with you to journey, if you go with 4 friends? And also want to take a game with highest rating? </p>",
            "<p> Select <i> Average Rating  </i> with <i> Highest  </i> results with <i> 2 Minimum Players </i> and <i> 4 Maximum Players. </i> </p>")
    })
    
    tab1_results <- reactive({
      filtered <- bgg %>%
        filter(min_players >= as.numeric(input$min_player) & 
                 max_players <= as.numeric(input$max_player))
      if (input$lowvshigh == "Highest") {
        results <- head(arrange(filtered, desc(get(input$statistic))), n = input$top_x) %>%
          select(names, input$statistic)
        results$Top <- 1:input$top_x
      } else {
        results <- tail(arrange(filtered, desc(get(input$statistic))), n = input$top_x) %>%
          select(names, input$statistic)
        results$Top <- input$top_x:1
      }
      results[, c(3, 2, 1)]
    })
    
    tab1_results_coef <- reactive({
      ifelse(input$lowvshigh == "Highest", -1, 1)
    })
    
    output$best_stat <- renderPlot({
      ggplot(tab1_results(),
             aes(x = reorder(Top, tab1_results_coef()*get(input$statistic)), y = get(input$statistic))) + 
        geom_bar(stat = "identity") + 
        theme(panel.border = element_rect(fill = NA, color = '#272B30'),
              rect = element_rect(fill = '#272B30'),
              panel.background = element_rect(fill = '#272B30'),
              axis.text = element_text(color = 'gray70', size = 14),
              panel.grid.major = element_line(color = 'gray40', linetype = 'dotted'),
              panel.grid.minor = element_line(color = 'gray40', linetype = 'dotted'),
              title = element_blank())
    })
    
    output$results <- renderTable({
      tab1_results()
    })
    
    # tab2
    output$help_panel <- renderText({
      paste("<p style = \"font-size: 34px\"> Help Panel </p>",
            "* See which games you can Search.")
    })
    
    output$tab2_game_found <- renderText({
      found <- bgg %>%
        filter(tolower(input$tab2_search) == tolower(names))
      if(is.na(found[1, 1])) {
        paste("Sorry, the game <b>", toupper(input$tab2_search), "</b> is not found in the database.",
              "Try another one from the lists from the right Help Panel.")
      } else {
        paste("<div align = \"center\"> <img src =", found$image_url, "height = 400>", "<br>",
            "<b style = \"font-size: 28px \"> Name: </b>", found$names, "<br>",
            "<b style = \"font-size: 28px \"> Category: </b>", found$category, "<br>",
            "<b style = \"font-size: 28px \"> Year: </b>", found$year, "<br>",
            "<b style = \"font-size: 28px \"> Designer: </b>", found$designer, "<br>",
            "<b style = \"font-size: 28px \"> Average Rating: </b>", found$avg_rating, "<br>",
            "<b style = \"font-size: 28px \"> Geek Rating: </b>", found$geek_rating, "<br>",
            "<b style = \"font-size: 28px \"> Owned: </b>", found$owned, "<br>",
            "<b style = \"font-size: 28px \"> Players (min - max): </b>", paste0(found$min_players, " - ", found$max_players), "<br>",
            "<b style = \"font-size: 28px \"> Average Time: </b>", found$avg_time, "<br>",
            "<b style = \"font-size: 28px \"> Skills Needed: </b>", found$mechanic, "<br>",
            "<b style = \"font-size: 28px \"> Weight: </b>", found$weight, "<br>",
            "<b style = \"font-size: 28px \"> For More Info: </b> <a target='_blank' href = ", found$bgg_url, "> CLICK HERE </a>", "<br>",
            "</div>")
      }
    })
    
    # tab3
    
    show_bombs <- F
    
    output$control_game <- renderText({
      paste("<p style = \"font-size: 34px\"> Control Game </p>",
            "* Set the level and see if the game is fair by enabling <i> Show bombs. </i>")
    })
    
    game <- reactiveValues(coords = NULL,
                           message = "",
                           show_bombs = F)
    
    observeEvent(input$game_levels, {
      game$coords <- rbind(data.frame('x' = 0, 'y' = 0, 'type' = "player"),
                           data.frame('x' = sample(10, as.numeric(input$game_levels) * 5, replace = T),
                                      'y' = sample(10, as.numeric(input$game_levels) * 5, replace = T),
                                      'type' = 'bombs'))
      game$show_bombs <- F
    })
    
    observeEvent(input$move_player, {
      if (input$move_player == '1') {
        game$coords[1, 'y'] <- game$coords[1, 'y'] + 1
      } else if (input$move_player == '2') {
        game$coords[1, 'y'] <- game$coords[1, 'y'] - 1
      } else if (input$move_player == '3') {
        game$coords[1, 'x'] <- game$coords[1, 'x'] + 1
      } else if (input$move_player == '4') {
        game$coords[1, 'x'] <- game$coords[1, 'x'] - 1
      } else if (input$move_player == '5') {
        game$coords[1, 'y'] <- game$coords[1, 'y'] + 2
      } else if (input$move_player == '6') {
        game$coords[1, 'y'] <- game$coords[1, 'y'] - 2
      } else if (input$move_player == '7') {
        game$coords[1, 'x'] <- game$coords[1, 'x'] + 2
      } else if (input$move_player == '8') {
        game$coords[1, 'x'] <- game$coords[1, 'x'] - 2
      } 
      
      bomb_x <- game$coords[, 1][-1] 
      bomb_y <- game$coords[, 2][-1] 
      m_x <- match(game$coords[1, 'x'], bomb_x)
      m_y <- match(game$coords[1, 'y'], bomb_y)
      if ((!is.na(m_x) && !is.na(m_y)) && m_x == m_y) {
        game$message = "GAME OVER. Try Again."
      } else if ((game$coords[1, 'x'] > 10 || game$coords[1, 'x'] < 0) || 
                 (game$coords[1, 'y'] > 10 || game$coords[1, 'y'] < 0)){
        game$message = "GAME OVER. Try Again."
      } else {
        game$message = "You are still alive."
      }
      
      if (game$coords[1, 'x'] == 10 && game$coords[1, 'y'] == 10) {
        game$show_bombs <- T
        game$message = "YOU WOOON !!!"
      }
      
      if (game$message == "GAME OVER. Try Again.") {
        game$coords <- rbind(data.frame('x' = 0, 'y' = 0, 'type' = "player"),
                             data.frame('x' = sample(9, as.numeric(input$game_levels) * 5, replace = T),
                                        'y' = sample(9, as.numeric(input$game_levels) * 5, replace = T),
                                        'type' = 'bombs'))
      }
      
    })
    
    
    output$coords_table <- renderTable({
      game$coords
    })
    
    output$game_board <- renderPlot({
      if(input$show_bombs || game$show_bombs) {
        to_be_ploted <- game$coords
      } else {
        to_be_ploted <- game$coords[1, ]
      }
      ggplot(to_be_ploted, aes(x = x, y = y, color = type, shape = type)) + geom_point(size = 7) + 
        scale_color_manual(values = c("gainsboro", "firebrick3")) + 
        scale_shape_manual(values = c(as.numeric(input$character), 13)) + 
        coord_cartesian(xlim=c(0, 10), ylim=c(0, 10)) + 
        scale_x_continuous(breaks = seq(0, 10, 1)) + 
        scale_y_continuous(breaks = seq(0, 10, 1)) + 
        theme(panel.border = element_rect(fill = NA, color = '#272B30'),
              rect = element_rect(fill = '#272B30'),
              panel.background = element_rect(fill = '#272B30'),
              axis.text = element_text(color = 'gray70', size = 14),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = 'gray40', linetype = 'dotted'),
              title = element_blank(),
              legend.position="none")
    })
    
    output$game_title <- renderText({
      paste("<p style = \"font-size: 98px\"; align = \"center\"> Luck and Intuition </p>") 
    })
    
    observeEvent(input$game_rules, {
      showModal(modalDialog(easyClose = T, footer = modalButton("I want to Play"),
        tags$p("GAME RULES", style = "font-size: 40px; text-align: center"),
        tags$p("The game is named ", tags$b("Luck and Intuition"), " as there is no special technique to win the game and the main skills to rely on while playing are", tags$b("Luck and Intuition.")),
        tags$p("The game board is covered with bombs. In order to win the player needs to reach the coordinate (10; 10) without steping on any bomb and he/she will WIN."),
        tags$p("If the player suspects that the game is not fair, he/she can always enable showing bombs and win unfairly."),
        tags$p("If the player steps on a bomb, the message says that Game was Over and game starts from the beginning immediately."),
        tags$p("If the player steps outside from the board, in this case also he/she loses and game starts from the beginning immediately."),
        tags$p("If the player wins or wants to start the game from the beginning, he/she needs to reset the level to be able to do that."),
        tags$p("In each level there are not more than ", tags$i("level number * 5"), " bombs.")
              
      ))
    })
    
    output$game_message <- renderText({
      paste("<p style = \"font-size: 60px\"; align = \"center\">", game$message, "</p>") 
    })
    
    
  }
  
  
  shinyApp(ui=ui, server=server)
