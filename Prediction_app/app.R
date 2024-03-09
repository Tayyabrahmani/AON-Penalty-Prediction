library(shiny)
library(shinythemes)
library(readxl)
library(tidyverse)
library(fastDummies)
library(dplyr)
library(apollo)
library(shinyjs)
source("utils/forecast_function.R")
source("utils/utils_func.R")

replacement_map <- c('Final'= 'Final', 'Quarter-Final'= 'Quarter-Final', "2"= 'Group', '-'= '-', 'Group'= 'Group', 'last sixteen'= 'KO',
                     'Semi-Final'= 'Semi-Final', "3"= 'Group', "4"= 'Group', "1"= 'Group', 'last thirty-two'= 'Group', 'KO'= 'KO', "5"= 'Group',
                     'match place 3'= 'match place 3')

categorical_cols = c("consider_fe", "player_position", "foot", "round", "competition_grouped", "Penalty_type",
                     "minute_pars", "shot_hard", "ball_placed", "gk_stand", "tapped_the_ball")

numerical_cols = c()

interaction_cols = c(c("foot", "gk_stand"))

cols_reference = c("consider_nan", "player_position_GK", "player_position_DF", "foot_L", "greak_gk_no", "location_N", "round_-", "competition_grouped_Friendly", "Importantness_Game_1",
                   "Penalty_type_Shootout", "minute_pars_-", "decider_no", "shot_hard_no", "ball_placed_no", "ball_placed_NA",
                   "gk_stand_central", "sort_of_movement_-", "sort_of_movement_still", "tapped_the_ball_no")

cols_with_same_beta = c()


# Define the UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/csas/bootstrap.min.css"
    ),
    tags$style(
      HTML("
        .goal-post {
          position: fixed;
          width: 75vw;
          height: 55vh;
          border: 10px solid #000; /* Increased border thickness to 4px */
          border-bottom: none;
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          grid-template-rows: repeat(2, 1fr);
          gap: 10px;
          padding: 10px;
          box-shadow: 0 0 10px rgba(0, 0, 0, 0.2);
          background-color: #FFF;
          margin-left: 250px;
        }


        .goal-box {
          background-color: #FFF;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 16px;
          font-weight: bold;
          border: 2px solid #000;
        }

        .highlight {
          background-color: #FFD700;
        }

        .dropdown-panels {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          margin-bottom: 20px;
          background-color: #FFCC99;
          padding: 15px;
          overflow: auto;
          position: fixed; 
          top: 0;
          left: 0;
          width: 100%;
        }

        .main-content {
          margin-top: 60px;
        }
        
        /* Add custom styles for sidebar panel */
        .sidebar {
          width: 200px;
        }
      ")
    )
  ),
  
  navbarPage(
    theme = shinytheme("united"),
    title = "Penalty Kick Prediction",
    tags$style(HTML("
    .nav.navbar-nav {
      display: flex;
      flex-direction: row;
    }
  ")),      
    tabPanel(
      "Home",
      sidebarPanel(
        style = "display: flex; justify-content: center; align-items: center; width: 95vw; height: 85vh;",
        div(
          style = "display: flex; justify-content: center; align-items: center; width: 100%; height: 100%;",
          div(
            style = "max-width: 90vw; max-height: 85vh; overflow: auto;",
            img(
              src = "https://www.telegraph.co.uk/content/dam/world-cup/2022/12/06/TELEMMGLPICT000318886724_trans_NvBQzQNjv4BqFkh0sZHwxQaed0GEjan5yWZ8Qxj2mbySrADHD946rg0.jpeg",
              style = "width: 90vw; height: 80vh; object-fit: contain;"
            )
          )
        )
      )
    ),
    tabPanel(
      "Prediction",
      fluidPage(
        fluidRow(
          style = "margin-left: 100px;",
          column(
            width = 2,
            offset = 1,
            selectizeInput("Player", "Player", choices = NULL),
            selectizeInput("round", "Round", choices = NULL),
          ),
          column(
            width = 2,        
            selectizeInput("player_position", "Player Position", choices = NULL),
            selectizeInput("Penalty_type", "Penalty Type", choices = NULL),
          ),
          column(  
            width = 2,
            selectizeInput("foot", "Player Foot", choices = NULL),
            selectizeInput("tapped_the_ball", "Is the ball tapped", choices = NULL),
          ),
          column(
            width = 2,
            selectizeInput("competition_grouped", "Competition", choices = NULL),
            selectizeInput("gk_stand", "GK_stand", choices = NULL),
          ),
          column(
            width = 2,
            selectizeInput("shot_hard", "Is the shot Powerful", choices = NULL),
            selectizeInput("ball_placed", "Is the ball placed", choices = NULL),
          ),
          column(
            width = 8,
            offset = 2,
            align = "center",
            actionButton("predict_btn", "Predict", class = "btn btn-primary btn-lg")
          )
        ),

        fluidRow(
          column(
            width = 10,
            push = 12,
            h3("   "),
          div(class = "goal-post",
              div(class = "goal-box", id = "TL", "TL"),
              div(class = "goal-box", id = "TC", "TC"),
              div(class = "goal-box", id = "TR", "TR"),
              div(class = "goal-box", id = "BL", "BL"),
              div(class = "goal-box", id = "BC", "BC"),
              div(class = "goal-box", id = "BR", "BR")
          )),
        )
      )
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  # Read the data from the Excel file
  data <- reactive({
    req(file.exists("Input_data/SixAlt.xlsx"))
    database <- read_excel("Input_data/SixAlt.xlsx")
    database = process_database(database)
    Num_penalties = create_num_penalties(database)
    mode_val <- names(which.max(table(database$shot_hard)))
    database1 = process_database2(database, Num_penalties, mode_val)
    database1 = process_database3(database1, cols_reference)

    v1 <- colnames(database1)
    return(list(database = database, Num_penalties = Num_penalties, mode_val = mode_val, v1=v1))
  })

  getPlayerInfo <- function(player_name) {
    player_info = data()$database[c("Player", "player_position", "foot")]
    player_info = unique(player_info)
    info <- player_info[player_info$Player == player_name, c("player_position", "foot")]
    return(info)
  }
  
  # Update the select inputs based on the data
  observeEvent(data()$database, {
    updateSelectizeInput(session, "Player", choices = unique(data()$database$Player))
    updateSelectizeInput(session, "round", choices = c(setdiff(unique(data()$database$round), c("1", "2", "3", "4", "5", "last thirty-two", "last sixteen")), "Group"))
    updateSelectInput(session, "Penalty_type", choices = unique(data()$database$Penalty_type))
    updateSelectizeInput(session, "competition_grouped", choices = unique(data()$database$competition_grouped))
    updateSelectizeInput(session, "gk_stand", choices = setdiff(unique(data()$database$gk_stand), "0"))
    updateSelectizeInput(session, "shot_hard", choices = setdiff(unique(data()$database$shot_hard), NA))
    updateSelectizeInput(session, "ball_placed", choices = setdiff(unique(data()$database$ball_placed), NA))
    updateSelectizeInput(session, "tapped_the_ball", choices = unique(data()$database$tapped_the_ball))
  })

  # Observe changes in the player input
  observeEvent(input$Player, {
    player_info <- getPlayerInfo(input$Player)
    if (!is.null(player_info)) {
      updateSelectizeInput(session, "player_position", choices = player_info$player_position)
      updateSelectizeInput(session, "foot", choices = player_info$foot)
    }
  })
  
  # Function to predict the penalty kick direction
  predictKickDirection <- function(input) {
    # Perform your prediction logic here based on the inputs and the data
    v1 = data()$v1
    test_database = convertInputsToDataFrame(input)
    test_database <- merge(test_database, unique(data()$database[c('Player', 'player_position', 'foot')]),
                           by.x="Player", by.y="Player")
    
    test_database = process_database2(test_database, data()$Num_penalties, data()$mode_val)
    test_database = process_database3(test_database, cols_reference)

    if (!all(v1 %in% colnames(test_database))) {
      # Create the missing columns with 0 as input
      missing_columns <- setdiff(v1, colnames(test_database))
      missing_columns <- setdiff(missing_columns, cols_reference)
      test_database[, missing_columns] <- 0
    }
    
    test_database = add_interactions(test_database)
    prediction_model = apollo_loadModel("Output/Final Model")
    kick_direction = forecastOutput(prediction_model, test_database)   
    return(kick_direction) 
  }
  
  # Reactively predict the kick direction when the predict button is clicked
  observeEvent(input$predict_btn, {
 
    # Call the predictKickDirection function to get the prediction
    prediction <- predictKickDirection(input)
    
    # Display the prediction in the output text
    output$prediction_output <- renderText(paste("Predicted kick direction:", prediction))
    
    # Highlight the box based on the prediction
    updateGoalBox(prediction)
    
    # Navigate to the prediction page
    updateNavbarPage(session, "Penalty Kick Prediction", selected = "Prediction")
  })
  
  # Function to update the goal box highlighting
  updateGoalBox <- function(prediction) {
    # Reset the highlighting on all goal boxes
    for (box_id in c("TL", "TC", "TR", "BL", "BC", "BR")) {
      removeCssClass(box_id, "highlight")
    }
    
    # Highlight the predicted goal box
    addCssClass(prediction, "highlight")
  }
  
  # Helper function to add a CSS class to an element
  addCssClass <- function(element_id, class_name) {
    runjs(paste0("document.getElementById('", element_id, "').classList.add('", class_name, "');"))
  }
  
  # Helper function to remove a CSS class from an element
  removeCssClass <- function(element_id, class_name) {
    runjs(paste0("document.getElementById('", element_id, "').classList.remove('", class_name, "');"))
  }
  
}

# Run the app
shinyApp(ui, server)
