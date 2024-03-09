substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

create_num_penalties <- function(database){
  # Add the num of penalties and cumsum to the database
  Num_penalties = database %>% 
    group_by(Player) %>% 
    summarize(Num_penalties = n()) %>%
    arrange(desc(Num_penalties)) %>%
    mutate(Percent = Num_penalties / sum(Num_penalties) * 100,
           cum_Percent = cumsum(Percent),
           consider_fe = ifelse(cum_Percent <= 81, Player, "nan"))
  return(Num_penalties)
}

process_database <- function(database){
  database <- as.data.frame(database)
  database = database %>% rename(any_of(c("player_position" = "player position", "foot" = "foot", "age" = "age", "greak_gk" = "Great GK?", "height_gk" = "Height of GK",
                                          "location" = "Location (H-A-N)", "round" = "Round Number", "competition_grouped" = "competition grouped", "Importantness_Game" = "Importantness Game",
                                          "Penalty_type" = "Ingame-Shootout?", "lead_deficit" = "Lead-Deficit", "minute_pars" = "Minute Pars", "decider" = "Decider?",
                                          "shot_hard" = "Schuss hart ja nein", "ball_placed" = "Platziert?", "gk_stand" = "GK Stand", "sort_of_movement" = "Sort of Movement",
                                          "tapped_the_ball" = "tapped the ball?", "weeks_since_last_penalty" = "in weeks")))
  return(database)
}

process_database2 <- function(database, Num_penalties, mode_val){
  database <- merge(database, Num_penalties[c('Player', 'consider_fe')], by.x="Player", by.y="Player")
  
  # replace the missing values with the mode
  database <- database %>% mutate(shot_hard = ifelse(is.na(shot_hard), mode_val, shot_hard), 
                                  ball_placed = ifelse(is.na(ball_placed), mode_val, ball_placed)
  )
  
  # Replace values in the "Category" column
  database$round <- replacement_map[database$round]
  
  # Convert categorical columns to numeric columns
  database[numerical_cols] <- sapply(database[numerical_cols], as.numeric)
  
  # apply string replacement to selected columns
  database <- database %>%
    mutate(across(any_of(categorical_cols), ~ str_replace_all(., " ", "_")))
  
  categorical_cols = intersect(categorical_cols, colnames(database))
  
  # Create dummy variable
  database <- dummy_cols(database,
                         select_columns = c(categorical_cols),
                         remove_first_dummy = FALSE,
                         remove_selected_columns = TRUE)
  return(database)
}

process_database3 <- function(database, cols_reference){
  # Columns to be removed
  if(length(cols_reference) > 0) {
    database <- database[, -which(names(database) %in% cols_reference)]
  }
  return(database)
}

add_interactions <- function(database){
  for(i in c("R_slightly_left", "R_slightly_right", "R_clearly_right", "R0", "R_clearly_left")){
    database[paste("R_", sub("R_|R", "", i), sep="")] = database["foot_R"] * database[paste("gk_stand_", sub("R_|R", "", i), sep="")]
  }
  return(database)
}

convertInputsToDataFrame <- function(input) {
  dataframe <- data.frame(
    Player = input$Player,
    round = input$round,
    # minute_pars = input$minute_pars,
    competition_grouped = input$competition_grouped,
    gk_stand = input$gk_stand,
    shot_hard = input$shot_hard,
    ball_placed = input$ball_placed,
    tapped_the_ball = input$tapped_the_ball,
    Penalty_type = input$Penalty_type
  )
  return(dataframe)
}
