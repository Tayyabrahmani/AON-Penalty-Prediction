#How to implement a model in apollo and minimize the Log-likelihood Function
library(tidyverse)
library(conflicted)
library(fastDummies)
library(readxl)
library(dplyr)
library(apollo)							# run apollo package
apollo_initialise()

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

database <- read_excel("SixAlt.xlsx")
database <- as.data.frame(database)
database = rename(database, "player_position" = "player position", "foot" = "foot", "age" = "age", "greak_gk" = "Great GK?", "height_gk" = "Height of GK",
"location" = "Location (H-A-N)", "round" = "Round Number", "competition_grouped" = "competition grouped", "Importantness_Game" = "Importantness Game",
"Penalty_type" = "Ingame-Shootout?", "lead_deficit" = "Lead-Deficit", "minute_pars" = "Minute Pars", "decider" = "Decider?",
"shot_hard" = "Schuss hart ja nein", "ball_placed" = "Platziert?", "gk_stand" = "GK Stand", "sort_of_movement" = "Sort of Movement",
"tapped_the_ball" = "tapped the ball?", "weeks_since_last_penalty" = "in weeks") 

# Add the num of penalties and cumsum to the database
Num_penalties = database %>% 
  group_by(Player) %>% 
  summarize(Num_penalties = n()) %>%
  arrange(desc(Num_penalties)) %>%
  mutate(Percent = Num_penalties / sum(Num_penalties) * 100,
         cum_Percent = cumsum(Percent),
        consider_fe = ifelse(cum_Percent <= 81, Player, "nan"))

database <- merge(database, Num_penalties[c('Player', 'consider_fe')], by.x="Player", by.y="Player")

# "consider_fe",
categorical_cols = c("player_position", "foot", "greak_gk", "location", "round", "competition_grouped", "Importantness_Game", "Penalty_type",
                     "minute_pars", "decider", "shot_hard", "ball_placed", "gk_stand", "sort_of_movement", "tapped_the_ball")

numerical_cols = c("age", "height_gk", "lead_deficit", "weeks_since_last_penalty")

interaction_cols = c(c(), c(), c(), c(), c(), c(), c(), c())

cols_to_remove = c("consider_nan", "player_position_GK", "player_position_DF", "foot_L", "greak_gk_no", "location_N", "round_-", "competition_grouped_Friendly", "Importantness_Game_1",
                   "Penalty_type_Shootout", "minute_pars_-", "decider_no", "shot_hard_no", "ball_placed_no", "ball_placed_NA", 
                   "gk_stand_central", "sort_of_movement_-", "sort_of_movement_still", "tapped_the_ball_no")
 
cols_with_same_beta = c()

# calculate the mode of the "shot_hard" column
mode_val <- names(which.max(table(database$shot_hard)))

# replace the missing values with the mode
database <- database %>% mutate(shot_hard = ifelse(is.na(shot_hard), mode_val, shot_hard), 
                                ball_placed = ifelse(is.na(ball_placed), mode_val, ball_placed)
                                )

# Define the replacement mapping
replacement_map <- c('Final'= 'Final', 'Quarter-Final'= 'Quarter-Final', "2"= 'Group', '-'= '-', 'Group'= 'Group', 'last sixteen'= 'Group',
                     'Semi-Final'= 'Semi-Final', "3"= 'Group', "4"= 'Group', "1"= 'Group', 'last thirty-two'= 'Group', 'KO'= 'KO', "5"= 'Group',
                     'match place 3'= 'match place 3')

# Replace values in the "Category" column
database$round <- replacement_map[database$round]

# Convert categorical columns to numeric columns
database[numerical_cols] <- sapply(database[numerical_cols], as.numeric)

# apply string replacement to selected columns
database <- database %>%
  mutate(across(all_of(categorical_cols), ~ str_replace_all(., " ", "_")))

# Create dummy variable
database <- dummy_cols(database,
                       select_columns = c(categorical_cols),
                       remove_first_dummy = FALSE,
                       remove_selected_columns = TRUE)

# Concatenating categorical and numerical columns
cols_to_select = c(categorical_cols, numerical_cols)

# Columns to be removed
if(length(cols_to_remove) > 0) {
  database <- database[, -which(names(database) %in% cols_to_remove)]
}

list_cols_to_select <- lapply(seq_along(cols_to_select), function(i) cols_to_select[1:i])

for(cols_to_select in list_cols_to_select)
{  #set some controls
  dir.create(paste("Model_Output/Model", length(cols_to_select)))
  apollo_control=list(modelName = paste("Model", length(cols_to_select)),
                      modelDescr = "Player position and foot model", indivID="ID",
                      outputDirectory = paste("Model_Output/Model", length(cols_to_select)))

  # Choice mapping
  choice_map = c("1"="TL", "2"="TC", "3"="TR", "4"="BL", "5"="BC", "6"="BR")

  #Define name and starting values for the coefficients to be estimated
  K <- grep(paste(cols_to_select, collapse = "|"), names(database), value=TRUE)
  n <- 6
  # cols_with_same_beta <- grep(paste(cols_with_same_beta, collapse = "|"), names(database), value=TRUE)
  # K <- setdiff(K, cols_with_same_beta)
  K <- paste0(rep(K, each = n-1), rep(2:n, times = length(K)))
  # K = c(K, cols_with_same_beta)
  apollo_beta_constants = paste0("asc_", choice_map[sort(unique(database$Choice))[-1]])
  apollo_beta_constants <- c(K, apollo_beta_constants)
  apollo_beta = setNames(rep(0,length(apollo_beta_constants)),paste0("b_", apollo_beta_constants))

  #all coefficients may be altered, none is fixed
  apollo_fixed=c()

  #check if you have defined everything necessary 
  apollo_inputs = apollo_validateInputs()

  apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
    on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit
    
    P = list()								 ### Create list of probabilities P
    
    V = list()								 ### List of utilities

    # Add choice make characteristics
    for(j in 1:length(unique(Choice))){
      if (j==1){
        V[[paste0("alt_", choice_map[j])]] = 0
      } else {
        V[[paste0("alt_", choice_map[j])]] = get(paste0("b_asc_", choice_map[j]))
        for(k in 1:length(K)) {
            if (substr(K[k], 0, nchar(K[k])-1) %in% numerical_cols) {
            V[[paste0("alt_", choice_map[j])]] = V[[paste0("alt_", choice_map[j])]] +
          get(paste0("b_", K[k]))*get(substr(K[k], 0, nchar(K[k])-1))}
            else if (substrRight(K[k], 1) == j){
            V[[paste0("alt_", choice_map[j])]] = V[[paste0("alt_", choice_map[j])]] +
          get(paste0("b_", K[k]))*get(sub(j, "", K[k]))} 
            }
        }
    }

    mnl_settings = list(						       ### Define settings for model 
      alternatives = c(alt_TL=1, alt_TC=2, alt_TR=3, alt_BL=4, alt_BC=5, alt_BR=6),					 ### component
      avail        = 1,
      choiceVar    = Choice,
      V            = V)
    
    P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
    
    P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
    ### for same ID
    
    P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
    
    return(P)
  }

  BaseSpec = apollo_estimate(apollo_beta,
                            apollo_fixed,
                            apollo_probabilities,
                            apollo_inputs,
                            estimate_settings = list(maxIterations=100))

  apollo_modelOutput(BaseSpec)
  apollo_saveOutput(BaseSpec)
}
# L<-NULL
# L[[1]]<-BaseSpec

# for(i in seq(16, 21))
# {print(apollo_lrTest(apollo_loadModel(paste("Model_Output/Model ", i,"/Model ", i, sep="")), apollo_loadModel(paste("Model_Output/Model ", i + 1,"/Model ", i + 1, sep=""))))}
