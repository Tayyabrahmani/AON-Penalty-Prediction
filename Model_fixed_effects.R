#How to implement a model in apollo and minimize the Log-likelihood Function
library(tidyverse)
library(conflicted)
library(fastDummies)
library(readxl)
library(dplyr)
library(apollo)							# run apollo package
apollo_initialise()
source("utils/utils_func.R")

categorical_cols = c("consider_fe", "player_position", "foot", "round", "competition_grouped", "Penalty_type",
                     "minute_pars", "shot_hard", "ball_placed", "gk_stand", "tapped_the_ball")

numerical_cols = c()

interaction_cols = c(c("foot", "gk_stand"))

cols_reference = c("consider_nan", "player_position_GK", "player_position_DF", "foot_L", "greak_gk_no", "location_N", "round_-", "competition_grouped_Friendly", "Importantness_Game_1",
                   "Penalty_type_Shootout", "minute_pars_-", "decider_no", "shot_hard_no", "ball_placed_no", "ball_placed_NA", 
                   "gk_stand_central", "sort_of_movement_-", "sort_of_movement_still", "tapped_the_ball_no")
 
cols_with_same_beta = c()

# Define the replacement mapping
replacement_map <- c('Final'= 'Final', 'Quarter-Final'= 'Quarter-Final', "2"= 'Group', '-'= '-', 'Group'= 'Group', 'last sixteen'= 'Group',
                     'Semi-Final'= 'Semi-Final', "3"= 'Group', "4"= 'Group', "1"= 'Group', 'last thirty-two'= 'Group', 'KO'= 'KO', "5"= 'Group',
                     'match place 3'= 'match place 3')

# Load the database
database <- read_excel("Input_data/SixAlt.xlsx")
database = process_database(database)
Num_penalties = create_num_penalties(database)

# calculate the mode of the "shot_hard" column
mode_val <- names(which.max(table(database$shot_hard)))

database = process_database2(database, Num_penalties, mode_val)

# Concatenating categorical and numerical columns
cols_to_select = c(categorical_cols, numerical_cols)
database = process_database3(database, cols_reference)

database = add_interactions(database)

#set some controls
apollo_control=list(modelName = "Final Model",
                  modelDescr = "Player position and foot model", indivID="ID",
                  outputDirectory="Output")

# Choice mapping
choice_map = c("1"="TL", "2"="TC", "3"="TR", "4"="BL", "5"="BC", "6"="BR")

#Define name and starting values for the coefficients to be estimated
K <- grep(paste(cols_to_select, collapse = "|"), names(database), value=TRUE)
n <- 6
K = c(K, c("R_slightly_left", "R_slightly_right", "R_clearly_right", "R_0", "R_clearly_left"))
K <- paste0(rep(K, each = n-1), rep(2:n, times = length(K)))
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
                           apollo_inputs)

apollo_modelOutput(BaseSpec)
apollo_saveOutput(BaseSpec)
