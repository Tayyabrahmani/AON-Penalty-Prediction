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
database = rename(database, "player_position" = "player position", "gk_stand" = "GK Stand", "sort_of_movement" = "Sort of Movement",
"competition_grouped" = "competition grouped", "Importantness_Game" = "Importantness Game", "lead_deficit" = "Lead-Deficit", "minute_pars" = "Minute Pars",
"location" = "Location (H-A-N)", "Penalty_type" = "Ingame-Shootout?", "decider" = "Decider?", "shot_hard" = "Schuss hart ja nein",
"greak_gk" = "Great GK?")

# Add the num of penalties and cumsum to the database
Num_penalties = database %>% 
  group_by(Player) %>% 
  summarize(Num_penalties = n()) %>%
  arrange(desc(Num_penalties)) %>%
  mutate(Percent = Num_penalties / sum(Num_penalties) * 100,
         cum_Percent = cumsum(Percent),
        consider_fe = ifelse(cum_Percent <= 81, Player, "nan"))

database <- merge(database, Num_penalties[c('Player', 'consider_fe')], by.x="Player", by.y="Player")

cols_to_select = c("consider_fe", "location", "lead_deficit", "minute_pars", "competition_grouped",
                   "Importantness_Game", "gk_stand", "sort_of_movement", "Penalty_type", "decider", "shot_hard", "greak_gk")
cols_to_remove = c("consider_nan", "location_N", "lead_deficit_0",
                   "minute_pars_-", "competition_grouped_Friendly", "Importantness_Game_1",
                   "gk_stand_central", "sort_of_movement_-", "sort_of_movement_still", "Penalty_type_Shootout", "decider_no", "shot_hard_no", "greak_gk_no")
 
# cols_to_select = c("location", "lead_deficit", "minute_pars", "competition_grouped", 
#                    "Importantness_Game", "gk_stand", "sort_of_movement", "Penalty_type", "decider", "shot_hard", "greak_gk")
# cols_to_remove = c("location_N", "lead_deficit_0",
#                    "minute_pars_-", "competition_grouped_Friendly", "Importantness_Game_1",
#                    "gk_stand_central", "sort_of_movement_-", "sort_of_movement_still", "Penalty_type_Shootout", "decider_no", "shot_hard_no", "greak_gk_no"
#                    )
cols_with_same_beta = c("lead_deficit", "minute_pars", "competition_grouped", "Importantness_Game")

# calculate the mode of the "shot_hard" column
mode_val <- names(which.max(table(database$shot_hard)))

# replace the missing values with the mode
database <- database %>% mutate(shot_hard = ifelse(is.na(shot_hard), mode_val, shot_hard))

# Remove lead deficit from the categorical columns
if( "lead_deficit" %in% cols_to_select){
    database$lead_deficit = as.numeric(database$lead_deficit)
    cols_to_select = setdiff(cols_to_select, c("lead_deficit"))
}

# apply string replacement to selected columns
database <- database %>%
  mutate(across(all_of(cols_to_select), ~ str_replace_all(., " ", "_")))

cols_to_select = c(cols_to_select, c("lead_deficit"))

# Create dummy variable
database <- dummy_cols(database,
                       select_columns = c(cols_to_select),
                       remove_first_dummy = FALSE,
                       remove_selected_columns = TRUE)

# Columns to be removed
if(length(cols_to_remove) > 0) {
  database <- database[, -which(names(database) %in% cols_to_remove)]
}

#set some controls
apollo_control=list(modelName="Naive Model PSP",
                    modelDescr="Player position and foot model", indivID="ID",
                    nCores = 4)


# Choice mapping
choice_map = c("1"="TL", "2"="TC", "3"="TR", "4"="BL", "5"="BC", "6"="BR")

#Define name and starting values for the coefficients to be estimated
K <- grep(paste(cols_to_select, collapse = "|"), names(database), value=TRUE)
n <- 6
cols_with_same_beta <- grep(paste(cols_with_same_beta, collapse = "|"), names(database), value=TRUE)
### TODO:
database = select(database, K, Choice, ID)

K <- setdiff(K, cols_with_same_beta)
K <- paste0(rep(K, each = n-1), rep(2:n, times = length(K)))
K = c(K, cols_with_same_beta)
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

  choice_map = c("1"="TL", "2"="TC", "3"="TR", "4"="BL", "5"="BC", "6"="BR")
  cols_to_select = c("consider_fe", "location", "lead_deficit", "minute_pars", "competition_grouped",
                   "Importantness_Game", "gk_stand", "sort_of_movement", "Penalty_type", "decider", "shot_hard", "greak_gk")
  print(cols_to_select)
  print(names(database))
  K <- grep(paste(cols_to_select, collapse = "|"), names(database), value=TRUE)
  print(K)
  # n <- 6
  # cols_with_same_beta <- grep(paste(cols_with_same_beta, collapse = "|"), names(database), value=TRUE)
  # ### TODO:
  # database = select(database, K, Choice, ID)

  # K <- setdiff(K, cols_with_same_beta)
  # K <- paste0(rep(K, each = n-1), rep(2:n, times = length(K)))
  # K = c(K, cols_with_same_beta)


  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities

  # Add choice make characteristics
  for(j in 1:length(unique(Choice))){
    if (j==1){
      V[[paste0("alt_", choice_map[j])]] = 0
    } else {
      V[[paste0("alt_", choice_map[j])]] = get(paste0("b_asc_", choice_map[j]))
        for(k in 1:length(K)) {
            if (K[k] %in% cols_with_same_beta) {
            V[[paste0("alt_", choice_map[j])]] = V[[paste0("alt_", choice_map[j])]] +
          get(paste0("b_", K[k]))*get(K[k])}
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

# apollo_outOfSample(
# apollo_beta,
# apollo_fixed,
# apollo_probabilities,
# apollo_inputs,
# estimate_settings = list(estimationRoutine = "bfgs", maxIterations = 200, writeIter =
# FALSE, hessianRoutine = "none", printLevel = 3L, silent = TRUE),
# outOfSample_settings = list(nRep = 10, validationSize = 0.1, samples = NA, rmse = NULL)
# )

BaseSpec = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs,
                           estimate_settings = list(maxIterations=50))

apollo_modelOutput(BaseSpec)
# apollo_saveOutput(BaseSpec)

# L<-NULL
# L[[1]]<-BaseSpec


# Modelnames<-c("test1")
