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

cols_to_select = c("lead_deficit", "minute_pars")
cols_to_remove = c("lead_deficit_0", "minute_pars_-")
cols_with_same_beta = c("NONE")

# apply string replacement to selected columns
database <- database %>%
  mutate(across(all_of(cols_to_select), ~ str_replace_all(., " ", "_")))

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
                    modelDescr="Player position and foot model", indivID="ID")


# Choice mapping
choice_map = c("1"="TL", "2"="TC", "3"="TR", "4"="BL", "5"="BC", "6"="BR")

#Define name and starting values for the coefficients to be estimated
K <- grep(paste(cols_to_select, collapse = "|"), names(database), value=TRUE)
n <- 6
cols_with_same_beta <- grep(paste(cols_with_same_beta, collapse = "|"), names(database), value=TRUE)
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


BaseSpec = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(BaseSpec)

L[[2]]<-BaseSpec

Modelnames<-c("test1")