library(apollo)
#### ModelPWhole ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model1",
  modelDescr ="Simple MNL model on penalty data Player Whole",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0, 
              b_poisitiondf1=0, b_poisitiondf2=0, b_poisitiondf3=0, b_poisitiondf4=0, b_poisitiondf5=0, b_poisitiondf6=0,
              b_playerfit1=0, b_playerfit2=0, b_playerfit3=0, b_playerfit4=0, b_playerfit5=0, b_playerfit6=0
              )


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_lastpenaltygoal5","b_shothardness5", "b_poisitiondf5", "b_playerfit5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_shothardness1*shothardness_cat + b_playerfit1*Playerfit + b_poisitiondf1*posdef_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_shothardness2*shothardness_cat + b_playerfit2*Playerfit + b_poisitiondf2*posdef_cat  
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_shothardness3*shothardness_cat + b_playerfit3*Playerfit + b_poisitiondf3*posdef_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_shothardness4*shothardness_cat + b_playerfit4*Playerfit + b_poisitiondf4*posdef_cat   
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_shothardness5*shothardness_cat + b_playerfit5*Playerfit + b_poisitiondf5*posdef_cat   
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_shothardness6*shothardness_cat + b_playerfit6*Playerfit + b_poisitiondf6*posdef_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayerWhole = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayerWhole)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayerWhole)
------------------------------------------------------------------------------
  #### ModelP1 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP1",
  modelDescr ="Simple MNL model on penalty data P1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL 
  V[['TC']]  = asc_TC 
  V[['TR']]  = asc_TR 
  V[['DL']]  = asc_DL 
  V[['DC']]  = asc_DC 
  V[['DR']]  = asc_DR 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer1)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer1)
------------------------------------------------------------------------------
  #### ModelP2 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP2",
  modelDescr ="Simple MNL model on penalty data P2",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5" )
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer2)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer2)
------------------------------------------------------------------------------
  #### ModelP3 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP3",
  modelDescr ="Simple MNL model on penalty data P3",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_lastpenaltygoal5" )
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_lastpenaltygoal1*lastpenaltygoal_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_lastpenaltygoal2*lastpenaltygoal_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_lastpenaltygoal3*lastpenaltygoal_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_lastpenaltygoal4*lastpenaltygoal_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_lastpenaltygoal5*lastpenaltygoal_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_lastpenaltygoal6*lastpenaltygoal_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer3)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer3)
------------------------------------------------------------------------------
  #### ModelP4 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP4",
  modelDescr ="Simple MNL model on penalty data P4",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_lastpenaltygoal5", "b_shothardness5" )
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_shothardness1*shothardness_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_shothardness2*shothardness_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_shothardness3*shothardness_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_shothardness4*shothardness_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_shothardness5*shothardness_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_shothardness6*shothardness_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer4 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer4)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer4)
------------------------------------------------------------------------------
  #### ModelP5 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP5",
  modelDescr ="Simple MNL model on penalty data P5",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_poisitiondf1=0, b_poisitiondf2=0, b_poisitiondf3=0, b_poisitiondf4=0, b_poisitiondf5=0, b_poisitiondf6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_lastpenaltygoal5", "b_shothardness5", "b_poisitiondf5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_shothardness1*shothardness_cat + b_poisitiondf1*posdef_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_shothardness2*shothardness_cat + b_poisitiondf2*posdef_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_shothardness3*shothardness_cat + b_poisitiondf3*posdef_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_shothardness4*shothardness_cat + b_poisitiondf4*posdef_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_shothardness5*shothardness_cat + b_poisitiondf5*posdef_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_shothardness6*shothardness_cat + b_poisitiondf6*posdef_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer5 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer5)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer5)
------------------------------------------------------------------------------
  #### ModelP6 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP6",
  modelDescr ="Simple MNL model on penalty data P6",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_poisitiondf1=0, b_poisitiondf2=0, b_poisitiondf3=0, b_poisitiondf4=0, b_poisitiondf5=0, b_poisitiondf6=0,
              b_playerfit1=0, b_playerfit2=0, b_playerfit3=0, b_playerfit4=0, b_playerfit5=0, b_playerfit6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_lastpenaltygoal5", "b_shothardness5", "b_poisitiondf5", "b_playerfit5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_shothardness1*shothardness_cat + b_poisitiondf1*posdef_cat + b_playerfit1*Playerfit
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_shothardness2*shothardness_cat + b_poisitiondf2*posdef_cat + b_playerfit2*Playerfit
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_shothardness3*shothardness_cat + b_poisitiondf3*posdef_cat + b_playerfit3*Playerfit
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_shothardness4*shothardness_cat + b_poisitiondf4*posdef_cat + b_playerfit4*Playerfit
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_shothardness5*shothardness_cat + b_poisitiondf5*posdef_cat + b_playerfit5*Playerfit
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_shothardness6*shothardness_cat + b_poisitiondf6*posdef_cat + b_playerfit6*Playerfit
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer6 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer6)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer6)
------------------------------------------------------------------------------
  #### ModelP7 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP7",
  modelDescr ="Simple MNL model on penalty data P7",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_playerfit1=0, b_playerfit2=0, b_playerfit3=0, b_playerfit4=0, b_playerfit5=0, b_playerfit6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_playerfit5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_playerfit1*Playerfit
  V[['TC']]  = asc_TC + b_playerfit2*Playerfit
  V[['TR']]  = asc_TR + b_playerfit3*Playerfit
  V[['DL']]  = asc_DL + b_playerfit4*Playerfit
  V[['DC']]  = asc_DC + b_playerfit5*Playerfit
  V[['DR']]  = asc_DR + b_playerfit6*Playerfit
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer7 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer7)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer7)
------------------------------------------------------------------------------
  #### ModelP8 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP8",
  modelDescr ="Simple MNL model on penalty data P8",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_playerfit1=0, b_playerfit2=0, b_playerfit3=0, b_playerfit4=0, b_playerfit5=0, b_playerfit6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_playerfit5", "b_mvmtGK5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_playerfit1*Playerfit + b_mvmtGK1*mvmtGK_cat
  V[['TC']]  = asc_TC + b_playerfit2*Playerfit + b_mvmtGK2*mvmtGK_cat
  V[['TR']]  = asc_TR + b_playerfit3*Playerfit + b_mvmtGK3*mvmtGK_cat
  V[['DL']]  = asc_DL + b_playerfit4*Playerfit + b_mvmtGK4*mvmtGK_cat
  V[['DC']]  = asc_DC + b_playerfit5*Playerfit + b_mvmtGK5*mvmtGK_cat
  V[['DR']]  = asc_DR + b_playerfit6*Playerfit + b_mvmtGK6*mvmtGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer8 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer8)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer8)
-------------------------------------------------------------------------------

----------------------------------
  #### ModelP9 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP9",
  modelDescr ="Simple MNL model on penalty data P9",
  indivID    = "ID"
)

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))
                                                      
##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_ll1=0, b_ll2=0, b_ll3=0, b_ll4=0, b_ll5=0, b_ll6=0,
              b_ml1=0, b_ml2=0, b_ml3=0, b_ml4=0, b_ml5=0, b_ml6=0,
              b_hl1=0, b_hl2=0, b_hl3=0, b_hl4=0, b_hl5=0, b_hl6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_ll5", "b_ml5", "b_hl5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_ll1*playerfitlow*status_cat + b_ml1*playerfitmedium*status_cat + b_hl1*playerfithigh*status_cat
  V[['TC']]  = asc_TC + b_ll2*playerfitlow*status_cat + b_ml2*playerfitmedium*status_cat + b_hl2*playerfithigh*status_cat
  V[['TR']]  = asc_TR + b_ll3*playerfitlow*status_cat + b_ml3*playerfitmedium*status_cat + b_hl3*playerfithigh*status_cat
  V[['DL']]  = asc_DL + b_ll4*playerfitlow*status_cat + b_ml4*playerfitmedium*status_cat + b_hl4*playerfithigh*status_cat
  V[['DC']]  = asc_DC + b_ll5*playerfitlow*status_cat + b_ml5*playerfitmedium*status_cat + b_hl5*playerfithigh*status_cat
  V[['DR']]  = asc_DR + b_ll6*playerfitlow*status_cat + b_ml6*playerfitmedium*status_cat + b_hl6*playerfithigh*status_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer9 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer9)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
apollo_saveOutput(modelPlayer9)
----------------------------------
  #### ModelP10 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP10",
  modelDescr ="Simple MNL model on penalty data P10",
  indivID    = "ID"
)

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))

##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_pl1=0, b_pl2=0, b_pl3=0, b_pl4=0, b_pl5=0, b_pl6=0,
              b_pm1=0, b_pm2=0, b_pm3=0, b_pm4=0, b_pm5=0, b_pm6=0,
              b_ph1=0, b_ph2=0, b_ph3=0, b_ph4=0, b_ph5=0, b_ph6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_pl5", "b_pm5", "b_ph5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_pl1*playerfitlow + b_pm1*playerfitmedium + b_ph1*playerfithigh
  V[['TC']]  = asc_TC + b_pl2*playerfitlow + b_pm2*playerfitmedium + b_ph2*playerfithigh
  V[['TR']]  = asc_TR + b_pl3*playerfitlow + b_pm3*playerfitmedium + b_ph3*playerfithigh
  V[['DL']]  = asc_DL + b_pl4*playerfitlow + b_pm4*playerfitmedium + b_ph4*playerfithigh
  V[['DC']]  = asc_DC + b_pl5*playerfitlow + b_pm5*playerfitmedium + b_ph5*playerfithigh
  V[['DR']]  = asc_DR + b_pl6*playerfitlow + b_pm6*playerfitmedium + b_ph6*playerfithigh
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer10 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer10)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)
----------------------------------
  #### ModelP11 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP11",
  modelDescr ="Simple MNL model on penalty data P11",
  indivID    = "ID"
)

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))

##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_plg1=0, b_plg2=0, b_plg3=0, b_plg4=0, b_plg5=0, b_plg6=0,
              b_pmg1=0, b_pmg2=0, b_pmg3=0, b_pmg4=0, b_pmg5=0, b_pmg6=0,
              b_phg1=0, b_phg2=0, b_phg3=0, b_phg4=0, b_phg5=0, b_phg6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_plg5", "b_pmg5", "b_phg5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_plg1*playerfitlow*status_cat*greatGK_cat + b_pmg1*playerfitmedium*status_cat*greatGK_cat + b_phg1*playerfithigh*status_cat*greatGK_cat
  V[['TC']]  = asc_TC + b_plg2*playerfitlow*status_cat*greatGK_cat + b_pmg2*playerfitmedium*status_cat*greatGK_cat + b_phg2*playerfithigh*status_cat*greatGK_cat
  V[['TR']]  = asc_TR + b_plg3*playerfitlow*status_cat*greatGK_cat + b_pmg3*playerfitmedium*status_cat*greatGK_cat + b_phg3*playerfithigh*status_cat*greatGK_cat
  V[['DL']]  = asc_DL + b_plg4*playerfitlow*status_cat*greatGK_cat + b_pmg4*playerfitmedium*status_cat*greatGK_cat + b_phg4*playerfithigh*status_cat*greatGK_cat
  V[['DC']]  = asc_DC + b_plg5*playerfitlow*status_cat*greatGK_cat + b_pmg5*playerfitmedium*status_cat*greatGK_cat + b_phg5*playerfithigh*status_cat*greatGK_cat
  V[['DR']]  = asc_DR + b_plg6*playerfitlow*status_cat*greatGK_cat + b_pmg6*playerfitmedium*status_cat*greatGK_cat + b_phg6*playerfithigh*status_cat*greatGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer11 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer11)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)

apollo_saveOutput(modelPlayer11)
----------------------------------
  #### ModelP12 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP12",
  modelDescr ="Simple MNL model on penalty data P12",
  indivID    = "ID"
)

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))

##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_phg1=0, b_phg2=0, b_phg3=0, b_phg4=0, b_phg5=0, b_phg6=0,
              b_hl1=0, b_hl2=0, b_hl3=0, b_hl4=0, b_hl5=0, b_hl6=0, 
              b_plg1=0, b_plg2=0, b_plg3=0, b_plg4=0, b_plg5=0, b_plg6=0,
              b_pmg1=0, b_pmg2=0, b_pmg3=0, b_pmg4=0, b_pmg5=0, b_pmg6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_phg5", "b_hl5", "b_plg5", "b_pmg5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_hl1*playerfithigh*status_cat + b_plg1*playerfitlow*status_cat + b_pmg1*playerfitmedium*status_cat + b_phg1*greatGK_cat
  V[['TC']]  = asc_TC + b_hl2*playerfithigh*status_cat + b_plg2*playerfitlow*status_cat + b_pmg2*playerfitmedium*status_cat + b_phg2*greatGK_cat
  V[['TR']]  = asc_TR + b_hl3*playerfithigh*status_cat + b_plg3*playerfitlow*status_cat + b_pmg3*playerfitmedium*status_cat + b_phg3*greatGK_cat
  V[['DL']]  = asc_DL + b_hl4*playerfithigh*status_cat + b_plg4*playerfitlow*status_cat + b_pmg4*playerfitmedium*status_cat + b_phg4*greatGK_cat
  V[['DC']]  = asc_DC + b_hl5*playerfithigh*status_cat + b_plg5*playerfitlow*status_cat + b_pmg5*playerfitmedium*status_cat + b_phg5*greatGK_cat
  V[['DR']]  = asc_DR + b_hl6*playerfithigh*status_cat + b_plg6*playerfitlow*status_cat + b_pmg6*playerfitmedium*status_cat + b_phg6*greatGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer12 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer12)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)

----------------------------------
  #### ModelP13 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP13",
  modelDescr ="Simple MNL model on penalty data P13",
  indivID    = "ID"
)

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))
database$mvmtGKnew_cat <- as.numeric(as.character(factor(ifelse(database$mvmtGK_cat == 1 | database$mvmtGK_cat == 2 | database$mvmtGK_cat == 3 | database$mvmtGK_cat == 4,1,0))))


##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_hsm1=0, b_hsm2=0 ,b_hsm3=0 ,b_hsm4=0 ,b_hsm5=0 , b_hsm6=0,
              b_lsm1=0, b_lsm2=0 ,b_lsm3=0 ,b_lsm4=0 ,b_lsm5=0 , b_lsm6=0,
              b_msm1=0, b_msm2=0 ,b_msm3=0 ,b_msm4=0 ,b_msm5=0 , b_msm6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_hsm5", "b_lsm5", "b_msm5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_hsm1*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm1*playerfitlow*status_cat*mvmtGKnew_cat + b_msm1*playerfitmedium*status_cat*mvmtGKnew_cat 
  V[['TC']]  = asc_TC + b_hsm2*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm2*playerfitlow*status_cat*mvmtGKnew_cat + b_msm2*playerfitmedium*status_cat*mvmtGKnew_cat
  V[['TR']]  = asc_TR + b_hsm3*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm3*playerfitlow*status_cat*mvmtGKnew_cat + b_msm3*playerfitmedium*status_cat*mvmtGKnew_cat
  V[['DL']]  = asc_DL + b_hsm4*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm4*playerfitlow*status_cat*mvmtGKnew_cat + b_msm4*playerfitmedium*status_cat*mvmtGKnew_cat
  V[['DC']]  = asc_DC + b_hsm5*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm5*playerfitlow*status_cat*mvmtGKnew_cat + b_msm5*playerfitmedium*status_cat*mvmtGKnew_cat
  V[['DR']]  = asc_DR + b_hsm6*playerfithigh*status_cat*mvmtGKnew_cat + b_lsm6*playerfitlow*status_cat*mvmtGKnew_cat + b_msm6*playerfitmedium*status_cat*mvmtGKnew_cat 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer13 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer13)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)

apollo_saveOutput(modelPlayer13)
----------------------------------
  #### ModelP14 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelP14",
  modelDescr ="Simple MNL model on penalty data P14",
  indivID    = "ID"
)

database = read.csv("6altallcolumncompleted.csv",header=TRUE)
database$playerfitlow<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==1,1,0))))
database$playerfitmedium<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==2,1,0))))
database$playerfithigh<-as.numeric(as.character(as.factor(ifelse(database$Playerfit==3,1,0))))
database$status_cat <- as.numeric(as.character(as.factor(ifelse(database$Status == "Goal", 1, 0))))
database$greatGK_cat <- as.numeric(as.character(factor(ifelse(database$Great_GK == "yes",1,0))))
database$mvmtGKnew_cat <- as.numeric(as.character(factor(ifelse(database$mvmtGK_cat == 1 | database$mvmtGK_cat == 2 | database$mvmtGK_cat == 3 | database$mvmtGK_cat == 4,1,0))))
database$heightGKnew_cat <- as.numeric(as.character(factor(ifelse(database$HeiGK >= 190,1,0))))

##################################################################################################################

#database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/6altallcolumncompleted.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0,
              b_hsh1=0, b_hsh2=0 ,b_hsh3=0 ,b_hsh4=0 ,b_hsh5=0 , b_hsh6=0,
              b_lsh1=0, b_lsh2=0 ,b_lsh3=0 ,b_lsh4=0 ,b_lsh5=0 , b_lsh6=0,
              b_msh1=0, b_msh2=0 ,b_msh3=0 ,b_msh4=0 ,b_msh5=0 , b_msh6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_hsh5", "b_lsh5", "b_msh5")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_hsh1*playerfithigh*status_cat*heightGKnew_cat + b_lsh1*playerfitlow*status_cat*heightGKnew_cat + b_msh1*playerfitmedium*status_cat*heightGKnew_cat 
  V[['TC']]  = asc_TC + b_hsh2*playerfithigh*status_cat*heightGKnew_cat + b_lsh2*playerfitlow*status_cat*heightGKnew_cat + b_msh2*playerfitmedium*status_cat*heightGKnew_cat
  V[['TR']]  = asc_TR + b_hsh3*playerfithigh*status_cat*heightGKnew_cat + b_lsh3*playerfitlow*status_cat*heightGKnew_cat + b_msh3*playerfitmedium*status_cat*heightGKnew_cat
  V[['DL']]  = asc_DL + b_hsh4*playerfithigh*status_cat*heightGKnew_cat + b_lsh4*playerfitlow*status_cat*heightGKnew_cat + b_msh4*playerfitmedium*status_cat*heightGKnew_cat
  V[['DC']]  = asc_DC + b_hsh5*playerfithigh*status_cat*heightGKnew_cat + b_lsh5*playerfitlow*status_cat*heightGKnew_cat + b_msh5*playerfitmedium*status_cat*heightGKnew_cat
  V[['DR']]  = asc_DR + b_hsh6*playerfithigh*status_cat*heightGKnew_cat + b_lsh6*playerfitlow*status_cat*heightGKnew_cat + b_msh6*playerfitmedium*status_cat*heightGKnew_cat 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=
                        5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P) 
}


modelPlayer14 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelPlayer14)

#forecastNew = apollo_prediction(modelPlayerWhole,
#                               apollo_probabilities,
#                               apollo_inputs)

apollo_saveOutput(modelPlayer14)
