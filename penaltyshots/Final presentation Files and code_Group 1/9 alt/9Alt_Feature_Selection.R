################################################################## #
#### NULL model                       ####
################################################################## #

#install.packages("apollo")

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(lmtest)

### Initialize code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model",
  modelDescr ="Simple MNL model on penalty datanull",
  indivID    = "ID"
)


database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC")
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
  V[['ML']]  = asc_ML
  V[['MC']]  = asc_MC
  V[['MR']]  = asc_MR
  V[['DL']]  = asc_DL
  V[['DC']]  = asc_DC
  V[['DR']]  = asc_DR
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model1)

forecast1 = apollo_prediction(model1,
                              apollo_probabilities,
                              apollo_inputs)

apollo_saveOutput(model1)
---------------------------------------------------------------------------------------------------------------------------------
#### Model2 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")
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
  V[['ML']]  = asc_ML + b_foot4*foot_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model2)

forecast2 = apollo_prediction(model2,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model2)

#Log Likelihood ratio test
apollo_lrTest(model1, model2)
---------------------------------------------------------------------------------------------------------------------------------
#### Model3 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model3)

forecast3 = apollo_prediction(model3,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model3)

#Log Likelihood ratio test
apollo_lrTest(model2, model3)
-----------------------------------------------------------------------------------------------------------------------------
#### Model4 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model4 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model4)

forecast4 = apollo_prediction(model4,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model4)

#Log Likelihood ratio test
apollo_lrTest(model3, model4)
---------------------------------------------------------------------------------------------------------------------------
#### Model5 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model5 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model5)

forecast5 = apollo_prediction(model5,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model5)

#Log Likelihood ratio test
apollo_lrTest(model4, model5)
---------------------------------------------------------------------------------------------------------------------------
#### Model6 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model6 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model6)

forecast6 = apollo_prediction(model6,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model6)

#Log Likelihood ratio test
apollo_lrTest(model5, model6)
---------------------------------------------------------------------------------------------------------------------------
#### Model7 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_opplstpengoal_cat1=0,b_opplstpengoal_cat2=0,b_opplstpengoal_cat3=0,b_opplstpengoal_cat4=0,b_opplstpengoal_cat5=0,b_opplstpengoal_cat6=0,b_opplstpengoal_cat7=0,b_opplstpengoal_cat8=0,b_opplstpengoal_cat9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_opplstpengoal_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_opplstpengoal_cat1*opplstpengoal_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_opplstpengoal_cat2*opplstpengoal_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_opplstpengoal_cat3*opplstpengoal_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_opplstpengoal_cat4*opplstpengoal_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_opplstpengoal_cat5*opplstpengoal_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_opplstpengoal_cat6*opplstpengoal_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_opplstpengoal_cat7*opplstpengoal_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_opplstpengoal_cat8*opplstpengoal_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_opplstpengoal_cat9*opplstpengoal_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model7 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model7)

forecast7 = apollo_prediction(model7,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model7)

#Log Likelihood ratio test
apollo_lrTest(model6, model7)
---------------------------------------------------------------------------------------------------------------------------
#### Model8 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0)
              


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model8 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model8)

forecast8 = apollo_prediction(model8,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model8)

#Log Likelihood ratio test
apollo_lrTest(model6, model8)
---------------------------------------------------------------------------------------------------------------------------
#### Model9 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model9 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model9)

forecast9 = apollo_prediction(model9,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model9)

#Log Likelihood ratio test
apollo_lrTest(model8, model9)
---------------------------------------------------------------------------------------------------------------------------
#### Model10 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygoldPL_cat1=0,b_jerseygoldPL_cat2=0,b_jerseygoldPL_cat3=0,b_jerseygoldPL_cat4=0,b_jerseygoldPL_cat5=0,b_jerseygoldPL_cat6=0,b_jerseygoldPL_cat7=0,b_jerseygoldPL_cat8=0,b_jerseygoldPL_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygoldPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygoldPL_cat1*jerseygoldPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygoldPL_cat2*jerseygoldPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygoldPL_cat3*jerseygoldPL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygoldPL_cat4*jerseygoldPL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygoldPL_cat5*jerseygoldPL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygoldPL_cat6*jerseygoldPL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygoldPL_cat7*jerseygoldPL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygoldPL_cat8*jerseygoldPL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygoldPL_cat9*jerseygoldPL_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model10 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model10)

forecast10 = apollo_prediction(model10,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model10)

#Log Likelihood ratio test
apollo_lrTest(model9, model10)
---------------------------------------------------------------------------------------------------------------------------
#### Model11 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseypinkPL_cat1=0,b_jerseypinkPL_cat2=0,b_jerseypinkPL_cat3=0,b_jerseypinkPL_cat4=0,b_jerseypinkPL_cat5=0,b_jerseypinkPL_cat6=0,b_jerseypinkPL_cat7=0,b_jerseypinkPL_cat8=0,b_jerseypinkPL_cat9=0)
              


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseypinkPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseypinkPL_cat1*jerseypinkPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseypinkPL_cat2*jerseypinkPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseypinkPL_cat3*jerseypinkPL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseypinkPL_cat4*jerseypinkPL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseypinkPL_cat5*jerseypinkPL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseypinkPL_cat6*jerseypinkPL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseypinkPL_cat7*jerseypinkPL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseypinkPL_cat8*jerseypinkPL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseypinkPL_cat9*jerseypinkPL_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model11 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model11)

forecast11 = apollo_prediction(model11,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model11)

#Log Likelihood ratio test
apollo_lrTest(model9, model11)
---------------------------------------------------------------------------------------------------------------------------
#### Model12 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model12 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model12)

forecast12 = apollo_prediction(model12,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model12)

#Log Likelihood ratio test
apollo_lrTest(model9, model12)
---------------------------------------------------------------------------------------------------------------------------
#### Model13 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_secondlastGKdivedir_cat1=0,b_secondlastGKdivedir_cat2=0,b_secondlastGKdivedir_cat3=0,b_secondlastGKdivedir_cat4=0,b_secondlastGKdivedir_cat5=0,b_secondlastGKdivedir_cat6=0,b_secondlastGKdivedir_cat7=0,b_secondlastGKdivedir_cat8=0,b_secondlastGKdivedir_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_secondlastGKdivedir_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_secondlastGKdivedir_cat1*secondlastGKdivedir_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_secondlastGKdivedir_cat2*secondlastGKdivedir_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_secondlastGKdivedir_cat3*secondlastGKdivedir_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_secondlastGKdivedir_cat4*secondlastGKdivedir_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_secondlastGKdivedir_cat5*secondlastGKdivedir_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_secondlastGKdivedir_cat6*secondlastGKdivedir_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_secondlastGKdivedir_cat7*secondlastGKdivedir_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_secondlastGKdivedir_cat8*secondlastGKdivedir_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_secondlastGKdivedir_cat9*secondlastGKdivedir_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model13 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model13)

forecast13 = apollo_prediction(model13,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model13)

#Log Likelihood ratio test
apollo_lrTest(model12, model13)
---------------------------------------------------------------------------------------------------------------------------
#### Model15 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0
              )


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model15 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model15)

forecast15 = apollo_prediction(model15,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model15)

#Log Likelihood ratio test
apollo_lrTest(model12, model15)
---------------------------------------------------------------------------------------------------------------------------
#### Model16 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_heightGK_cat1=0,b_heightGK_cat2=0,b_heightGK_cat3=0,b_heightGK_cat4=0,b_heightGK_cat5=0,b_heightGK_cat6=0,b_heightGK_cat7=0,b_heightGK_cat8=0,b_heightGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_heightGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_heightGK_cat1*heightGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_heightGK_cat2*heightGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_heightGK_cat3*heightGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_heightGK_cat4*heightGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_heightGK_cat5*heightGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_heightGK_cat6*heightGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_heightGK_cat7*heightGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_heightGK_cat8*heightGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_heightGK_cat9*heightGK_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model16 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model16)

forecast16 = apollo_prediction(model16,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model16)

#Log Likelihood ratio test
apollo_lrTest(model15, model16)
---------------------------------------------------------------------------------------------------------------------------
#### Model17 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_decider_cat1=0,b_decider_cat2=0,b_decider_cat3=0,b_decider_cat4=0,b_decider_cat5=0,b_decider_cat6=0,b_decider_cat7=0,b_decider_cat8=0,b_decider_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_decider_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_decider_cat1*decider_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_decider_cat2*decider_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_decider_cat3*decider_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_decider_cat4*decider_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_decider_cat5*decider_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_decider_cat6*decider_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_decider_cat7*decider_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_decider_cat8*decider_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_decider_cat9*decider_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model17 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model17)

forecast17 = apollo_prediction(model17,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model17)

#Log Likelihood ratio test
apollo_lrTest(model15, model17)
---------------------------------------------------------------------------------------------------------------------------
#### Model18 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_roundnogroup_cat1=0,b_roundnogroup_cat2=0,b_roundnogroup_cat3=0,b_roundnogroup_cat4=0,b_roundnogroup_cat5=0,b_roundnogroup_cat6=0,b_roundnogroup_cat7=0,b_roundnogroup_cat8=0,b_roundnogroup_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_roundnogroup_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_roundnogroup_cat1*roundnogroup_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_roundnogroup_cat2*roundnogroup_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_roundnogroup_cat3*roundnogroup_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_roundnogroup_cat4*roundnogroup_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_roundnogroup_cat5*roundnogroup_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_roundnogroup_cat6*roundnogroup_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_roundnogroup_cat7*roundnogroup_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_roundnogroup_cat8*roundnogroup_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_roundnogroup_cat9*roundnogroup_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model18 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model18)

forecast18 = apollo_prediction(model18,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model18)

#Log Likelihood ratio test
apollo_lrTest(model15, model18)
---------------------------------------------------------------------------------------------------------------------------
#### Model19 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_posdef_cat1=0,b_posdef_cat2=0,b_posdef_cat3=0,b_posdef_cat4=0,b_posdef_cat5=0,b_posdef_cat6=0,b_posdef_cat7=0,b_posdef_cat8=0,b_posdef_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_posdef_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_posdef_cat1*posdef_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_posdef_cat2*posdef_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_posdef_cat3*posdef_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_posdef_cat4*posdef_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_posdef_cat5*posdef_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_posdef_cat6*posdef_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_posdef_cat7*posdef_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_posdef_cat8*posdef_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_posdef_cat9*posdef_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model19 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model19)

forecast19 = apollo_prediction(model19,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model19)

#Log Likelihood ratio test
apollo_lrTest(model15, model19)
---------------------------------------------------------------------------------------------------------------------------
#### Model20 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model20 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model20)

forecast20 = apollo_prediction(model20,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model20)

#Log Likelihood ratio test
apollo_lrTest(model15, model20)
---------------------------------------------------------------------------------------------------------------------------
#### Model21 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_gameimportance_cat1=0,b_gameimportance_cat2=0,b_gameimportance_cat3=0,b_gameimportance_cat4=0,b_gameimportance_cat5=0,b_gameimportance_cat6=0,b_gameimportance_cat7=0,b_gameimportance_cat8=0,b_gameimportance_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_gameimportance_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_gameimportance_cat1*gameimportance_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_gameimportance_cat2*gameimportance_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_gameimportance_cat3*gameimportance_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_gameimportance_cat4*gameimportance_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_gameimportance_cat5*gameimportance_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_gameimportance_cat6*gameimportance_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_gameimportance_cat7*gameimportance_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_gameimportance_cat8*gameimportance_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_gameimportance_cat9*gameimportance_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model21 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model21)

forecast21 = apollo_prediction(model21,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model21)

#Log Likelihood ratio test
apollo_lrTest(model20, model21)
---------------------------------------------------------------------------------------------------------------------------
#### Model22 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_specialmatch_cat1=0,b_specialmatch_cat2=0,b_specialmatch_cat3=0,b_specialmatch_cat4=0,b_specialmatch_cat5=0,b_specialmatch_cat6=0,b_specialmatch_cat7=0,b_specialmatch_cat8=0,b_specialmatch_cat9=0)
             



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_specialmatch_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_specialmatch_cat1*specialmatch_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_specialmatch_cat2*specialmatch_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_specialmatch_cat3*specialmatch_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_specialmatch_cat4*specialmatch_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_specialmatch_cat5*specialmatch_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_specialmatch_cat6*specialmatch_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_specialmatch_cat7*specialmatch_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_specialmatch_cat8*specialmatch_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_specialmatch_cat9*specialmatch_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model22 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model22)

forecast22 = apollo_prediction(model22,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model22)

#Log Likelihood ratio test
apollo_lrTest(model20, model22)
---------------------------------------------------------------------------------------------------------------------------
#### Model23 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_lastGKdive_cat1=0,b_lastGKdive_cat2=0,b_lastGKdive_cat3=0,b_lastGKdive_cat4=0,b_lastGKdive_cat5=0,b_lastGKdive_cat6=0,b_lastGKdive_cat7=0,b_lastGKdive_cat8=0,b_lastGKdive_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_lastGKdive_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_lastGKdive_cat1*lastGKdive_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_lastGKdive_cat2*lastGKdive_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_lastGKdive_cat3*lastGKdive_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_lastGKdive_cat4*lastGKdive_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_lastGKdive_cat5*lastGKdive_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_lastGKdive_cat6*lastGKdive_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_lastGKdive_cat7*lastGKdive_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_lastGKdive_cat8*lastGKdive_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_lastGKdive_cat9*lastGKdive_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model23 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model23)

forecast23 = apollo_prediction(model23,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model23)

#Log Likelihood ratio test
apollo_lrTest(model20, model23)
---------------------------------------------------------------------------------------------------------------------------
#### Model24 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_clubornational_cat1=0,b_clubornational_cat2=0,b_clubornational_cat3=0,b_clubornational_cat4=0,b_clubornational_cat5=0,b_clubornational_cat6=0,b_clubornational_cat7=0,b_clubornational_cat8=0,b_clubornational_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_clubornational_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_clubornational_cat1*clubornational_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_clubornational_cat2*clubornational_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_clubornational_cat3*clubornational_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_clubornational_cat4*clubornational_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_clubornational_cat5*clubornational_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_clubornational_cat6*clubornational_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_clubornational_cat7*clubornational_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_clubornational_cat8*clubornational_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_clubornational_cat9*clubornational_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model24 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model24)

forecast24 = apollo_prediction(model24,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model24)

#Log Likelihood ratio test
apollo_lrTest(model20, model24)
---------------------------------------------------------------------------------------------------------------------------
#### Model25 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compCup_cat1=0,b_compCup_cat2=0,b_compCup_cat3=0,b_compCup_cat4=0,b_compCup_cat5=0,b_compCup_cat6=0,b_compCup_cat7=0,b_compCup_cat8=0,b_compCup_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compCup_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compCup_cat1*compCup_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compCup_cat2*compCup_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compCup_cat3*compCup_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compCup_cat4*compCup_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compCup_cat5*compCup_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compCup_cat6*compCup_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compCup_cat7*compCup_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compCup_cat8*compCup_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compCup_cat9*compCup_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model25 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model25)

forecast25 = apollo_prediction(model25,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model25)

#Log Likelihood ratio test
apollo_lrTest(model20, model25)
---------------------------------------------------------------------------------------------------------------------------
#### Model26 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compECC_cat1=0,b_compECC_cat2=0,b_compECC_cat3=0,b_compECC_cat4=0,b_compECC_cat5=0,b_compECC_cat6=0,b_compECC_cat7=0,b_compECC_cat8=0,b_compECC_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compECC_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compECC_cat1*compECC_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compECC_cat2*compECC_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compECC_cat3*compECC_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compECC_cat4*compECC_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compECC_cat5*compECC_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compECC_cat6*compECC_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compECC_cat7*compECC_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compECC_cat8*compECC_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compECC_cat9*compECC_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model26 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model26)

forecast26 = apollo_prediction(model26,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model26)

#Log Likelihood ratio test
apollo_lrTest(model20, model26)
---------------------------------------------------------------------------------------------------------------------------
#### Model27 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compNational_cat1=0,b_compNational_cat2=0,b_compNational_cat3=0,b_compNational_cat4=0,b_compNational_cat5=0,b_compNational_cat6=0,b_compNational_cat7=0,b_compNational_cat8=0,b_compNational_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compNational_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compNational_cat1*compNational_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compNational_cat2*compNational_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compNational_cat3*compNational_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compNational_cat4*compNational_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compNational_cat5*compNational_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compNational_cat6*compNational_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compNational_cat7*compNational_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compNational_cat8*compNational_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compNational_cat9*compNational_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model27 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model27)

forecast27 = apollo_prediction(model27,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model27)

#Log Likelihood ratio test
apollo_lrTest(model20, model27)
--------------------------------------------------------------------------------------------------------------------------
#### Model28 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model28 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model28)

forecast28 = apollo_prediction(model28,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model28)

#Log Likelihood ratio test
apollo_lrTest(model20, model28)
----------------------------------------------------------------------------------------------------------------------------
#### Model29 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseygreyGK_cat1=0,b_jerseygreyGK_cat2=0,b_jerseygreyGK_cat3=0,b_jerseygreyGK_cat4=0,b_jerseygreyGK_cat5=0,b_jerseygreyGK_cat6=0,b_jerseygreyGK_cat7=0,b_jerseygreyGK_cat8=0,b_jerseygreyGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseygreyGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseygreyGK_cat1*jerseygreyGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseygreyGK_cat2*jerseygreyGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseygreyGK_cat3*jerseygreyGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseygreyGK_cat4*jerseygreyGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseygreyGK_cat5*jerseygreyGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseygreyGK_cat6*jerseygreyGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseygreyGK_cat7*jerseygreyGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseygreyGK_cat8*jerseygreyGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseygreyGK_cat9*jerseygreyGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model29 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model29)

forecast29 = apollo_prediction(model29,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model29)

#Log Likelihood ratio test
apollo_lrTest(model28, model29)
----------------------------------------------------------------------------------------------------------------------------
#### Model30 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyredGK_cat1=0,b_jerseyredGK_cat2=0,b_jerseyredGK_cat3=0,b_jerseyredGK_cat4=0,b_jerseyredGK_cat5=0,b_jerseyredGK_cat6=0,b_jerseyredGK_cat7=0,b_jerseyredGK_cat8=0,b_jerseyredGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyredGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyredGK_cat1*jerseyredGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyredGK_cat2*jerseyredGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyredGK_cat3*jerseyredGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyredGK_cat4*jerseyredGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyredGK_cat5*jerseyredGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyredGK_cat6*jerseyredGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyredGK_cat7*jerseyredGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyredGK_cat8*jerseyredGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyredGK_cat9*jerseyredGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model30 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model30)

forecast30 = apollo_prediction(model30,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model30)

#Log Likelihood ratio test
apollo_lrTest(model28, model30)
----------------------------------------------------------------------------------------------------------------------------
#### Model31 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackGK_cat1=0,b_jerseyblackGK_cat2=0,b_jerseyblackGK_cat3=0,b_jerseyblackGK_cat4=0,b_jerseyblackGK_cat5=0,b_jerseyblackGK_cat6=0,b_jerseyblackGK_cat7=0,b_jerseyblackGK_cat8=0,b_jerseyblackGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackGK_cat1*jerseyblackGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackGK_cat2*jerseyblackGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackGK_cat3*jerseyblackGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackGK_cat4*jerseyblackGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackGK_cat5*jerseyblackGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackGK_cat6*jerseyblackGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackGK_cat7*jerseyblackGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackGK_cat8*jerseyblackGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackGK_cat9*jerseyblackGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model31 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model31)

forecast31 = apollo_prediction(model31,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model31)

#Log Likelihood ratio test
apollo_lrTest(model28, model31)
---------------------------------------------------------------------------------------------------------------------------
#### Model32 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblueGK_cat1=0,b_jerseyblueGK_cat2=0,b_jerseyblueGK_cat3=0,b_jerseyblueGK_cat4=0,b_jerseyblueGK_cat5=0,b_jerseyblueGK_cat6=0,b_jerseyblueGK_cat7=0,b_jerseyblueGK_cat8=0,b_jerseyblueGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblueGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblueGK_cat1*jerseyblueGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblueGK_cat2*jerseyblueGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblueGK_cat3*jerseyblueGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblueGK_cat4*jerseyblueGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblueGK_cat5*jerseyblueGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblueGK_cat6*jerseyblueGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblueGK_cat7*jerseyblueGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblueGK_cat8*jerseyblueGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblueGK_cat9*jerseyblueGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model32 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model32)

forecast32 = apollo_prediction(model32,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model32)

#Log Likelihood ratio test
apollo_lrTest(model28, model32)
---------------------------------------------------------------------------------------------------------------------------
#### Model33 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseygreenGK_cat1=0,b_jerseygreenGK_cat2=0,b_jerseygreenGK_cat3=0,b_jerseygreenGK_cat4=0,b_jerseygreenGK_cat5=0,b_jerseygreenGK_cat6=0,b_jerseygreenGK_cat7=0,b_jerseygreenGK_cat8=0,b_jerseygreenGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseygreenGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseygreenGK_cat1*jerseygreenGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseygreenGK_cat2*jerseygreenGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseygreenGK_cat3*jerseygreenGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseygreenGK_cat4*jerseygreenGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseygreenGK_cat5*jerseygreenGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseygreenGK_cat6*jerseygreenGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseygreenGK_cat7*jerseygreenGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseygreenGK_cat8*jerseygreenGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseygreenGK_cat9*jerseygreenGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model33 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model33)

forecast33 = apollo_prediction(model33,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model33)

#Log Likelihood ratio test
apollo_lrTest(model28, model33)
---------------------------------------------------------------------------------------------------------------------------
#### Model35 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyyellowGK_cat1=0,b_jerseyyellowGK_cat2=0,b_jerseyyellowGK_cat3=0,b_jerseyyellowGK_cat4=0,b_jerseyyellowGK_cat5=0,b_jerseyyellowGK_cat6=0,b_jerseyyellowGK_cat7=0,b_jerseyyellowGK_cat8=0,b_jerseyyellowGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyyellowGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyyellowGK_cat1*jerseyyellowGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyyellowGK_cat2*jerseyyellowGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyyellowGK_cat3*jerseyyellowGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyyellowGK_cat4*jerseyyellowGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyyellowGK_cat5*jerseyyellowGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyyellowGK_cat6*jerseyyellowGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyyellowGK_cat7*jerseyyellowGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyyellowGK_cat8*jerseyyellowGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyyellowGK_cat9*jerseyyellowGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model34 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model34)

forecast34 = apollo_prediction(model34,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model34)

#Log Likelihood ratio test
apollo_lrTest(model28, model34)
---------------------------------------------------------------------------------------------------------------------------
#### Model35 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyorangeGK_cat1=0,b_jerseyorangeGK_cat2=0,b_jerseyorangeGK_cat3=0,b_jerseyorangeGK_cat4=0,b_jerseyorangeGK_cat5=0,b_jerseyorangeGK_cat6=0,b_jerseyorangeGK_cat7=0,b_jerseyorangeGK_cat8=0,b_jerseyorangeGK_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyorangeGK_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyorangeGK_cat1*jerseyorangeGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyorangeGK_cat2*jerseyorangeGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyorangeGK_cat3*jerseyorangeGK_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyorangeGK_cat4*jerseyorangeGK_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyorangeGK_cat5*jerseyorangeGK_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyorangeGK_cat6*jerseyorangeGK_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyorangeGK_cat7*jerseyorangeGK_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyorangeGK_cat8*jerseyorangeGK_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyorangeGK_cat9*jerseyorangeGK_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model35 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model35)

forecast35 = apollo_prediction(model35,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model35)

#Log Likelihood ratio test
apollo_lrTest(model28, model35)
---------------------------------------------------------------------------------------------------------------------------
#### Model36 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackPL_cat1=0,b_jerseyblackPL_cat2=0,b_jerseyblackPL_cat3=0,b_jerseyblackPL_cat4=0,b_jerseyblackPL_cat5=0,b_jerseyblackPL_cat6=0,b_jerseyblackPL_cat7=0,b_jerseyblackPL_cat8=0,b_jerseyblackPL_cat9=0
              )



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackPL_cat1*jerseyblackPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackPL_cat2*jerseyblackPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackPL_cat3*jerseyblackPL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackPL_cat4*jerseyblackPL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackPL_cat5*jerseyblackPL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackPL_cat6*jerseyblackPL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackPL_cat7*jerseyblackPL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackPL_cat8*jerseyblackPL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackPL_cat9*jerseyblackPL_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model36 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model36)

forecast36 = apollo_prediction(model36,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model36)

#Log Likelihood ratio test
apollo_lrTest(model28, model36)
---------------------------------------------------------------------------------------------------------------------------
#### Model37 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackPL_cat1=0,b_jerseyblackPL_cat2=0,b_jerseyblackPL_cat3=0,b_jerseyblackPL_cat4=0,b_jerseyblackPL_cat5=0,b_jerseyblackPL_cat6=0,b_jerseyblackPL_cat7=0,b_jerseyblackPL_cat8=0,b_jerseyblackPL_cat9=0,
              b_jerseywhitePL_cat1=0,b_jerseywhitePL_cat2=0,b_jerseywhitePL_cat3=0,b_jerseywhitePL_cat4=0,b_jerseywhitePL_cat5=0,b_jerseywhitePL_cat6=0,b_jerseywhitePL_cat7=0,b_jerseywhitePL_cat8=0,b_jerseywhitePL_cat9=0
)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackPL_cat5","b_jerseywhitePL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackPL_cat1*jerseyblackPL_cat + b_jerseywhitePL_cat1*jerseywhitePL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackPL_cat2*jerseyblackPL_cat + b_jerseywhitePL_cat2*jerseywhitePL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackPL_cat3*jerseyblackPL_cat + b_jerseywhitePL_cat3*jerseywhitePL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackPL_cat4*jerseyblackPL_cat + b_jerseywhitePL_cat4*jerseywhitePL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackPL_cat5*jerseyblackPL_cat + b_jerseywhitePL_cat5*jerseywhitePL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackPL_cat6*jerseyblackPL_cat + b_jerseywhitePL_cat6*jerseywhitePL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackPL_cat7*jerseyblackPL_cat + b_jerseywhitePL_cat7*jerseywhitePL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackPL_cat8*jerseyblackPL_cat + b_jerseywhitePL_cat8*jerseywhitePL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackPL_cat9*jerseyblackPL_cat + b_jerseywhitePL_cat9*jerseywhitePL_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model37 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model37)

forecast37 = apollo_prediction(model37,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model37)

#Log Likelihood ratio test
apollo_lrTest(model36, model37)
---------------------------------------------------------------------------------------------------------------------------
#### Model38 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackPL_cat1=0,b_jerseyblackPL_cat2=0,b_jerseyblackPL_cat3=0,b_jerseyblackPL_cat4=0,b_jerseyblackPL_cat5=0,b_jerseyblackPL_cat6=0,b_jerseyblackPL_cat7=0,b_jerseyblackPL_cat8=0,b_jerseyblackPL_cat9=0,
              b_jerseypurplePL_cat1=0,b_jerseypurplePL_cat2=0,b_jerseypurplePL_cat3=0,b_jerseypurplePL_cat4=0,b_jerseypurplePL_cat5=0,b_jerseypurplePL_cat6=0,b_jerseypurplePL_cat7=0,b_jerseypurplePL_cat8=0,b_jerseypurplePL_cat9=0
)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackPL_cat5","b_jerseypurplePL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackPL_cat1*jerseyblackPL_cat + b_jerseypurplePL_cat1*jerseypurplePL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackPL_cat2*jerseyblackPL_cat + b_jerseypurplePL_cat2*jerseypurplePL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackPL_cat3*jerseyblackPL_cat + b_jerseypurplePL_cat3*jerseypurplePL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackPL_cat4*jerseyblackPL_cat + b_jerseypurplePL_cat4*jerseypurplePL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackPL_cat5*jerseyblackPL_cat + b_jerseypurplePL_cat5*jerseypurplePL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackPL_cat6*jerseyblackPL_cat + b_jerseypurplePL_cat6*jerseypurplePL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackPL_cat7*jerseyblackPL_cat + b_jerseypurplePL_cat7*jerseypurplePL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackPL_cat8*jerseyblackPL_cat + b_jerseypurplePL_cat8*jerseypurplePL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackPL_cat9*jerseyblackPL_cat + b_jerseypurplePL_cat9*jerseypurplePL_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model38 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model38)

forecast38 = apollo_prediction(model38,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model38)

#Log Likelihood ratio test
apollo_lrTest(model36, model38)
---------------------------------------------------------------------------------------------------------------------------
#### Model39 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackPL_cat1=0,b_jerseyblackPL_cat2=0,b_jerseyblackPL_cat3=0,b_jerseyblackPL_cat4=0,b_jerseyblackPL_cat5=0,b_jerseyblackPL_cat6=0,b_jerseyblackPL_cat7=0,b_jerseyblackPL_cat8=0,b_jerseyblackPL_cat9=0,
              b_jerseyredPL_cat1=0,b_jerseyredPL_cat2=0,b_jerseyredPL_cat3=0,b_jerseyredPL_cat4=0,b_jerseyredPL_cat5=0,b_jerseyredPL_cat6=0,b_jerseyredPL_cat7=0,b_jerseyredPL_cat8=0,b_jerseyredPL_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackPL_cat5","b_jerseyredPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackPL_cat1*jerseyblackPL_cat + b_jerseyredPL_cat1*jerseyredPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackPL_cat2*jerseyblackPL_cat + b_jerseyredPL_cat2*jerseyredPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackPL_cat3*jerseyblackPL_cat + b_jerseyredPL_cat3*jerseyredPL_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackPL_cat4*jerseyblackPL_cat + b_jerseyredPL_cat4*jerseyredPL_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackPL_cat5*jerseyblackPL_cat + b_jerseyredPL_cat5*jerseyredPL_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackPL_cat6*jerseyblackPL_cat + b_jerseyredPL_cat6*jerseyredPL_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackPL_cat7*jerseyblackPL_cat + b_jerseyredPL_cat7*jerseyredPL_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackPL_cat8*jerseyblackPL_cat + b_jerseyredPL_cat8*jerseyredPL_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackPL_cat9*jerseyblackPL_cat + b_jerseyredPL_cat9*jerseyredPL_cat
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model39 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model39)

forecast39 = apollo_prediction(model39,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model39)

#Log Likelihood ratio test
apollo_lrTest(model36, model39)
---------------------------------------------------------------------------------------------------------------------------
#### Final model ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("~/Documents/R_TUTORIALS/Analytics_project/9Alt_model/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0,asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0,
              b_mvmtGK_cat1=0, b_mvmtGK_cat2=0, b_mvmtGK_cat3=0, b_mvmtGK_cat4=0, b_mvmtGK_cat5=0, b_mvmtGK_cat6=0, b_mvmtGK_cat7=0, b_mvmtGK_cat8=0, b_mvmtGK_cat9=0,
              b_ingameshoot_cat1=0,b_ingameshoot_cat2=0,b_ingameshoot_cat3=0,b_ingameshoot_cat4=0,b_ingameshoot_cat5=0,b_ingameshoot_cat6=0,b_ingameshoot_cat7=0,b_ingameshoot_cat8=0,b_ingameshoot_cat9=0,
              b_compleague_cat1=0,b_compleague_cat2=0,b_compleague_cat3=0,b_compleague_cat4=0,b_compleague_cat5=0,b_compleague_cat6=0,b_compleague_cat7=0,b_compleague_cat8=0, b_compleague_cat9=0,
              b_lastpenaltygoal_cat1=0,b_lastpenaltygoal_cat2=0,b_lastpenaltygoal_cat3=0,b_lastpenaltygoal_cat4=0,b_lastpenaltygoal_cat5=0,b_lastpenaltygoal_cat6=0,b_lastpenaltygoal_cat7=0,b_lastpenaltygoal_cat8=0,b_lastpenaltygoal_cat9=0,
              b_shothardness_cat1=0,b_shothardness_cat2=0,b_shothardness_cat3=0,b_shothardness_cat4=0,b_shothardness_cat5=0,b_shothardness_cat6=0,b_shothardness_cat7=0, b_shothardness_cat8=0,b_shothardness_cat9=0,
              b_tappedball_cat1=0,b_tappedball_cat2=0,b_tappedball_cat3=0,b_tappedball_cat4=0,b_tappedball_cat5=0,b_tappedball_cat6=0,b_tappedball_cat7=0,b_tappedball_cat8=0,b_tappedball_cat9=0,
              b_jerseygreenPL_cat1=0,b_jerseygreenPL_cat2=0,b_jerseygreenPL_cat3=0,b_jerseygreenPL_cat4=0,b_jerseygreenPL_cat5=0,b_jerseygreenPL_cat6=0,b_jerseygreenPL_cat7=0,b_jerseygreenPL_cat8=0,b_jerseygreenPL_cat9=0,
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_jerseyblackPL_cat1=0,b_jerseyblackPL_cat2=0,b_jerseyblackPL_cat3=0,b_jerseyblackPL_cat4=0,b_jerseyblackPL_cat5=0,b_jerseyblackPL_cat6=0,b_jerseyblackPL_cat7=0,b_jerseyblackPL_cat8=0,b_jerseyblackPL_cat9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_jerseygreenPL_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5","b_jerseyblackPL_cat5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_jerseygreenPL_cat1*jerseygreenPL_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_jerseyblackPL_cat1*jerseyblackPL_cat 
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_jerseygreenPL_cat2*jerseygreenPL_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_jerseyblackPL_cat2*jerseyblackPL_cat 
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_jerseygreenPL_cat3*jerseygreenPL_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_jerseyblackPL_cat3*jerseyblackPL_cat 
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_jerseygreenPL_cat4*jerseygreenPL_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_jerseyblackPL_cat4*jerseyblackPL_cat 
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_jerseygreenPL_cat5*jerseygreenPL_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_jerseyblackPL_cat5*jerseyblackPL_cat 
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_jerseygreenPL_cat6*jerseygreenPL_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_jerseyblackPL_cat6*jerseyblackPL_cat 
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_jerseygreenPL_cat7*jerseygreenPL_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_jerseyblackPL_cat7*jerseyblackPL_cat 
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_jerseygreenPL_cat8*jerseygreenPL_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_jerseyblackPL_cat8*jerseyblackPL_cat 
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_jerseygreenPL_cat9*jerseygreenPL_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_jerseyblackPL_cat9*jerseyblackPL_cat 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


final_model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(final_model)

forecast_final = apollo_prediction(final_model,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(final_model)
-------------------------------------------------------------------------------------------------------------------------------
  
  
  
 