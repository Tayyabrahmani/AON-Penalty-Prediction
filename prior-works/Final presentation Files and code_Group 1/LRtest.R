# ################################################################# #
#### NULL model                       ####
# ################################################################# #

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
  modelName  ="Apollo_6_alt_model",
  modelDescr ="Simple MNL model on penalty datanull",
  indivID    = "ID"
)


database = read.csv("6altallcolumn.csv",header=TRUE)
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


model1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model1)

forecast1 = apollo_prediction(model1,
                              apollo_probabilities,
                              apollo_inputs)

apollo_saveOutput(model1)
-------------------------------------------------------------------------------------------------------------------

#### Model2 ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model2",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4")
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


model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model2)

forecast2 = apollo_prediction(model2,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model2)

#Log Likelihood ratio test
apollo_lrTest(model1, model2)
---------------------------------------------------------------------------------------------------------------------------
#### Model3 ####

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model3",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat
  
  
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


model3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model3)

forecast3 = apollo_prediction(model3,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model3)

#Log Likelihood ratio test
apollo_lrTest(model2, model3)
------------------------------------------------------------------------------------------------------------------

#### Model4 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model4",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat
  
  
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


model4 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model4)

forecast4 = apollo_prediction(model4,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model4)

#Log Likelihood ratio test
apollo_lrTest(model3, model4)
-----------------------------------------------------------------------------------------------------------------

#### Model5 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model5",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat
  
  
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


model5 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model5)

forecast5 = apollo_prediction(model5,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model5)

#Log Likelihood ratio test
apollo_lrTest(model4, model5)
--------------------------------------------------------------------------------------------------------------------
#### Model6 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model6",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat
  
  
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


model6 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model6)

forecast5 = apollo_prediction(model6,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model6)

#Log Likelihood ratio test 
apollo_lrTest(model5, model6)
---------------------------------------------------------------------------------------------------------------
#### Model7 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model7",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat
  
  
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
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model8",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat
  
  
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


model8 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model8)

forecast8 = apollo_prediction(model8,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model8)

#Log Likelihood ratio test 
apollo_lrTest(model7, model8)
-------------------------------------------------------------------------------------------------------------------------------
#### Model9 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model9",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat
  
  
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


model9 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model9)

forecast9 = apollo_prediction(model9,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model9)

#Log Likelihood ratio test 
apollo_lrTest(model8, model9)
-------------------------------------------------------------------------------------------------------------------------
#### Model10 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model10",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat
  
  
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


model10 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model10)

forecast10 = apollo_prediction(model10,
                              apollo_probabilities,
                              apollo_inputs)
apollo_saveOutput(model10)

#Log Likelihood ratio test 
apollo_lrTest(model9, model10)
-------------------------------------------------------------------------------------------------------------------------
#### Model11 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model11",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat
  
  
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


model11 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model11)

forecast11 = apollo_prediction(model11,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model11)

#Log Likelihood ratio test 
apollo_lrTest(model10, model11)
-----------------------------------------------------------------------------------------------------------------------------
#### Model12 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model12",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat
  
  
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


model12 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model12)

forecast12 = apollo_prediction(model12,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model12)

#Log Likelihood ratio test 
apollo_lrTest(model11, model12)
---------------------------------------------------------------------------------------------------------------------------
#### Model13 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model13",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat
  
  
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


model13 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model13)

forecast13 = apollo_prediction(model13,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model13)

#Log Likelihood ratio test 
apollo_lrTest(model12, model13)  
----------------------------------------------------------------------------------------------------------------------------
#### Model14 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model14",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat
  
  
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


model14 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model14)

forecast14 = apollo_prediction(model14,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model14)

#Log Likelihood ratio test 
apollo_lrTest(model13, model14)
-----------------------------------------------------------------------------------------------------------------------
#### Model15 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model15",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat
  
  
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


model15 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model15)

forecast15 = apollo_prediction(model15,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model15)

#Log Likelihood ratio test 
apollo_lrTest(model14, model15)
----------------------------------------------------------------------------------------------------------------------------
#### Model16 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model16",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat 
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat
  
  
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


model16 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model16)

forecast16 = apollo_prediction(model16,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model16)

#Log Likelihood ratio test 
apollo_lrTest(model15, model16)
-------------------------------------------------------------------------------------------------------------------------
#### Model17 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model17",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat
  
  
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


model17 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model17)

forecast17 = apollo_prediction(model17,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model17)

#Log Likelihood ratio test 
apollo_lrTest(model16, model17)
----------------------------------------------------------------------------------------------------------------------------
#### Model18 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model18",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat
  
  
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


model18 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model18)

forecast18 = apollo_prediction(model18,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model18)

#Log Likelihood ratio test 
apollo_lrTest(model17, model18)
------------------------------------------------------------------------------------------------------------------------------
#### Model19 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model19",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat
  
  
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


model19 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model19)

forecast19 = apollo_prediction(model19,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model19)

#Log Likelihood ratio test 
apollo_lrTest(model18, model19)
---------------------------------------------------------------------------------------------------------------------------
#### Model20 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model20",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat
  
  
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


model20 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model20)

forecast20 = apollo_prediction(model20,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model20)

#Log Likelihood ratio test 
apollo_lrTest(model19, model20)
-----------------------------------------------------------------------------------------------------------------
#### Model21 ####
### Initialise code
  apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model21",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat
  
  
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


model21 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model21)

forecast21 = apollo_prediction(model21,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model21)

#Log Likelihood ratio test 
apollo_lrTest(model20, model21)
------------------------------------------------------------------------------------------------------------------------
#### Model22 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model22",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat
  
  
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


model22 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model22)

forecast22 = apollo_prediction(model22,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model22)

#Log Likelihood ratio test 
apollo_lrTest(model21, model22)
--------------------------------------------------------------------------------------------------------------------------
#### Model23 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model23",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat
  
  
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


model23 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model23)

forecast23 = apollo_prediction(model23,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model23)

#Log Likelihood ratio test 
apollo_lrTest(model22, model23)
------------------------------------------------------------------------------------------------------------------------
#### Model24 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model24",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat
  
  
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


model24 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model24)

forecast24 = apollo_prediction(model24,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model24)

#Log Likelihood ratio test 
apollo_lrTest(model23, model24)
----------------------------------------------------------------------------------------------------------------------------
#### Model25 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model25",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat
  
  
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


model25 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model25)

forecast25 = apollo_prediction(model25,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model25)

#Log Likelihood ratio test 
apollo_lrTest(model24, model25)
---------------------------------------------------------------------------------------------------------------------------
#### Model26 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model26",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat
  
  
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


model26 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model26)

forecast26 = apollo_prediction(model26,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model26)

#Log Likelihood ratio test 
apollo_lrTest(model25, model26)
----------------------------------------------------------------------------------------------------------------------------
#### Model27 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model27",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat
  
  
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


model27 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model27)

forecast27 = apollo_prediction(model27,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model27)

#Log Likelihood ratio test 
apollo_lrTest(model26, model27)
----------------------------------------------------------------------------------------------------------------------------
#### Model28 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model28",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat
  
  
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


model28 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model28)

forecast28 = apollo_prediction(model28,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model28)

#Log Likelihood ratio test 
apollo_lrTest(model27, model28)
----------------------------------------------------------------------------------------------------------------------------
#### Model29 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model29",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat
  
  
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


model29 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model29)

forecast29 = apollo_prediction(model29,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model29)

#Log Likelihood ratio test 
apollo_lrTest(model28, model29)
-----------------------------------------------------------------------------------------------------------------------------
#### Model30 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model30",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat
  
  
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


model30 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model30)

forecast30 = apollo_prediction(model30,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model30)

#Log Likelihood ratio test 
apollo_lrTest(model29, model30)
---------------------------------------------------------------------------------------------------------------------------
#### Model31 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model31",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat
  
  
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


model31 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model31)

forecast31 = apollo_prediction(model31,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model31)

#Log Likelihood ratio test 
apollo_lrTest(model30, model31)
------------------------------------------------------------------------------------------------------------------------
#### Model32 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model32",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat
  
  
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


model32 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model32)

forecast32 = apollo_prediction(model32,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model32)

#Log Likelihood ratio test 
apollo_lrTest(model31, model32)
------------------------------------------------------------------------------------------------------------------------------
#### Model33 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model33",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat
  
  
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


model33 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model33)

forecast33 = apollo_prediction(model33,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model33)

#Log Likelihood ratio test 
apollo_lrTest(model32, model33)
-------------------------------------------------------------------------------------------------------------------------------
#### Model34 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model34",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat
  
  
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


model34 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model34)

forecast34 = apollo_prediction(model34,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model34)

#Log Likelihood ratio test 
apollo_lrTest(model33, model34)
--------------------------------------------------------------------------------------------------------------------------------
#### Model35 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model35",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0,
              b_jerseypinkPL1=0,b_jerseypinkPL2=0,b_jerseypinkPL3=0,b_jerseypinkPL4=0,b_jerseypinkPL5=0,b_jerseypinkPL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4","b_jerseypinkPL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat + b_jerseypinkPL1*jerseypinkPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat + b_jerseypinkPL2*jerseypinkPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat + b_jerseypinkPL3*jerseypinkPL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat + b_jerseypinkPL4*jerseypinkPL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat + b_jerseypinkPL5*jerseypinkPL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat + b_jerseypinkPL6*jerseypinkPL_cat
  
  
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


model35 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model35)

forecast35 = apollo_prediction(model35,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model35)

#Log Likelihood ratio test 
apollo_lrTest(model34, model35)
-------------------------------------------------------------------------------------------------------------------------------
#### Model36 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model36",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0,
              b_jerseypinkPL1=0,b_jerseypinkPL2=0,b_jerseypinkPL3=0,b_jerseypinkPL4=0,b_jerseypinkPL5=0,b_jerseypinkPL6=0,
              b_jerseypurplePL1=0,b_jerseypurplePL2=0,b_jerseypurplePL3=0,b_jerseypurplePL4=0,b_jerseypurplePL5=0,b_jerseypurplePL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4","b_jerseypinkPL4","b_jerseypurplePL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat + b_jerseypinkPL1*jerseypinkPL_cat + b_jerseypurplePL1*jerseypurplePL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat + b_jerseypinkPL2*jerseypinkPL_cat + b_jerseypurplePL2*jerseypurplePL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat + b_jerseypinkPL3*jerseypinkPL_cat + b_jerseypurplePL3*jerseypurplePL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat + b_jerseypinkPL4*jerseypinkPL_cat + b_jerseypurplePL4*jerseypurplePL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat + b_jerseypinkPL5*jerseypinkPL_cat + b_jerseypurplePL5*jerseypurplePL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat + b_jerseypinkPL6*jerseypinkPL_cat + b_jerseypurplePL6*jerseypurplePL_cat
  
  
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


model36 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model36)

forecast36 = apollo_prediction(model36,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model36)

#Log Likelihood ratio test 
apollo_lrTest(model35, model36)
--------------------------------------------------------------------------------------------------------------------------------
#### Model37 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model37",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0,
              b_jerseypinkPL1=0,b_jerseypinkPL2=0,b_jerseypinkPL3=0,b_jerseypinkPL4=0,b_jerseypinkPL5=0,b_jerseypinkPL6=0,
              b_jerseypurplePL1=0,b_jerseypurplePL2=0,b_jerseypurplePL3=0,b_jerseypurplePL4=0,b_jerseypurplePL5=0,b_jerseypurplePL6=0,
              b_jerseyredPL1=0,b_jerseyredPL2=0,b_jerseyredPL3=0,b_jerseyredPL4=0,b_jerseyredPL5=0,b_jerseyredPL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4","b_jerseypinkPL4","b_jerseypurplePL4","b_jerseyredPL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat + b_jerseypinkPL1*jerseypinkPL_cat + b_jerseypurplePL1*jerseypurplePL_cat + b_jerseyredPL1*jerseyredPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat + b_jerseypinkPL2*jerseypinkPL_cat + b_jerseypurplePL2*jerseypurplePL_cat + b_jerseyredPL2*jerseyredPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat + b_jerseypinkPL3*jerseypinkPL_cat + b_jerseypurplePL3*jerseypurplePL_cat + b_jerseyredPL3*jerseyredPL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat + b_jerseypinkPL4*jerseypinkPL_cat + b_jerseypurplePL4*jerseypurplePL_cat + b_jerseyredPL4*jerseyredPL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat + b_jerseypinkPL5*jerseypinkPL_cat + b_jerseypurplePL5*jerseypurplePL_cat + b_jerseyredPL5*jerseyredPL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat + b_jerseypinkPL6*jerseypinkPL_cat + b_jerseypurplePL6*jerseypurplePL_cat + b_jerseyredPL6*jerseyredPL_cat
  
  
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


model37 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model37)

forecast37 = apollo_prediction(model37,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model37)

#Log Likelihood ratio test 
apollo_lrTest(model36, model37)
-----------------------------------------------------------------------------------------------------------------------------------
#### Model38 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model38",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0,
              b_jerseypinkPL1=0,b_jerseypinkPL2=0,b_jerseypinkPL3=0,b_jerseypinkPL4=0,b_jerseypinkPL5=0,b_jerseypinkPL6=0,
              b_jerseypurplePL1=0,b_jerseypurplePL2=0,b_jerseypurplePL3=0,b_jerseypurplePL4=0,b_jerseypurplePL5=0,b_jerseypurplePL6=0,
              b_jerseyredPL1=0,b_jerseyredPL2=0,b_jerseyredPL3=0,b_jerseyredPL4=0,b_jerseyredPL5=0,b_jerseyredPL6=0,
              b_jerseygreenPL1=0,b_jerseygreenPL2=0,b_jerseygreenPL3=0,b_jerseygreenPL4=0,b_jerseygreenPL5=0,b_jerseygreenPL6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4","b_jerseypinkPL4","b_jerseypurplePL4","b_jerseyredPL4","b_jerseygreenPL4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat + b_jerseypinkPL1*jerseypinkPL_cat + b_jerseypurplePL1*jerseypurplePL_cat + b_jerseyredPL1*jerseyredPL_cat + b_jerseygreenPL1*jerseygreenPL_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat + b_jerseypinkPL2*jerseypinkPL_cat + b_jerseypurplePL2*jerseypurplePL_cat + b_jerseyredPL2*jerseyredPL_cat + b_jerseygreenPL2*jerseygreenPL_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat + b_jerseypinkPL3*jerseypinkPL_cat + b_jerseypurplePL3*jerseypurplePL_cat + b_jerseyredPL3*jerseyredPL_cat + b_jerseygreenPL3*jerseygreenPL_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat + b_jerseypinkPL4*jerseypinkPL_cat + b_jerseypurplePL4*jerseypurplePL_cat + b_jerseyredPL4*jerseyredPL_cat + b_jerseygreenPL4*jerseygreenPL_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat + b_jerseypinkPL5*jerseypinkPL_cat + b_jerseypurplePL5*jerseypurplePL_cat + b_jerseyredPL5*jerseyredPL_cat + b_jerseygreenPL5*jerseygreenPL_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat + b_jerseypinkPL6*jerseypinkPL_cat + b_jerseypurplePL6*jerseypurplePL_cat + b_jerseyredPL6*jerseyredPL_cat + b_jerseygreenPL6*jerseygreenPL_cat
  
  
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


model38 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model38)

forecast38 = apollo_prediction(model38,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model38)

#Log Likelihood ratio test 
apollo_lrTest(model37, model38)
-----------------------------------------------------------------------------------------------------------------------------------
#### Model39 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model39",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0,
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0,
              b_standposGK1=0,b_standposGK2=0,b_standposGK3=0,b_standposGK4=0,b_standposGK5=0,b_standposGK6=0,
              b_ingameshoot1=0,b_ingameshoot2=0,b_ingameshoot3=0,b_ingameshoot4=0,b_ingameshoot5=0,b_ingameshoot6=0,
              b_compleague1=0,b_compleague2=0,b_compleague3=0,b_compleague4=0,b_compleague5=0,b_compleague6=0,
              b_lastpenaltygoal1=0,b_lastpenaltygoal2=0,b_lastpenaltygoal3=0,b_lastpenaltygoal4=0,b_lastpenaltygoal5=0,b_lastpenaltygoal6=0,
              b_opplstpengoal1=0,b_opplstpengoal2=0,b_opplstpengoal3=0,b_opplstpengoal4=0,b_opplstpengoal5=0,b_opplstpengoal6=0,
              b_shothardness1=0,b_shothardness2=0,b_shothardness3=0,b_shothardness4=0,b_shothardness5=0,b_shothardness6=0,
              b_tappedball1=0,b_tappedball2=0,b_tappedball3=0,b_tappedball4=0,b_tappedball5=0,b_tappedball6=0,
              b_lastGKdive1=0,b_lastGKdive2=0,b_lastGKdive3=0,b_lastGKdive4=0,b_lastGKdive5=0,b_lastGKdive6=0,
              b_clubornational1=0,b_clubornational2=0,b_clubornational3=0,b_clubornational4=0,b_clubornational5=0,b_clubornational6=0,
              b_compNational1=0,b_compNational2=0,b_compNational3=0,b_compNational4=0,b_compNational5=0,b_compNational6=0,
              b_secondlastGKdivedir1=0,b_secondlastGKdivedir2=0,b_secondlastGKdivedir3=0,b_secondlastGKdivedir4=0,b_secondlastGKdivedir5=0,b_secondlastGKdivedir6=0,
              b_heightGK1=0,b_heightGK2=0,b_heightGK3=0,b_heightGK4=0,b_heightGK5=0,b_heightGK6=0,
              b_decider1=0,b_decider2=0,b_decider3=0,b_decider4=0,b_decider5=0,b_decider6=0,
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_posdef1=0,b_posdef2=0,b_posdef3=0,b_posdef4=0,b_posdef5=0,b_posdef6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_gameimportance1=0,b_gameimportance2=0,b_gameimportance3=0,b_gameimportance4=0,b_gameimportance5=0,b_gameimportance6=0,
              b_specialmatch1=0,b_specialmatch2=0,b_specialmatch3=0,b_specialmatch4=0,b_specialmatch5=0,b_specialmatch6=0,
              b_compCup1=0,b_compCup2=0,b_compCup3=0,b_compCup4=0,b_compCup5=0,b_compCup6=0,
              b_compECC1=0,b_compECC2=0,b_compECC3=0,b_compECC4=0,b_compECC5=0,b_compECC6=0,
              b_compQuali1=0,b_compQuali2=0,b_compQuali3=0,b_compQuali4=0,b_compQuali5=0,b_compQuali6=0,
              b_jerseygreyGK1=0,b_jerseygreyGK2=0,b_jerseygreyGK3=0,b_jerseygreyGK4=0,b_jerseygreyGK5=0,b_jerseygreyGK6=0,
              b_jerseyredGK1=0,b_jerseyredGK2=0,b_jerseyredGK3=0,b_jerseyredGK4=0,b_jerseyredGK5=0,b_jerseyredGK6=0,
              b_jerseyblackGK1=0,b_jerseyblackGK2=0,b_jerseyblackGK3=0,b_jerseyblackGK4=0,b_jerseyblackGK5=0,b_jerseyblackGK6=0,
              b_jerseyblueGK1=0,b_jerseyblueGK2=0,b_jerseyblueGK3=0,b_jerseyblueGK4=0,b_jerseyblueGK5=0,b_jerseyblueGK6=0,
              b_jerseygreenGK1=0,b_jerseygreenGK2=0,b_jerseygreenGK3=0,b_jerseygreenGK4=0,b_jerseygreenGK5=0,b_jerseygreenGK6=0,
              b_jerseyyellowGK1=0,b_jerseyyellowGK2=0,b_jerseyyellowGK3=0,b_jerseyyellowGK4=0,b_jerseyyellowGK5=0,b_jerseyyellowGK6=0,
              b_jerseyorangeGK1=0,b_jerseyorangeGK2=0,b_jerseyorangeGK3=0,b_jerseyorangeGK4=0,b_jerseyorangeGK5=0,b_jerseyorangeGK6=0,
              b_jerseygoldPL1=0,b_jerseygoldPL2=0,b_jerseygoldPL3=0,b_jerseygoldPL4=0,b_jerseygoldPL5=0,b_jerseygoldPL6=0,
              b_jerseywhitePL1=0,b_jerseywhitePL2=0,b_jerseywhitePL3=0,b_jerseywhitePL4=0,b_jerseywhitePL5=0,b_jerseywhitePL6=0,
              b_jerseyblackPL1=0,b_jerseyblackPL2=0,b_jerseyblackPL3=0,b_jerseyblackPL4=0,b_jerseyblackPL5=0,b_jerseyblackPL6=0,
              b_jerseypinkPL1=0,b_jerseypinkPL2=0,b_jerseypinkPL3=0,b_jerseypinkPL4=0,b_jerseypinkPL5=0,b_jerseypinkPL6=0,
              b_jerseypurplePL1=0,b_jerseypurplePL2=0,b_jerseypurplePL3=0,b_jerseypurplePL4=0,b_jerseypurplePL5=0,b_jerseypurplePL6=0,
              b_jerseyredPL1=0,b_jerseyredPL2=0,b_jerseyredPL3=0,b_jerseyredPL4=0,b_jerseyredPL5=0,b_jerseyredPL6=0,
              b_jerseygreenPL1=0,b_jerseygreenPL2=0,b_jerseygreenPL3=0,b_jerseygreenPL4=0,b_jerseygreenPL5=0,b_jerseygreenPL6=0,
              b_secondlastpen1=0,b_secondlastpen2=0,b_secondlastpen3=0,b_secondlastpen4=0,b_secondlastpen5=0,b_secondlastpen6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4","b_standposGK4","b_ingameshoot4","b_compleague4","b_lastpenaltygoal4","b_opplstpengoal4","b_shothardness4","b_tappedball4","b_lastGKdive4","b_clubornational4","b_compNational4","b_secondlastGKdivedir4","b_heightGK4","b_decider4","b_roundnogroup4","b_posdef4","b_GKmvmttyp4","b_gameimportance4","b_specialmatch4","b_compCup4","b_compECC4","b_compQuali4","b_jerseygreyGK4","b_jerseyredGK4","b_jerseyblackGK4","b_jerseyblueGK4","b_jerseygreenGK4","b_jerseyyellowGK4","b_jerseyorangeGK4","b_jerseygoldPL4","b_jerseywhitePL4","b_jerseyblackPL4","b_jerseypinkPL4","b_jerseypurplePL4","b_jerseyredPL4","b_jerseygreenPL4","b_secondlastpen4")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK1*heightGK_cat + b_decider1*decider_cat + b_roundnogroup1*roundnogroup_cat + b_posdef1*posdef_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_gameimportance1*gameimportance_cat + b_specialmatch1*specialmatch_cat + b_compCup1*compCup_cat + b_compECC1*compECC_cat + b_compQuali1*compQuali_cat + b_jerseygreyGK1*jerseygreyGK_cat + b_jerseyredGK1*jerseyredGK_cat + b_jerseyblackGK1*jerseyblackGK_cat + b_jerseyblueGK1*jerseyblueGK_cat + b_jerseygreenGK1*jerseygreenGK_cat + b_jerseyyellowGK1*jerseyyellowGK_cat + b_jerseyorangeGK1*jerseyorangeGK_cat + b_jerseygoldPL1*jerseygoldPL_cat + b_jerseywhitePL1*jerseywhitePL_cat + b_jerseyblackPL1*jerseyblackPL_cat + b_jerseypinkPL1*jerseypinkPL_cat + b_jerseypurplePL1*jerseypurplePL_cat + b_jerseyredPL1*jerseyredPL_cat + b_jerseygreenPL1*jerseygreenPL_cat + b_secondlastpen1*secondlastpen_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK2*heightGK_cat + b_decider2*decider_cat + b_roundnogroup2*roundnogroup_cat + b_posdef2*posdef_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_gameimportance2*gameimportance_cat + b_specialmatch2*specialmatch_cat + b_compCup2*compCup_cat + b_compECC2*compECC_cat + b_compQuali2*compQuali_cat + b_jerseygreyGK2*jerseygreyGK_cat + b_jerseyredGK2*jerseyredGK_cat + b_jerseyblackGK2*jerseyblackGK_cat + b_jerseyblueGK2*jerseyblueGK_cat + b_jerseygreenGK2*jerseygreenGK_cat + b_jerseyyellowGK2*jerseyyellowGK_cat + b_jerseyorangeGK2*jerseyorangeGK_cat + b_jerseygoldPL2*jerseygoldPL_cat + b_jerseywhitePL2*jerseywhitePL_cat + b_jerseyblackPL2*jerseyblackPL_cat + b_jerseypinkPL2*jerseypinkPL_cat + b_jerseypurplePL2*jerseypurplePL_cat + b_jerseyredPL2*jerseyredPL_cat + b_jerseygreenPL2*jerseygreenPL_cat + b_secondlastpen2*secondlastpen_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK3*heightGK_cat + b_decider3*decider_cat + b_roundnogroup3*roundnogroup_cat + b_posdef3*posdef_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_gameimportance3*gameimportance_cat + b_specialmatch3*specialmatch_cat + b_compCup3*compCup_cat + b_compECC3*compECC_cat + b_compQuali3*compQuali_cat + b_jerseygreyGK3*jerseygreyGK_cat + b_jerseyredGK3*jerseyredGK_cat + b_jerseyblackGK3*jerseyblackGK_cat + b_jerseyblueGK3*jerseyblueGK_cat + b_jerseygreenGK3*jerseygreenGK_cat + b_jerseyyellowGK3*jerseyyellowGK_cat + b_jerseyorangeGK3*jerseyorangeGK_cat + b_jerseygoldPL3*jerseygoldPL_cat + b_jerseywhitePL3*jerseywhitePL_cat + b_jerseyblackPL3*jerseyblackPL_cat + b_jerseypinkPL3*jerseypinkPL_cat + b_jerseypurplePL3*jerseypurplePL_cat + b_jerseyredPL3*jerseyredPL_cat + b_jerseygreenPL3*jerseygreenPL_cat + b_secondlastpen3*secondlastpen_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK4*heightGK_cat + b_decider4*decider_cat + b_roundnogroup4*roundnogroup_cat + b_posdef4*posdef_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_gameimportance4*gameimportance_cat + b_specialmatch4*specialmatch_cat + b_compCup4*compCup_cat + b_compECC4*compECC_cat + b_compQuali4*compQuali_cat + b_jerseygreyGK4*jerseygreyGK_cat + b_jerseyredGK4*jerseyredGK_cat + b_jerseyblackGK4*jerseyblackGK_cat + b_jerseyblueGK4*jerseyblueGK_cat + b_jerseygreenGK4*jerseygreenGK_cat + b_jerseyyellowGK4*jerseyyellowGK_cat + b_jerseyorangeGK4*jerseyorangeGK_cat + b_jerseygoldPL4*jerseygoldPL_cat + b_jerseywhitePL4*jerseywhitePL_cat + b_jerseyblackPL4*jerseyblackPL_cat + b_jerseypinkPL4*jerseypinkPL_cat + b_jerseypurplePL4*jerseypurplePL_cat + b_jerseyredPL4*jerseyredPL_cat + b_jerseygreenPL4*jerseygreenPL_cat + b_secondlastpen4*secondlastpen_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK5*heightGK_cat + b_decider5*decider_cat + b_roundnogroup5*roundnogroup_cat + b_posdef5*posdef_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_gameimportance5*gameimportance_cat + b_specialmatch5*specialmatch_cat + b_compCup5*compCup_cat + b_compECC5*compECC_cat + b_compQuali5*compQuali_cat + b_jerseygreyGK5*jerseygreyGK_cat + b_jerseyredGK5*jerseyredGK_cat + b_jerseyblackGK5*jerseyblackGK_cat + b_jerseyblueGK5*jerseyblueGK_cat + b_jerseygreenGK5*jerseygreenGK_cat + b_jerseyyellowGK5*jerseyyellowGK_cat + b_jerseyorangeGK5*jerseyorangeGK_cat + b_jerseygoldPL5*jerseygoldPL_cat + b_jerseywhitePL5*jerseywhitePL_cat + b_jerseyblackPL5*jerseyblackPL_cat + b_jerseypinkPL5*jerseypinkPL_cat + b_jerseypurplePL5*jerseypurplePL_cat + b_jerseyredPL5*jerseyredPL_cat + b_jerseygreenPL5*jerseygreenPL_cat + b_secondlastpen5*secondlastpen_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK6*heightGK_cat + b_decider6*decider_cat + b_roundnogroup6*roundnogroup_cat + b_posdef6*posdef_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_gameimportance6*gameimportance_cat + b_specialmatch6*specialmatch_cat + b_compCup6*compCup_cat + b_compECC6*compECC_cat + b_compQuali6*compQuali_cat + b_jerseygreyGK6*jerseygreyGK_cat + b_jerseyredGK6*jerseyredGK_cat + b_jerseyblackGK6*jerseyblackGK_cat + b_jerseyblueGK6*jerseyblueGK_cat + b_jerseygreenGK6*jerseygreenGK_cat + b_jerseyyellowGK6*jerseyyellowGK_cat + b_jerseyorangeGK6*jerseyorangeGK_cat + b_jerseygoldPL6*jerseygoldPL_cat + b_jerseywhitePL6*jerseywhitePL_cat + b_jerseyblackPL6*jerseyblackPL_cat + b_jerseypinkPL6*jerseypinkPL_cat + b_jerseypurplePL6*jerseypurplePL_cat + b_jerseyredPL6*jerseyredPL_cat + b_jerseygreenPL6*jerseygreenPL_cat + b_secondlastpen6*secondlastpen_cat
  
  
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


model39 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model39)

forecast39 = apollo_prediction(model39,
                               apollo_probabilities,
                               apollo_inputs)
apollo_saveOutput(model39)

#Log Likelihood ratio test 
apollo_lrTest(model38, model39)