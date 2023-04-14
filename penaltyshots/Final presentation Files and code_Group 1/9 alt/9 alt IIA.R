#### IIA Property Verification####
### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withallalt",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

# ################################################################# # 

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/File of 9 alt model IIA/9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_ML = 0, asc_MC = 0, asc_MR = 0, asc_DL = 0, asc_DC = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['MR']]  = asc_ML + b_foot4*foot_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat
  V[['ML']]  = asc_MR + b_foot6*foot_cat
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


model1_withallalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model1_withallalt)


#### #### IIA Property Verification#############################################################################################################
###Model 1- without TL alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutTLalt",
  modelDescr ="Simple MNL model on penalty data TL",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_TL_alt.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TC = 0, asc_TR = 0,asc_ML = 0,asc_MC  = 0, asc_MR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
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
    alternatives  = c(TC=2, TR=3,ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TC=av_2, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutTLalt1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutTLalt1)

#### #### IIA Property Verification#############################################################################################################
###Model 2- without TC alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutTCalt",
  modelDescr ="Simple MNL model on penalty data TC",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_2.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TR = 0,asc_ML = 0,asc_MC  = 0, asc_MR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['TR']]  = asc_TR + b_foot3*foot_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TR=3,ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TR=av_3, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutTCalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutTCalt)
#### #### IIA Property Verification#############################################################################################################
###Model 3- without TR alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutTRalt",
  modelDescr ="Simple MNL model on penalty data TR",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_3.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_ML = 0,asc_MC  = 0, asc_MR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['ML']]  = asc_ML + b_foot4*foot_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,ML=4, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, ML=av_4, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutTRalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutTRalt)
#### #### IIA Property Verification#############################################################################################################
###Model 4- without ML alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutMLalt",
  modelDescr ="Simple MNL model on penalty data ML",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_4.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_MC  = 0, asc_MR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['MC']]  = asc_MC + b_foot5*foot_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, MC=5, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, MC=av_5, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutMLalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutMLalt)
#### #### IIA Property Verification#############################################################################################################
###Model 5- without MC alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutMCalt",
  modelDescr ="Simple MNL model on penalty data MC",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_5.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_ML  = 0, asc_MR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot6=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_TC", "b_foot2")


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
  V[['MR']]  = asc_MR + b_foot6*foot_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, ML=4, MR=6, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MR=av_6, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutMCalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutMCalt)
#### #### IIA Property Verification#############################################################################################################
###Model 6- without MR alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutMRalt",
  modelDescr ="Simple MNL model on penalty data MR",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_6.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_ML  = 0, asc_MC= 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot7=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['DL']]  = asc_DL + b_foot7*foot_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, ML=4, MC=5, DL=7, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5, DL=av_7, DC=av_8, DR=av_9), 
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


model_withoutMRalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutMRalt)
#### #### IIA Property Verification#############################################################################################################
###Model 7- without DL alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutDLalt",
  modelDescr ="Simple MNL model on penalty data DL",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_7.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_ML  = 0, asc_MC= 0, asc_MR = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot8=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['DC']]  = asc_DC + b_foot8*foot_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, ML=4, MC=5, MR=6, DC=8, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5,MR=av_6, DC=av_8, DR=av_9), 
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


model_withoutDLalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutDLalt)
#### #### IIA Property Verification#############################################################################################################
###Model 8- without DC alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutDCalt",
  modelDescr ="Simple MNL model on penalty data DC",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_8.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_ML  = 0, asc_MC= 0, asc_MR = 0,asc_DL  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot9=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  V[['DR']]  = asc_DR + b_foot9*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, ML=4, MC=5, MR=6, DL=7, DR=9), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5,MR=av_6, DL=av_7, DR=av_9), 
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


model_withoutDCalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutDCalt)
#### #### IIA Property Verification#############################################################################################################
###Model 9- without DR alternative

library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_model1_withoutDRalt",
  modelDescr ="Simple MNL model on penalty data DR",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("9altallcolumn_without_9.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0,asc_TR = 0,asc_ML  = 0, asc_MC= 0, asc_MR = 0,asc_DL  = 0, asc_DC = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, b_foot7=0, b_foot8=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5")


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
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2,TR=3, ML=4, MC=5, MR=6, DL=7, DC=8), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, ML=av_4, MC=av_5,MR=av_6, DL=av_7, DC=av_8), 
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


model_withoutDRalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutDRalt)


