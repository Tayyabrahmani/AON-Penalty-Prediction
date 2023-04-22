#### IIA Property Verification####
### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model1_withallalt",
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
apollo_fixed = c("asc_DC", "b_foot5")


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


model1_withallalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model1_withallalt)
##################################################################################################################
#### #### IIA Property Verification####
###Model 1- without TL alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model1_withoutTLalt",
  modelDescr ="Simple MNL model on penalty data TL",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without1TL.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5")


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
  V[['DL']]  = asc_DL + b_foot4*foot_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
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


model_withoutTLalt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model_withoutTLalt)
##################################################################################################################
#### #### IIA Property Verification####
###Model 2- without TC alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model1_withoutTCalt",
  modelDescr ="Simple MNL model on penalty data TC",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without2TC.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5")


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
  V[['DL']]  = asc_DL + b_foot4*foot_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
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
##################################################################################################################
#### #### IIA Property Verification####
###Model 3- without TR alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model1_withoutTRalt",
  modelDescr ="Simple MNL model on penalty data TR",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without3TR.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot4=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5")


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
  V[['DL']]  = asc_DL + b_foot4*foot_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, DL=av_4, DC=av_5, DR=av_6), 
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
##################################################################################################################
#### #### IIA Property Verification####
###Model 4- without DL alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model4_withoutDLalt",
  modelDescr ="Simple MNL model on penalty data DL",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without4DL.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0,asc_DC  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot5=0, b_foot6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5")


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
  V[['DC']]  = asc_DC + b_foot5*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DC=av_5, DR=av_6), 
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
##################################################################################################################
#### #### IIA Property Verification####
###Model 5- without DC alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model5_withoutDCalt",
  modelDescr ="Simple MNL model on penalty data DC",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without5DC.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0,asc_DL  = 0, asc_DR = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot6=0)


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
  V[['DL']]  = asc_DL + b_foot4*foot_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DR=av_6), 
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
##################################################################################################################
#### #### IIA Property Verification####
###Model 6- without DR alternative
library(apollo)

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_model5_withoutDRalt",
  modelDescr ="Simple MNL model on penalty data DR",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn_without6DR.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0,asc_DL  = 0, asc_DC = 0, 
              b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5")


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
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5), 
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