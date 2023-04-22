#How to implement a model in apollo and minimize the Log-likelihood Function

install.packages("apollo")						# get apollo package
library(apollo)								# run apollo package
apollo_initialise()

install.packages("tidyverse")
library(tidyverse)	

library(readxl)
database <- read_excel("SixAlt.xlsx")
View(database)
database <- as.data.frame(database)


#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

# sort the data by the individual identifier column
#database <- database %>% arrange(ID)

apollo_control=list(modelName="BaseSpec_Penalty_shot_Prediction",
                    modelDescr="Constants only model",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_TC = 0, asc_TR = 0, asc_BL = 0, asc_BC = 0, asc_BR = 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['TL']]  = 	0	      
  V[['TC']]  =  asc_TC 
  V[['TR']]  = 	asc_TR 
  V[['BL']]  = 	asc_BL  
  V[['BC']]  =  asc_BC 
  V[['BR']]  =  asc_BR 
 
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(TL=1, TC=2, TR=3, BL=4, BC=5, BR=6),					 ### component	
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

L<-NULL
L[[1]]<-BaseSpec


Modelnames<-c("test1")