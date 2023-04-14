#How to implement a model in apollo and minimize the Log-likelihood Function

install.packages("apollo")						# get apollo package
library(apollo)								# run apollo package
apollo_initialise()

database=read.csv(file="ModeChoiceData.csv",header=TRUE,row.names=1)   #get mode choice data


#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

apollo_control=list(modelName="BaseSpec",
modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_rail              = 0,
              b_cost                = 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		

  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['car']]  = 		    b_cost * cost_car
  V[['rail']] = asc_rail +  b_cost * cost_rail  
 
   mnl_settings = list(						       ### Define settings for model 
   alternatives = c(car=1, rail=2),					 ### component	
   avail        = list(car=av_car, rail=av_rail),
   choiceVar    = choice,
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

compareResults(L,Modelnames)
