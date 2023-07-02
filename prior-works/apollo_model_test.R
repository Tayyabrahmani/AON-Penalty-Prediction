# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_iterative_coding",
  modelDescr      = "MNL model using iterative coding for alternatives and attributes",
  indivID         = "ID", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### For this example, we generate a database with N observations
# following a logit generation process.
# - No panel data (i.e. each obs is from a different individual).
# - J alternatives, with varying availability (in this case set randomly, 
#   with a 70% chance of being available)
# - K attributes per alternative, each with values between 0 and 5.
# - True coefficients are 1 and -1 for odd and even numbered attributes, 
#   respectively.

set.seed(24)
N = 2000
J = 100
K = 10

X = list()
for(j in 1:J){
  X[[j]]=matrix(5*runif(N*K), nrow=N, ncol=K, 
                dimnames=list(c(), paste0("x_",j,"_",1:K)))
}

avail     = matrix(runif(N*J)>=0.3, nrow=N, ncol=J, 
                   dimnames=list(c(), paste0("avail_",1:J)))
epsilon   = -log(-log(matrix(runif(N*J), nrow=N, ncol=J)))
beta_true = rep(c(1,-1),ceiling(K/2))[1:K]
U         = sapply(X,"%*%",beta_true) + epsilon
U[!avail] = -Inf
Y         = apply(U, MARGIN=1, which.max)
database  = data.frame(ID=1:N, choice=Y, do.call(cbind,X), avail)
rm(N,X,avail,epsilon,U,Y,j)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = setNames(rep(0,K),paste0("beta_",1:K))

### Vector with names (in quotes) of parameters to be kept fixed at their 
# starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs   = apollo_validateInputs()
apollo_inputs$J = J # need to retain J (number of alternatives) for use
# inside apollo_probabilities
apollo_inputs$K = K # need to retain K (number of attributes) for use 
# inside apollo_probabilities

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, 
                                functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  for(j in 1:apollo_inputs$J){
    V[[paste0("alt_",j)]] = 0
    for(k in 1:apollo_inputs$K) V[[paste0("alt_",j)]] = V[[paste0("alt_",j)]] + 
        get(paste0("beta_",k))*get(paste0("x_",j,"_",k))
  }
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = setNames(1:apollo_inputs$J, names(V)), 
    avail         = setNames(apollo_inputs$database[,paste0("avail_",1:apollo_inputs$J)], names(V)), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)