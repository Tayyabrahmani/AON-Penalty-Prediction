#### Final model ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_9_alt_modelfinal",
  modelDescr ="Simple MNL model on penalty data1",
  indivID    = "ID"
)

################################################################## # 

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/9altallcolumn.csv",header=TRUE)
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
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_ts1=0, b_ts2=0, b_ts3=0, b_ts4=0, b_ts5=0, b_ts6=0, b_ts7=0, b_ts8=0, b_ts9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5", "b_ts5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_ts1*shothardness_cat*tappedball_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_ts2*shothardness_cat*tappedball_cat 
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_ts3*shothardness_cat*tappedball_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_ts4*shothardness_cat*tappedball_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_ts5*shothardness_cat*tappedball_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_ts6*shothardness_cat*tappedball_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_ts7*shothardness_cat*tappedball_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_ts8*shothardness_cat*tappedball_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_ts9*shothardness_cat*tappedball_cat
  
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

database = read.csv("/Users/nitishaggarwal/Desktop/Project_1_penaltyShot (2)/Project/test_data.csv",header=TRUE)
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
              b_standposGK_cat1=0,b_standposGK_cat2=0,b_standposGK_cat3=0,b_standposGK_cat4=0,b_standposGK_cat5=0,b_standposGK_cat6=0,b_standposGK_cat7=0,b_standposGK_cat8=0,b_standposGK_cat9=0,
              b_GKmvmttyp_cat1=0,b_GKmvmttyp_cat2=0,b_GKmvmttyp_cat3=0,b_GKmvmttyp_cat4=0,b_GKmvmttyp_cat5=0,b_GKmvmttyp_cat6=0,b_GKmvmttyp_cat7=0,b_GKmvmttyp_cat8=0,b_GKmvmttyp_cat9=0,
              b_compQuali_cat1=0,b_compQuali_cat2=0,b_compQuali_cat3=0,b_compQuali_cat4=0,b_compQuali_cat5=0,b_compQuali_cat6=0,b_compQuali_cat7=0,b_compQuali_cat8=0,b_compQuali_cat9=0,
              b_ts1=0, b_ts2=0, b_ts3=0, b_ts4=0, b_ts5=0, b_ts6=0, b_ts7=0, b_ts8=0, b_ts9=0)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_MC", "b_foot5","b_mvmtGK_cat5","b_ingameshoot_cat5","b_compleague_cat5","b_lastpenaltygoal_cat5","b_shothardness_cat5","b_tappedball_cat5","b_standposGK_cat5","b_GKmvmttyp_cat5","b_compQuali_cat5", "b_ts5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK_cat1*mvmtGK_cat + b_ingameshoot_cat1*ingameshoot_cat + b_compleague_cat1*compleague_cat + b_lastpenaltygoal_cat1*lastpenaltygoal_cat + b_shothardness_cat1*shothardness_cat + b_tappedball_cat1*tappedball_cat + b_standposGK_cat1*standposGK_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_compQuali_cat1*compQuali_cat + b_ts1*shothardness_cat*tappedball_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK_cat2*mvmtGK_cat + b_ingameshoot_cat2*ingameshoot_cat + b_compleague_cat2*compleague_cat + b_lastpenaltygoal_cat2*lastpenaltygoal_cat + b_shothardness_cat2*shothardness_cat + b_tappedball_cat2*tappedball_cat + b_standposGK_cat2*standposGK_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_compQuali_cat2*compQuali_cat + b_ts2*shothardness_cat*tappedball_cat 
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK_cat3*mvmtGK_cat + b_ingameshoot_cat3*ingameshoot_cat + b_compleague_cat3*compleague_cat + b_lastpenaltygoal_cat3*lastpenaltygoal_cat + b_shothardness_cat3*shothardness_cat + b_tappedball_cat3*tappedball_cat + b_standposGK_cat3*standposGK_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_compQuali_cat3*compQuali_cat + b_ts3*shothardness_cat*tappedball_cat
  V[['ML']]  = asc_ML + b_foot4*foot_cat + b_mvmtGK_cat4*mvmtGK_cat + b_ingameshoot_cat4*ingameshoot_cat + b_compleague_cat4*compleague_cat + b_lastpenaltygoal_cat4*lastpenaltygoal_cat + b_shothardness_cat4*shothardness_cat + b_tappedball_cat4*tappedball_cat + b_standposGK_cat4*standposGK_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_compQuali_cat4*compQuali_cat + b_ts4*shothardness_cat*tappedball_cat
  V[['MC']]  = asc_MC + b_foot5*foot_cat + b_mvmtGK_cat5*mvmtGK_cat + b_ingameshoot_cat5*ingameshoot_cat + b_compleague_cat5*compleague_cat + b_lastpenaltygoal_cat5*lastpenaltygoal_cat + b_shothardness_cat5*shothardness_cat + b_tappedball_cat5*tappedball_cat + b_standposGK_cat5*standposGK_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_compQuali_cat5*compQuali_cat + b_ts5*shothardness_cat*tappedball_cat
  V[['MR']]  = asc_MR + b_foot6*foot_cat + b_mvmtGK_cat6*mvmtGK_cat + b_ingameshoot_cat6*ingameshoot_cat + b_compleague_cat6*compleague_cat + b_lastpenaltygoal_cat6*lastpenaltygoal_cat + b_shothardness_cat6*shothardness_cat + b_tappedball_cat6*tappedball_cat + b_standposGK_cat6*standposGK_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_compQuali_cat6*compQuali_cat + b_ts6*shothardness_cat*tappedball_cat
  V[['DL']]  = asc_DL + b_foot7*foot_cat + b_mvmtGK_cat7*mvmtGK_cat + b_ingameshoot_cat7*ingameshoot_cat + b_compleague_cat7*compleague_cat + b_lastpenaltygoal_cat7*lastpenaltygoal_cat + b_shothardness_cat7*shothardness_cat + b_tappedball_cat7*tappedball_cat + b_standposGK_cat7*standposGK_cat + b_GKmvmttyp_cat7*GKmvmttyp_cat + b_compQuali_cat7*compQuali_cat + b_ts7*shothardness_cat*tappedball_cat
  V[['DC']]  = asc_DC + b_foot8*foot_cat + b_mvmtGK_cat8*mvmtGK_cat + b_ingameshoot_cat8*ingameshoot_cat + b_compleague_cat8*compleague_cat + b_lastpenaltygoal_cat8*lastpenaltygoal_cat + b_shothardness_cat8*shothardness_cat + b_tappedball_cat8*tappedball_cat + b_standposGK_cat8*standposGK_cat + b_GKmvmttyp_cat8*GKmvmttyp_cat + b_compQuali_cat8*compQuali_cat + b_ts8*shothardness_cat*tappedball_cat
  V[['DR']]  = asc_DR + b_foot9*foot_cat + b_mvmtGK_cat9*mvmtGK_cat + b_ingameshoot_cat9*ingameshoot_cat + b_compleague_cat9*compleague_cat + b_lastpenaltygoal_cat9*lastpenaltygoal_cat + b_shothardness_cat9*shothardness_cat + b_tappedball_cat9*tappedball_cat + b_standposGK_cat9*standposGK_cat + b_GKmvmttyp_cat9*GKmvmttyp_cat + b_compQuali_cat9*compQuali_cat + b_ts9*shothardness_cat*tappedball_cat
  
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

test=apollo_prediction(final_model,
                       apollo_probabilities,
                       apollo_inputs)
test

