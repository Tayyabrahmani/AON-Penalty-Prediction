# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#install.packages("apollo")

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_nullmodel",
  modelDescr ="Simple MNL model on penalty data",
  indivID    = "ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# ################################################################# #
#### data preprocessing                       ####
# ################################################################# #
database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, b_foot1=0, b_foot2=0, b_foot3=0, b_foot4=0, b_foot5=0, b_foot6=0, 
              b_mvmtGK1=0, b_mvmtGK2=0, b_mvmtGK3=0, b_mvmtGK4=0, b_mvmtGK5=0, b_mvmtGK6=0, 
              b_standposGK1=0, b_standposGK2=0, b_standposGK3=0, b_standposGK4=0, b_standposGK5=0, b_standposGK6=0,
              b_ingameshoot1=0, b_ingameshoot2=0, b_ingameshoot3=0, b_ingameshoot4=0, b_ingameshoot5=0, b_ingameshoot6=0, 
              b_compleague1=0, b_compleague2=0, b_compleague3=0, b_compleague4=0,b_compleague5=0, b_compleague6=0, 
              b_lastpenaltygoal1=0, b_lastpenaltygoal2=0, b_lastpenaltygoal3=0, b_lastpenaltygoal4=0,b_lastpenaltygoal5=0, b_lastpenaltygoal6=0,
              b_opplstpengoal1=0, b_opplstpengoal2=0, b_opplstpengoal3=0, b_opplstpengoal4=0,b_opplstpengoal5=0, b_opplstpengoal6=0, 
              b_shothardness1=0, b_shothardness2=0, b_shothardness3=0, b_shothardness4=0, b_shothardness5=0, b_shothardness6=0, 
              b_tappedball1=0, b_tappedball2=0, b_tappedball3=0, b_tappedball4=0, b_tappedball5=0, b_tappedball6=0,
              b_lastGKdive1=0, b_lastGKdive2=0, b_lastGKdive3=0, b_lastGKdive4=0, b_lastGKdive5=0, b_lastGKdive6=0, 
              b_clubornational1=0, b_clubornational2=0, b_clubornational3=0, b_clubornational4=0, b_clubornational5=0, b_clubornational6=0, 
              b_compNational1=0, b_compNational2=0, b_compNational3=0, b_compNational4=0, b_compNational5=0, b_compNational6=0,
              b_secondlastGKdivedir1=0, b_secondlastGKdivedir2=0, b_secondlastGKdivedir3=0, b_secondlastGKdivedir4=0, b_secondlastGKdivedir5=0, b_secondlastGKdivedir6=0,
              b_heightGK_cat1=0, b_heightGK_cat2=0, b_heightGK_cat3=0, b_heightGK_cat4=0, b_heightGK_cat5=0, b_heightGK_cat6=0,
              b_decider_cat1=0, b_decider_cat2=0, b_decider_cat3=0, b_decider_cat4=0, b_decider_cat5=0, b_decider_cat6=0,
              b_roundnogroup_cat1=0, b_roundnogroup_cat2=0, b_roundnogroup_cat3=0, b_roundnogroup_cat4=0, b_roundnogroup_cat5=0, b_roundnogroup_cat6=0,
              b_posdef_cat1=0, b_posdef_cat2=0, b_posdef_cat3=0, b_posdef_cat4=0, b_posdef_cat5=0, b_posdef_cat6=0,
              b_GKmvmttyp_cat1=0, b_GKmvmttyp_cat2=0, b_GKmvmttyp_cat3=0, b_GKmvmttyp_cat4=0, b_GKmvmttyp_cat5=0, b_GKmvmttyp_cat6=0,
              b_gameimportance_cat1=0, b_gameimportance_cat2=0, b_gameimportance_cat3=0, b_gameimportance_cat4=0, b_gameimportance_cat5=0, b_gameimportance_cat6=0,
              b_specialmatch_cat1=0, b_specialmatch_cat2=0, b_specialmatch_cat3=0, b_specialmatch_cat4=0, b_specialmatch_cat5=0, b_specialmatch_cat6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot4", "b_mvmtGK4", "b_standposGK4", "b_ingameshoot4", "b_compleague4", "b_lastpenaltygoal4", "b_opplstpengoal4", "b_shothardness4", "b_tappedball4", "b_lastGKdive4", "b_clubornational4", "b_compNational4", "b_secondlastGKdivedir4", "b_heightGK_cat4", "b_decider_cat4", "b_roundnogroup_cat4", "b_posdef_cat4", "b_GKmvmttyp_cat4","b_gameimportance_cat4", "b_specialmatch_cat4")
#apollo_fixed = c("asc_TC","b_foot2", "b_mvmtGK2", "b_standposGK2", "b_ingameshoot2","b_compleague2", "b_heightGK2", "b_decider2", "b_lastpenaltygoal2", "b_lastpenaldir2","b_saverateGK2", "b_opplstpengoal2","b_roundnogroup2", "b_posdef2", "b_GKmvmttyp2")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_lastGKdive1*lastGKdive_cat + b_clubornational1*clubornational_cat + b_compNational1*compNational_cat + b_secondlastGKdivedir1*secondlastGKdivedir_cat + b_heightGK_cat1*heightGK_cat + b_decider_cat1*decider_cat + b_roundnogroup_cat1*roundnogroup_cat + b_posdef_cat1*posdef_cat + b_GKmvmttyp_cat1*GKmvmttyp_cat + b_gameimportance_cat1*gameimportance_cat + b_specialmatch_cat1*specialmatch_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_lastGKdive2*lastGKdive_cat + b_clubornational2*clubornational_cat + b_compNational2*compNational_cat + b_secondlastGKdivedir2*secondlastGKdivedir_cat + b_heightGK_cat2*heightGK_cat + b_decider_cat2*decider_cat + b_roundnogroup_cat2*roundnogroup_cat + b_posdef_cat2*posdef_cat + b_GKmvmttyp_cat2*GKmvmttyp_cat + b_gameimportance_cat2*gameimportance_cat + b_specialmatch_cat2*specialmatch_cat
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_lastGKdive3*lastGKdive_cat + b_clubornational3*clubornational_cat + b_compNational3*compNational_cat + b_secondlastGKdivedir3*secondlastGKdivedir_cat + b_heightGK_cat3*heightGK_cat + b_decider_cat3*decider_cat + b_roundnogroup_cat3*roundnogroup_cat + b_posdef_cat3*posdef_cat + b_GKmvmttyp_cat3*GKmvmttyp_cat + b_gameimportance_cat3*gameimportance_cat + b_specialmatch_cat3*specialmatch_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_lastGKdive4*lastGKdive_cat + b_clubornational4*clubornational_cat + b_compNational4*compNational_cat + b_secondlastGKdivedir4*secondlastGKdivedir_cat + b_heightGK_cat4*heightGK_cat + b_decider_cat4*decider_cat + b_roundnogroup_cat4*roundnogroup_cat + b_posdef_cat4*posdef_cat + b_GKmvmttyp_cat4*GKmvmttyp_cat + b_gameimportance_cat4*gameimportance_cat + b_specialmatch_cat4*specialmatch_cat
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_lastGKdive5*lastGKdive_cat + b_clubornational5*clubornational_cat + b_compNational5*compNational_cat + b_secondlastGKdivedir5*secondlastGKdivedir_cat + b_heightGK_cat5*heightGK_cat + b_decider_cat5*decider_cat + b_roundnogroup_cat5*roundnogroup_cat + b_posdef_cat5*posdef_cat + b_GKmvmttyp_cat5*GKmvmttyp_cat + b_gameimportance_cat5*gameimportance_cat + b_specialmatch_cat5*specialmatch_cat
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_lastGKdive6*lastGKdive_cat + b_clubornational6*clubornational_cat + b_compNational6*compNational_cat + b_secondlastGKdivedir6*secondlastGKdivedir_cat + b_heightGK_cat6*heightGK_cat + b_decider_cat6*decider_cat + b_roundnogroup_cat6*roundnogroup_cat + b_posdef_cat6*posdef_cat + b_GKmvmttyp_cat6*GKmvmttyp_cat + b_gameimportance_cat6*gameimportance_cat + b_specialmatch_cat6*specialmatch_cat
  
  
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

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model6 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model6)

forecast6 = apollo_prediction(model6,
                                apollo_probabilities,
                                apollo_inputs)

apollo_lrTest(bl_2, bl_4)