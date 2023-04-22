#### Model1 ####
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_6_alt_modelwithts",
  modelDescr ="Simple MNL model on penalty data with ts",
  indivID    = "ID"
)

##################################################################################################################

database = read.csv("6altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)

database$shothardnessnew_cat <- as.numeric(as.character(as.factor(ifelse(database$Shothardness == "strong", 1, 0))))


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
              b_roundnogroup1=0,b_roundnogroup2=0,b_roundnogroup3=0,b_roundnogroup4=0,b_roundnogroup5=0,b_roundnogroup6=0,
              b_GKmvmttyp1=0,b_GKmvmttyp2=0,b_GKmvmttyp3=0,b_GKmvmttyp4=0,b_GKmvmttyp5=0,b_GKmvmttyp6=0,
              b_ts1=0, b_ts2=0, b_ts3=0, b_ts4=0, b_ts5=0, b_ts6=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC", "b_foot5", "b_mvmtGK5","b_standposGK5","b_ingameshoot5","b_compleague5","b_lastpenaltygoal5","b_opplstpengoal5","b_shothardness5","b_tappedball5","b_roundnogroup5","b_GKmvmttyp5", "b_ts5")
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
  V[['TL']]  = asc_TL + b_foot1*foot_cat + b_mvmtGK1*mvmtGK_cat + b_standposGK1*standposGK_cat + b_ingameshoot1*ingameshoot_cat + b_compleague1*compleague_cat + b_lastpenaltygoal1*lastpenaltygoal_cat + b_opplstpengoal1*opplstpengoal_cat + b_shothardness1*shothardness_cat + b_tappedball1*tappedball_cat + b_roundnogroup1*roundnogroup_cat + b_GKmvmttyp1*GKmvmttyp_cat + b_ts1*tappedball_cat*shothardnessnew_cat
  V[['TC']]  = asc_TC + b_foot2*foot_cat + b_mvmtGK2*mvmtGK_cat + b_standposGK2*standposGK_cat + b_ingameshoot2*ingameshoot_cat + b_compleague2*compleague_cat + b_lastpenaltygoal2*lastpenaltygoal_cat + b_opplstpengoal2*opplstpengoal_cat + b_shothardness2*shothardness_cat + b_tappedball2*tappedball_cat + b_roundnogroup2*roundnogroup_cat + b_GKmvmttyp2*GKmvmttyp_cat + b_ts2*tappedball_cat*shothardnessnew_cat  
  V[['TR']]  = asc_TR + b_foot3*foot_cat + b_mvmtGK3*mvmtGK_cat + b_standposGK3*standposGK_cat + b_ingameshoot3*ingameshoot_cat + b_compleague3*compleague_cat + b_lastpenaltygoal3*lastpenaltygoal_cat + b_opplstpengoal3*opplstpengoal_cat + b_shothardness3*shothardness_cat + b_tappedball3*tappedball_cat + b_roundnogroup3*roundnogroup_cat + b_GKmvmttyp3*GKmvmttyp_cat + b_ts3*tappedball_cat*shothardnessnew_cat
  V[['DL']]  = asc_DL + b_foot4*foot_cat + b_mvmtGK4*mvmtGK_cat + b_standposGK4*standposGK_cat + b_ingameshoot4*ingameshoot_cat + b_compleague4*compleague_cat + b_lastpenaltygoal4*lastpenaltygoal_cat + b_opplstpengoal4*opplstpengoal_cat + b_shothardness4*shothardness_cat + b_tappedball4*tappedball_cat + b_roundnogroup4*roundnogroup_cat + b_GKmvmttyp4*GKmvmttyp_cat + b_ts4*tappedball_cat*shothardnessnew_cat  
  V[['DC']]  = asc_DC + b_foot5*foot_cat + b_mvmtGK5*mvmtGK_cat + b_standposGK5*standposGK_cat + b_ingameshoot5*ingameshoot_cat + b_compleague5*compleague_cat + b_lastpenaltygoal5*lastpenaltygoal_cat + b_opplstpengoal5*opplstpengoal_cat + b_shothardness5*shothardness_cat + b_tappedball5*tappedball_cat + b_roundnogroup5*roundnogroup_cat + b_GKmvmttyp5*GKmvmttyp_cat + b_ts5*tappedball_cat*shothardnessnew_cat  
  V[['DR']]  = asc_DR + b_foot6*foot_cat + b_mvmtGK6*mvmtGK_cat + b_standposGK6*standposGK_cat + b_ingameshoot6*ingameshoot_cat + b_compleague6*compleague_cat + b_lastpenaltygoal6*lastpenaltygoal_cat + b_opplstpengoal6*opplstpengoal_cat + b_shothardness6*shothardness_cat + b_tappedball6*tappedball_cat + b_roundnogroup6*roundnogroup_cat + b_GKmvmttyp6*GKmvmttyp_cat + b_ts6*tappedball_cat*shothardnessnew_cat
  
  
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


modelwithts = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(modelwithts)

#apollo_saveOutput(modelwithts)
#apollo_lrTest(modelNew, modelwithts)


apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(estimationRoutine = "bfgs", maxIterations = 200, writeIter =
                             FALSE, hessianRoutine = "none", printLevel = 3L, silent = TRUE),
  outOfSample_settings = list(nRep = 10, validationSize = 0.1, samples = NA)
)

forecastts = apollo_prediction(modelwithts,
                                apollo_probabilities,
                                apollo_inputs)

forecastts

TLpred = mean(forecastts$TL) * 100
TLpred

TCpred = mean(forecastts$TC) * 100
TCpred

TRpred = mean(forecastts$TR) * 100
TRpred

DLpred = mean(forecastts$DL) * 100
DLpred

DCpred = mean(forecastts$DC) * 100
DCpred

DRpred = mean(forecastts$DR) * 100
DRpred


#max(forecastNew[1])

typeof(forecastts)

sapply(forecastts, "[", 1) 


probNMwo3 <- forecastts
red <- c(1,2,9)
reddataNMwo3 <- forecastts[-red]

reddataNMwo3

counterTrue = 0
counterFalse = 0

library(dplyr)
probsNMwo3 <- mutate(probNMwo3, correct = 4, incorrect = 5 )
probsNMwo3

for (i in 1:2358) {
  if (max(reddataNMwo3[i,]) == probNMwo3[i,9])
  {probsNMwo3[i,10] <- 1 ; probsNMwo3[i,11] <- 0}
  else {probsNMwo3[i,10] <- 0 ; probsNMwo3[i,11] <- 1}
}

countCorNMwo3 <- sum(probsNMwo3[,10])
countIncorNMwo3 <- sum(probsNMwo3[,11])

for (i in 1:2358) {
  if (max(reddataNMwo3[i,]) == probNMwo3[i,9])
  {probsNMwo3[i,10] <- 1 ; probsNMwo3[i,11] <- 0}
  else {probsNMwo3[i,10] <- 0 ; probsNMwo3[i,11] <- 1}
}
probsNMwo3


countCorNMwo3 <- sum(probsNMwo3[,10])
countCorNMwo3
countIncorNMwo3 <- sum(probsNMwo3[,11])
countIncorNMwo3

perc_crct <- countCorNMwo3 / 2358
perc_crct *100
perc_incor <- countIncorNMwo3 / 2358
perc_incor * 100

percTrueNMwo3 = (countCorNMwo3/(countCorNMwo3+countIncorNMwo3))*100
percTrueNMwo3
percFalseNMwo3 = (countIncorNMwo3/(countCorNMwo3+countIncorNMwo3))*100
percFalseNMwo3


IDcount = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 2)
  {IDcount = c(IDcount, probNMwo3[i,1])}
}

probNMwo3[8,2]
probNMwo3


probNMwo3 <- cbind(probNMwo3, database$playerid)
probNMwo3

names(probNMwo3)[names(probNMwo3) == 'database$playerid'] <- "playerid"
count(probNMwo3$playerid == 6)
str(probNMwo3)


probNMwo3$countofid <- probNMwo3 %>% 
  count(playerid)


testdata = read.csv("Test_file.csv",header=TRUE)

fos = apollo_prediction(
  modelwithts,
  apollo_probabilities,
  apollo_inputs,
  prediction_settings = testdata
)

apollo_classAlloc(modelwithts)
