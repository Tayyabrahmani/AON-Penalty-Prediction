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
  modelName  ="Apollo_9_alt_model1",
  modelDescr ="Simple MNL model on penalty data 9 alt",
  indivID    = "ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("9altallcolumn.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(TL=1, TC=2, TR=3, ML=4, MC=5, MR=6, DL=7, DC=8, DR=9),
  avail = 1,
  choiceVar    = database$Choice,
  explanators  = database[,c("foot_cat", "mvmtGK_cat", "standposGK_cat", "ingameshoot_cat", "compleague_cat", "heightGK_cat", "decider_cat", "lastpenaltygoal_cat",
                             "roundnogroup_cat", "opplstpengoal_cat", "posdef_cat", "GKmvmttyp_cat", "shothardness_cat", "tappedball_cat", "gameimportance_cat",
                             "specialmatch_cat", "lastGKdive_cat", "clubornational_cat", "compCup_cat", "compECC_cat", "compNational_cat", "compQuali_cat",
                             "jerseygreyGK_cat", "jerseyredGK_cat", "jerseyblackGK_cat", "jerseyblueGK_cat", "jerseygreenGK_cat", "jerseyyellowGK_cat", "jerseyorangeGK_cat",
                             "jerseygoldPL_cat", "jerseyblackPL_cat", "jerseywhitePL_cat", "jerseypinkPL_cat", "jerseypurplePL_cat", "jerseyredPL_cat", "jerseygreenPL_cat",
                             "secondlastGKdivedir_cat", "secondlastpen_cat")]
)


apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

output_9alt=apollo_choiceAnalysis(choiceAnalysis_settings,apollo_control,database)

