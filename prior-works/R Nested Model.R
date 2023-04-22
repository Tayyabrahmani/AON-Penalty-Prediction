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
  modelName  ="Apollo_Nested_Model",
  modelDescr ="Simple NL model on penalty data",
  indivID    ="ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv2("DataSetModel0NA6Alt.csv",header=TRUE)
colnames(database)[1] = "ID"
### Use only specific data
#database2 = subset(database2,database2$RP==1)

### Create new variable
#database$missprob.1 = 100*database$missprob.1
#database$missprob.2 = 100*database$missprob.2

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(TL=1, TC=2, TR=3,  DL=4, DC=5, DR=6),
  avail = 1,
  choiceVar    = database$Choice,
  explanators  = database[,c("foot")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

output=apollo_choiceAnalysis(choiceAnalysis_settings,apollo_control,database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_TL = 0, asc_TC = 0, asc_TR = 0, asc_DL = 0,asc_DC  = 0, asc_DR = 0, 
              #
              b_foot1=0, b_foot2=0, b_foot3=0,
              #
              b_moveGK1=0, b_moveGK2=0, b_moveGK3=0, b_moveGK4=0, b_moveGK5=0, b_moveGK6=0,
              #
              b_GKS1=0, b_GKS2=0, b_GKS3=0, b_GKS4=0, b_GKS5=0, b_GKS6=0,
              #
              b_IS1=0, b_IS2=0, b_IS3=0, b_IS4=0, b_IS5=0, b_IS6=0,
              # 
              b_comL1=0, b_comL2=0, b_comL3=0, b_comL4=0,b_comL5=0, b_comL6=0,
              #
              b_HGK1=0, b_HGK2=0, b_HGK3=0, b_HGK4=0,b_HGK5=0, b_HGK6=0,
              #
              b_dec1=0, b_dec2=0, b_dec3=0, b_dec4=0,b_dec5=0, b_dec6=0,
              #
              b_lp1=0, b_lp2=0, b_lp3=0, b_lp4=0,b_lp5=0, b_lp6=0,
              #
              b_latr1=0, b_latr2=0, b_latr3=0, b_latr4=0,b_latr5=0, b_latr6=0,
              #
              b_ladr1=0, b_ladr2=0, b_ladr3=0, b_ladr4=0,b_ladr5=0, b_ladr6=0,
              #
              b_sr1 = 0,  b_sr3 = 0,
              #
              b_perc=0,
              #
              b_rnog1=0, b_rnog2=0, b_rnog3=0, b_rnog4=0,b_rnog5=0, b_rnog6=0,
              #
              b_solsbg1=0, b_solsbg2=0, b_solsbg3=0, b_solsbg4=0,b_solsbg5=0, b_solsbg6=0,
              #
              b_nmovGK1=0, b_nmovGK2=0, b_nmovGK3=0, b_nmovGK4=0, b_nmovGK5=0, b_nmovGK6=0,
              #
              b_def1=0, b_def2=0, b_def3=0, b_def4=0, b_def5=0, b_def6=0,
              #
              lambda_L = 0.95, lambda_C = 0.95, lambda_R=0.95)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_DC","b_foot2", "b_moveGK5", "b_GKS5", "b_IS5","b_comL5", "b_HGK5", "b_dec5", "b_lp5", "b_latr5", "b_ladr5", "b_nmovGK5", "b_rnog5", "b_solsbg5", "b_def5")

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
  V[['TL']]  = asc_TL + b_foot1*foot + b_moveGK1*moveGK  + b_GKS1*GKS + b_IS1*IngSo + b_comL1*comLea + b_HGK1*HeiGK + b_dec1*Dec + b_lp1*lpbg  + b_latr1*la3 + b_ladr1*la6 + b_sr1*SRGK1  + b_perc*perc1   + b_rnog1*rnoGroup + b_solsbg1*Solsbg  + b_def1*def + b_nmovGK1*nmovGK
  V[['TC']]  = asc_TC + b_foot2*foot + b_moveGK2*moveGK  + b_GKS2*GKS + b_IS2*IngSo + b_comL2*comLea + b_HGK2*HeiGK + b_dec2*Dec + b_lp2*lpbg  + b_latr2*la3 + b_ladr2*la6                + b_perc*perc2   + b_rnog2*rnoGroup + b_solsbg2*Solsbg  + b_def2*def + b_nmovGK2*nmovGK 
  V[['TR']]  = asc_TR + b_foot3*foot + b_moveGK3*moveGK  + b_GKS3*GKS + b_IS3*IngSo + b_comL3*comLea + b_HGK3*HeiGK + b_dec3*Dec + b_lp3*lpbg  + b_latr3*la3 + b_ladr3*la6 + b_sr3*SRGK3  + b_perc*perc3   + b_rnog3*rnoGroup + b_solsbg3*Solsbg  + b_def3*def + b_nmovGK3*nmovGK 
  V[['DL']]  = asc_DL + b_foot1*foot + b_moveGK4*moveGK  + b_GKS4*GKS + b_IS4*IngSo + b_comL4*comLea + b_HGK4*HeiGK + b_dec4*Dec + b_lp4*lpbg  + b_latr4*la3 + b_ladr4*la6                + b_perc*perc4   + b_rnog4*rnoGroup + b_solsbg4*Solsbg  + b_def4*def + b_nmovGK4*nmovGK 
  V[['DC']]  = asc_DC + b_foot2*foot + b_moveGK5*moveGK  + b_GKS5*GKS + b_IS5*IngSo + b_comL5*comLea + b_HGK5*HeiGK + b_dec5*Dec + b_lp5*lpbg  + b_latr5*la3 + b_ladr5*la6                + b_perc*perc5   + b_rnog5*rnoGroup + b_solsbg5*Solsbg  + b_def5*def + b_nmovGK5*nmovGK 
  V[['DR']]  = asc_DR + b_foot3*foot + b_moveGK6*moveGK  + b_GKS6*GKS + b_IS6*IngSo + b_comL6*comLea + b_HGK6*HeiGK + b_dec6*Dec + b_lp6*lpbg  + b_latr6*la3 + b_ladr6*la6                + b_perc*perc6   + b_rnog6*rnoGroup + b_solsbg6*Solsbg  + b_def6*def + b_nmovGK6*nmovGK
  
  ### Specify nests for NL model
  nlNests  = list(root=1, L=lambda_L, C=lambda_C, R=lambda_R)
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]]   = c("L","C","R")
  nlStructure[["L"]]      = c("TL","DL")
  nlStructure[["C"]]      = c("TC","DC")
  nlStructure[["R"]]      = c("TR","DR")
  
  ### Define settings for NL model component
  nl_settings = list(
    alternatives  = c(TL=1, TC=2, TR=3, DL=4, DC=5, DR=6), 
    avail         = list(TL=av_1, TC=av_2, TR=av_3, DL=av_4, DC=av_5, DR=av_6), 
    choiceVar     = Choice,
    V             = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model3N = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model3N)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model3N)

forecast3N = apollo_prediction(model3N,
                             apollo_probabilities,
                             apollo_inputs)


#apollo_llCalc(apollo_beta,
#              apollo_probabilities,
#              apollo_inputs)


# likelihood ratio test
apollo_lrTest("Apollo_penNM_81","Apollo_testpennested3N6Alt_7")
#apollo_basTest("Apollo_testpen_38", "Apollo_testpen_39")
