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
  modelName  ="Apollo_6_alt_model1",
  modelDescr ="Simple MNL model on penalty data",
  indivID    = "ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# ################################################################# #
#### data preprocessing                       ####
# ################################################################# #
database = read.csv("6altpentalydata.csv",header=TRUE)
names(database)[1] <- "ID"
View(database)

database$foot_cat = as.numeric(as.character(factor(database$foot,
                                                   
                                                   levels = c("L", "R"),
                                                   
                                                   labels = c(0, 1))))

database$mvmtGK_cat <- as.numeric(as.character(as.factor(ifelse(database$moveGK == "no" | database$moveGK == "null", 0,
                                                                ifelse(database$moveGK == "step left" | database$moveGK ==  "links geneigt", 1,
                                                                       ifelse(database$moveGK == "jumping on spot" | database$moveGK == "step forward", 2,
                                                                              ifelse(database$moveGK == "step right" | database$moveGK ==  "rechts geneigt", 3, 4)))))))

database$standposGK_cat <- as.numeric(as.character(as.factor(ifelse(database$GKS == "clearly left" | database$GKS == "slightly left", 1,
                                                                    ifelse(database$GKS == "central", 2,
                                                                           ifelse(database$GKS == "clearly right" | database$GKS == "slightly right", 3, 0))))))


database$ingameshoot_cat = as.numeric(as.character(factor(database$Ingso,
                                                          
                                                          levels = c("Ingame", "Shootout"),
                                                          
                                                          labels = c(0, 1))))


database$compleague_cat <- as.numeric(as.character(as.factor(ifelse(database$Competetion == "League", 1, 0))))


database$decider_cat = as.numeric(as.character(factor(database$Dec,
                                                      
                                                      levels = c("no", "yes"),
                                                      
                                                      labels = c(0, 1))))


database$lastpenaltygoal_cat <- as.numeric(as.character(as.factor(ifelse(database$lpbg == "Missed" | database$lpbg == "Saved", 1,
                                                                         ifelse(database$lpbg == "Goal", 2, 0)))))

database$lastpenaldir_cat = as.numeric(as.character(factor(database$la6,
                                                           
                                                           levels = c("null", "1", "2", "3", "4", "5", "6"),
                                                           
                                                           labels = c(0, 1, 2, 3, 4, 5, 6))))

database$roundnogroup_cat <- as.numeric(as.character(as.factor(ifelse(database$rnoGroup == "Group", 1, 0))))

database$heightGK_cat <- as.numeric(as.character(as.factor(ifelse(database$HeiGK < 183, 1,
                                                                  ifelse(database$HeiGK == 183, 2,
                                                                         ifelse(database$HeiGK == 184, 3,
                                                                                ifelse(database$HeiGK == 185, 4,
                                                                                       ifelse(database$HeiGK == 186, 5,
                                                                                              ifelse(database$HeiGK == 187, 6,
                                                                                                     ifelse(database$HeiGK == 188, 7,
                                                                                                            ifelse(database$HeiGK == 189, 8,
                                                                                                                   ifelse(database$HeiGK == 190, 9,
                                                                                                                          ifelse(database$HeiGK == 191, 10,
                                                                                                                                 ifelse(database$HeiGK == 192, 11,
                                                                                                                                        ifelse(database$HeiGK == 193, 12,
                                                                                                                                               ifelse(database$HeiGK == 194, 13,
                                                                                                                                                      ifelse(database$HeiGK == 195, 14,
                                                                                                                                                             ifelse(database$HeiGK == 196, 15,
                                                                                                                                                                    ifelse(database$HeiGK > 196, 16, 0)))))))))))))))))))

database$posdef_cat<-as.numeric(as.character(as.factor(ifelse(database$positiondef=='DF',1,0))))


database$GKmvmttyp_cat=as.numeric(as.character(factor(database$nmovGK,
                                                      
                                                      levels=c("null","moving","still"),
                                                      
                                                      labels=c(0, 1, 2))))

database$opplstpengoal_cat<-as.numeric(as.character(as.factor(ifelse(database$Solsbgopponent=='Goal',1,
                                                                     ifelse(database$Solsbgopponent=='Missed'| database$Solsbgopponent=='Saved',2,0)))))




database$shothardness_cat<-as.numeric(as.character(as.factor(ifelse(database$Shothardness=='strong',1,
                                                                    ifelse(database$Shothardness=='normal',2, 
                                                                           ifelse(database$Shothardness=='weak',3,0))))))

database$tappedball_cat<-as.numeric(as.character(as.factor(ifelse(database$tappedtheball=='yes',1,0))))

database$gameimportance_cat<-as.numeric(as.character(as.factor(ifelse(database$Importantnessgamegrouped=='normal',1,
                                                                    ifelse(database$Importantnessgamegrouped=='important',2, 
                                                                           ifelse(database$Importantnessgamegrouped=='unimportant',3,0))))))

database$specialmatch_cat<-as.numeric(as.character(as.factor(ifelse(database$specialMatch=='yes',1,0))))

database$lastGKdive_cat<-as.numeric(as.character(as.factor(ifelse(database$lastGKdive=='Left',1, 
                                                                  ifelse(database$lastGKdive=='Center',2, 
                                                                         ifelse(database$lastGKdive=='Right',3,0))))))



database$clubornational_cat<-as.numeric(as.character(as.factor(ifelse(database$CluborNationalTeam=='National Team',1,0))))


database$compCup_cat<-as.numeric(as.character(as.factor(ifelse(database$competition=='Cup'| database$competition=='WMCUP',1,0))))

database$compECC_cat<-as.numeric(as.character(as.factor(ifelse(database$competition=='EL'| database$competition=='UCL',1,0))))

database$compNational_cat<-as.numeric(as.character(as.factor(ifelse(database$competition=='WM'| database$competition=='Copa America'
                                                                    | database$competition=='NL'| database$competition=='Friendly',1,0))))

database$compQuali_cat<-as.numeric(as.character(as.factor(ifelse(database$competition=='EL-Quali'| database$competition=='UCL-Quali'
                                                                 | database$competition=='WM-Quali'| database$competition=='EM-Quali'
                                                                 | database$competition=='Africacup-Quali',1,0))))

database$jerseygreyGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Grey" , 1,0))))

database$jerseyredGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Red" , 1,0))))

database$jerseyblackGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Black" , 1,0))))

database$jerseyblueGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Blue" , 1,0))))

database$jerseygreenGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Green" , 1,0))))

database$jerseyyellowGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Yellow" , 1,0))))

database$jerseyorangeGK_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourGK == "Orange" , 1,0))))

database$jerseygoldPL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Gold" , 1,0))))

database$jerseyblackPL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Black" , 1,0))))

database$jerseywhitePL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "White" , 1,0))))

database$jerseypinkPL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Pink" , 1,0))))

database$jerseypurplePL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Purple" , 1,0))))

database$jerseyredPL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Red" , 1,0))))

database$jerseygreenPL_cat <- as.numeric(as.character(as.factor(ifelse(database$JColourPlayer == "Green" , 1,0))))


database$secondlastGKdivedir_cat <- as.numeric(as.character(as.factor(ifelse(database$secondlastGKdive == "Left" , 1,ifelse(database$secondlastGKdive == "Right",2,ifelse(database$secondlastGKdive == "Center",3,0))))))

database$secondlastpen_cat <- as.numeric(as.character(as.factor(ifelse(database$secondlastpenalty == "Goal" , 1,ifelse(database$secondlastpenalty=="Missed",2,ifelse(database$secondlastpenalty=="Saved",3,0))))))

#database$minute_cat=as.numeric(factor(ifelse(database$Minute < 16,1,
 #                                                         ifelse((database$Minute > 15 && database$Minute < 31), 2,
  #                                                               ifelse((database$Minute > 30 && database$Minute < 46), 3,
   #                                                                     ifelse((database$Minute > 45 && database$Minute < 61), 4,
    #                                                                           ifelse((database$Minute > 60 && database$Minute < 76) | (database$Minute > 90 && database$Minute < 106), 5,
     #                                                                                 ifelse((database$Minute > 75 && database$Minute < 90) | (database$Minute > 105 && database$Minute < 120), 6,
      #                                                                                       ifelse((database$Minute==90 & database$Minute==120),7,0)))))))))


#print(class(database$Minute))

#c <- 

#if(as.numeric()> 0){
 # print("Positive number")
#}

write.csv(database,"6altallcolumn.csv", row.names = FALSE)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(TL=1, TC=2, TR=3,  DL=4, DC=5, DR=6),
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

output=apollo_choiceAnalysis(choiceAnalysis_settings,apollo_control,database)

