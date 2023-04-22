#forecast = apollo_prediction(model,
#                              apollo_probabilities,
#                              apollo_inputs)

#nNM = forecastnNM
#N3wo3var = forecast3N

### investigating the probabilities estimated by the model
probNMwo3 <- nNM
red <- c(1,2,9)
reddataNMwo3 <- probNMwo3[-red]
counterTrue = 0
counterFalse = 0

library(dplyr)
probsNMwo3 <- mutate(probNMwo3, correct = 4, incorrect = 5 )
for (i in 1:2358) {
  if (max(reddataNMwo3[i,]) == probNMwo3[i,9])
  {probsNMwo3[i,10] <- 1 ; probsNMwo3[i,11] <- 0}
  else {probsNMwo3[i,10] <- 0 ; probsNMwo3[i,11] <- 1}
}


### counting the number of true and false assignments, using the maximum likelihood method
#for (i in 1:2362) {
#    if (max(reddata[i,]) == prob[i,12])
#    {counterTrue = counterTrue + 1}
 #   else {counterFalse = counterFalse +1}  
#}
#countCortv <- sum(probs[,10])
#countIncortv <- sum(probs[,11])

## calculate percentage of True/False
#percTruetv = countCortv/(countCortv+countIncortv)
#percFalsetv = countIncortv/(countCortv+countIncortv)

countCorNMwo3 <- sum(probsNMwo3[,10])
countIncorNMwo3 <- sum(probsNMwo3[,11])

## calculate percentage of True/False
percTrueNMwo3 = countCorNMwo3/(countCorNMwo3+countIncorNMwo3)
percFalseNMwo3 = countIncorNMwo3/(countCorNMwo3+countIncorNMwo3)

## considering single penalty takers
#### separate data to penalty takers with only one shot in dataset, and those with more than one
IDcount = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 2)
  {IDcount = c(IDcount, probNMwo3[i,1])}
}
### looking for penalty takers with 5 or more shots in the data set
IDcount05om = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 5)
  {IDcount05om = c(IDcount05om, probNMwo3[i,1])}
}

### looking for penalty takers with 10 or more shots in the data set
IDcount10om = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 10)
  {IDcount10om = c(IDcount10om, probNMwo3[i,1])}
}

### looking for penalty takers with 20 or more shots in the data set
IDcount20om = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 20)
  {IDcount20om = c(IDcount20om, probNMwo3[i,1])}
}

### looking for penalty takers with 30 or more shots in the data set
IDcount30om = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 30)
  {IDcount30om = c(IDcount30om, probNMwo3[i,1])}
}

### looking for penalty takers with 50 or more shots in the data set
IDcount50om = c()
for (i in 1:2358){
  if (probNMwo3[i,2] == 50)
  {IDcount50om = c(IDcount50om, probNMwo3[i,1])}
}

### find out individuals with 30 <number of penalties < 50
help30 <- c()
for (i in 1:19){
  for (j in 1:9){
    if (IDcount30om[i]==IDcount50om[j])
      help30 <- c(help30,i)
  }
}
help30
IDcount30omc <- IDcount30om[-help30]

### find out individuals with 20 <number of penalties < 30
help20 <- c()
for (i in 1:26){
  for (j in 1:19){
    if (IDcount20om[i]==IDcount30om[j])
      help20 <- c(help20,i)
  }
}
help20
IDcount20omc <- IDcount20om[-help20]

### find out individuals with 10 <number of penalties < 20
help10 <- c()
for (i in 1:45){
  for (j in 1:26){
    if (IDcount10om[i]==IDcount20om[j])
      help10 <- c(help10,i)
  }
}
help10
IDcount10omc <- IDcount10om[-help10]

# finding all individuals with 4 < number penalty < 10 
help05 <- c()
for (i in 1:67){
  for (j in 1:45){
    if (IDcount05om[i]==IDcount10om[j])
      help05 <- c(help05,i)
  }
}
help05
IDcount05omc <- IDcount05om[-help05]

## predicting analysis
## Analyze penalty takers with only one shot in dataset
redrow= c()
for (j in 1:161){
  for (i in 1:2358){
    if (probNMwo3[i,1] == IDcount[j]){
      redrow = c(redrow, i)
    }
  }
}
redrowdata <- probsNMwo3[-redrow,]
#redrowdata2 <- redrowdata[-red]
#for (i in 1:324) {
#  if (max(redrowdata2[i,]) == redrowdata[i,12])
#  {counterTrue = counterTrue + 1}
#  else {counterFalse = counterFalse +1}  
#}
redrowdata
coucor1p <- sum(redrowdata[,10])
couincor1p <- sum(redrowdata[,11])
percTrue1p = coucor1p/(coucor1p+couincor1p)
percFalse1p = couincor1p/(coucor1p+couincor1p)

## Analyze penalty takers with  50 or more shots in dataset
rowcount <- c()
for (j in 1:9) {
  for (i in 1:2358) {
    if (probsNMwo3[i,1] == IDcount50om[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data50om <- probsNMwo3[rowcount,]

coucor50om <- sum(data50om[,10])
couincor50om <- sum(data50om[,11]) 
percTrue50om = coucor50om/(coucor50om+couincor50om)
percFalse50om = couincor50om/(coucor50om+couincor50om)

## Analyze penalty takers with  30-49 shots in dataset
rowcount <- c()
for (j in 1:10) {
  for (i in 1:2358) {
    if (probsNMwo3[i,1] == IDcount30omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data30om <- probsNMwo3[rowcount,]

coucor30om <- sum(data30om[,10])
couincor30om <- sum(data30om[,11]) 
percTrue30om = coucor30om/(coucor30om+couincor30om)
percFalse30om = couincor30om/(coucor30om+couincor30om)

## Analyze penalty takers with  20-29 shots in dataset
rowcount <- c()
for (j in 1:7) {
  for (i in 1:2358) {
    if (probsNMwo3[i,1] == IDcount20omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data20om <- probsNMwo3[rowcount,]

coucor20om <- sum(data20om[,10])
couincor20om <- sum(data20om[,11]) 
percTrue20om = coucor20om/(coucor20om+couincor20om)
percFalse20om = couincor20om/(coucor20om+couincor20om)

## Analyze penalty takers with  10-19 shots in dataset
rowcount <- c()
for (j in 1:19) {
  for (i in 1:2358) {
    if (probsNMwo3[i,1] == IDcount10omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data10om <- probsNMwo3[rowcount,]

coucor10om <- sum(data10om[,10])
couincor10om <- sum(data10om[,11]) 
percTrue10om = coucor10om/(coucor10om+couincor10om)
percFalse10om = couincor10om/(coucor10om+couincor10om)

## Analyze penalty takers with  5-9 shots in dataset
rowcount <- c()
for (j in 1:22) {
  for (i in 1:2358) {
    if (probsNMwo3[i,1] == IDcount05omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data05om <- probsNMwo3[rowcount,]

coucor05om <- sum(data05om[,10])
couincor05om <- sum(data05om[,11]) 
percTrue05om = coucor05om/(coucor05om+couincor05om)
percFalse05om = couincor05om/(coucor05om+couincor05om)

### average probability assignment
sumvec <- c()
for (j in 1:6){
  sumvec[j] <- sum(reddataNMwo3[,j])
}
sumvecperc = sumvec/2358
sumvecperc
ts <- sum(sumvecperc)

#################################################################
### Analyze single individuals regarding prediction quality
## individuals #1 Agüero
player1n <- "Agüero"
player1 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[1]){
    player1 <- c(player1, i)
  }
}
player1a <- probsNMwo3[player1,]
ccplayer1 <- sum(player1a[,10])
cicplayer1 <- sum(player1a[,11])
summplayer1 <- ccplayer1+cicplayer1
perccplayer1 <- ccplayer1/summplayer1
percicplayer1 <- cicplayer1/summplayer1
IDcount10om[1]

## individuals #2 Bentaleb
player2n <- "Bentaleb"
player2 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[2]){
    player2 <- c(player2, i)
  }
}
player2a <- probsNMwo3[player2,]
ccplayer2 <- sum(player2a[,10])
cicplayer2 <- sum(player2a[,11])
summplayer2 <- ccplayer2+cicplayer2
perccplayer2 <- ccplayer2/summplayer2
percicplayer2 <- cicplayer2/summplayer2
IDcount10om[2]

## individuals #3 Brosinski
player3n <- "Brosinski"
player3 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[3]){
    player3 <- c(player3, i)
  }
}
player3a <- probsNMwo3[player3,]
ccplayer3 <- sum(player3a[,10])
cicplayer3 <- sum(player3a[,11])
summplayer3 <- ccplayer3+cicplayer3
perccplayer3 <- ccplayer3/summplayer3
percicplayer3 <- cicplayer3/summplayer3
IDcount10om[3]

## individuals #4 Caliguiri
player4n <- "Caliguiri"
player4 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[4]){
    player4 <- c(player4, i)
  }
}
player4a <- probsNMwo3[player4,]
ccplayer4 <- sum(player4a[,10])
cicplayer4 <- sum(player4a[,11])
summplayer4 <- ccplayer4+cicplayer4
perccplayer4 <- ccplayer4/summplayer4
percicplayer4 <- cicplayer4/summplayer4
IDcount10om[4]

## individuals #5 Cavani
player5n <- "Cavani"
player5 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[5]){
    player5 <- c(player5, i)
  }
}
player5a <- probsNMwo3[player5,]
ccplayer5 <- sum(player5a[,10])
cicplayer5 <- sum(player5a[,11])
summplayer5 <- ccplayer5+cicplayer5
perccplayer5 <- ccplayer5/summplayer5
percicplayer5 <- cicplayer5/summplayer5
IDcount10om[5]

## individuals #6 Dybala
player6n <- "Dybala"
player6 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[6]){
    player6 <- c(player6, i)
  }
}
player6a <- probsNMwo3[player6,]
ccplayer6 <- sum(player6a[,10])
cicplayer6 <- sum(player6a[,11])
summplayer6 <- ccplayer6+cicplayer6
perccplayer6 <- ccplayer6/summplayer6
percicplayer6 <- cicplayer6/summplayer6
IDcount10om[6]

## individuals #7 Fekir
player7n <- "Fekir"
player7 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[7]){
    player7 <- c(player7, i)
  }
}
player7a <- probsNMwo3[player7,]
ccplayer7 <- sum(player7a[,10])
cicplayer7 <- sum(player7a[,11])
summplayer7 <- ccplayer7+cicplayer7
perccplayer7 <- ccplayer7/summplayer7
percicplayer7 <- cicplayer7/summplayer7
IDcount10om[7]

## individuals #8 Fernandes
player8n <- "Fernandes"
player8 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[8]){
    player8 <- c(player8, i)
  }
}
player8a <- probsNMwo3[player8,]
ccplayer8 <- sum(player8a[,10])
cicplayer8 <- sum(player8a[,11])
summplayer8 <- ccplayer8+cicplayer8
perccplayer8 <- ccplayer8/summplayer8
percicplayer8 <- cicplayer8/summplayer8
IDcount10om[8]

## individuals #9 Forsberg
player9n <- "Forsberg"
player9 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[9]){
    player9 <- c(player9, i)
  }
}
player9a <- probsNMwo3[player9,]
ccplayer9 <- sum(player9a[,10])
cicplayer9 <- sum(player9a[,11])
summplayer9 <- ccplayer9+cicplayer9
perccplayer9 <- ccplayer9/summplayer9
percicplayer9 <- cicplayer9/summplayer9
IDcount10om[9]

## individuals #10 Griezmann
player10n <- "Griezmann"
player10 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[10]){
    player10 <- c(player10, i)
  }
}
player10a <- probsNMwo3[player10,]
ccplayer10 <- sum(player10a[,10])
cicplayer10 <- sum(player10a[,11])
summplayer10 <- ccplayer10+cicplayer10
perccplayer10 <- ccplayer10/summplayer10
percicplayer10 <- cicplayer10/summplayer10
IDcount10om[10]

## individuals #11 Gündogan
player11n <- "Gündogan"
player11 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[11]){
    player11 <- c(player11, i)
  }
}
player11a <- probsNMwo3[player11,]
ccplayer11 <- sum(player11a[,10])
cicplayer11 <- sum(player11a[,11])
summplayer11 <- ccplayer11+cicplayer11
perccplayer11 <- ccplayer11/summplayer11
percicplayer11 <- cicplayer11/summplayer11
IDcount10om[11]

## individuals #12 Haaland
player12n <- "Haaland"
player12 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[12]){
    player12 <- c(player12, i)
  }
}
player12a <- probsNMwo3[player12,]
ccplayer12 <- sum(player12a[,10])
cicplayer12 <- sum(player12a[,11])
summplayer12 <- ccplayer12+cicplayer12
perccplayer12 <- ccplayer12/summplayer12
percicplayer12 <- cicplayer12/summplayer12
IDcount10om[12]

## individuals #13 Havertz
player13n <- "Havertz"
player13 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[13]){
    player13 <- c(player13, i)
  }
}
player13a <- probsNMwo3[player13,]
ccplayer13 <- sum(player13a[,10])
cicplayer13 <- sum(player13a[,11])
summplayer13 <- ccplayer13+cicplayer13
perccplayer13 <- ccplayer13/summplayer13
percicplayer13 <- cicplayer13/summplayer13
IDcount10om[13]

## individuals #14 Ibrahimovic
player14n <- "Ibrahimovic"
player14 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[14]){
    player14 <- c(player14, i)
  }
}
player14a <- probsNMwo3[player14,]
ccplayer14 <- sum(player14a[,10])
cicplayer14 <- sum(player14a[,11])
summplayer14 <- ccplayer14+cicplayer14
perccplayer14 <- ccplayer14/summplayer14
percicplayer14 <- cicplayer14/summplayer14
IDcount10om[14]

## individuals #15 Immobile
player15n <- "Immobile"
player15 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[15]){
    player15 <- c(player15, i)
  }
}
player15a <- probsNMwo3[player15,]
ccplayer15 <- sum(player15a[,10])
cicplayer15 <- sum(player15a[,11])
summplayer15 <- ccplayer15+cicplayer15
perccplayer15 <- ccplayer15/summplayer15
percicplayer15 <- cicplayer15/summplayer15
IDcount10om[15]

## individuals #16 James Rodriguez
player16n <- "James Rodriguez"
player16 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[16]){
    player16 <- c(player16, i)
  }
}
player16a <- probsNMwo3[player16,]
ccplayer16 <- sum(player16a[,10])
cicplayer16 <- sum(player16a[,11])
summplayer16 <- ccplayer16+cicplayer16
perccplayer16 <- ccplayer16/summplayer16
percicplayer16 <- cicplayer16/summplayer16
IDcount10om[16]

## individuals #17 Jorginho
player17n <- "Jorginho"
player17 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[17]){
    player17 <- c(player17, i)
  }
}
player17a <- probsNMwo3[player17,]
ccplayer17 <- sum(player17a[,10])
cicplayer17 <- sum(player17a[,11])
summplayer17 <- ccplayer17+cicplayer17
perccplayer17 <- ccplayer17/summplayer17
percicplayer17 <- cicplayer17/summplayer17
IDcount10om[17]

## individuals #18 Kane
player18n <- "Kane"
player18 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[18]){
    player18 <- c(player18, i)
  }
}
player18a <- probsNMwo3[player18,]
ccplayer18 <- sum(player18a[,10])
cicplayer18 <- sum(player18a[,11])
summplayer18 <- ccplayer18+cicplayer18
perccplayer18 <- ccplayer18/summplayer18
percicplayer18 <- cicplayer18/summplayer18
IDcount10om[18]

## individuals #19 Kessie
player19n <- "Kessie"
player19 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[19]){
    player19 <- c(player19, i)
  }
}
player19a <- probsNMwo3[player19,]
ccplayer19 <- sum(player19a[,10])
cicplayer19 <- sum(player19a[,11])
summplayer19 <- ccplayer19+cicplayer19
perccplayer19 <- ccplayer19/summplayer19
percicplayer19 <- cicplayer19/summplayer19
IDcount10om[19]

## individuals #20 Kolarov
player20n <- "Kolarov"
player20 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[20]){
    player20 <- c(player20, i)
  }
}
player20a <- probsNMwo3[player20,]
ccplayer20 <- sum(player20a[,10])
cicplayer20 <- sum(player20a[,11])
summplayer20 <- ccplayer20+cicplayer20
perccplayer20 <- ccplayer20/summplayer20
percicplayer20 <- cicplayer20/summplayer20
IDcount10om[20]

## individuals #21 Kramaric
player21n <- "Kramaric"
player21 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[21]){
    player21 <- c(player21, i)
  }
}
player21a <- probsNMwo3[player21,]
ccplayer21 <- sum(player21a[,10])
cicplayer21 <- sum(player21a[,11])
summplayer21 <- ccplayer21+cicplayer21
perccplayer21 <- ccplayer21/summplayer21
percicplayer21 <- cicplayer21/summplayer21
IDcount10om[21]

## individuals #22 Kruse
player22n <- "Kruse"
player22 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[22]){
    player22 <- c(player22, i)
  }
}
player22a <- probsNMwo3[player22,]
ccplayer22 <- sum(player22a[,10])
cicplayer22 <- sum(player22a[,11])
summplayer22 <- ccplayer22+cicplayer22
perccplayer22 <- ccplayer22/summplayer22
percicplayer22 <- cicplayer22/summplayer22
IDcount10om[22]

## individuals #23 Lewandowski
player23n <- "Lewandowski"
player23 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[23]){
    player23 <- c(player23, i)
  }
}
player23a <- probsNMwo3[player23,]
ccplayer23 <- sum(player23a[,10])
cicplayer23 <- sum(player23a[,11])
summplayer23 <- ccplayer23+cicplayer23
perccplayer23 <- ccplayer23/summplayer23
percicplayer23 <- cicplayer23/summplayer23
IDcount10om[23]

## individuals #24 Ljajic
player24n <- "Ljajic"
player24 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[24]){
    player24 <- c(player24, i)
  }
}
player24a <- probsNMwo3[player24,]
ccplayer24 <- sum(player24a[,10])
cicplayer24 <- sum(player24a[,11])
summplayer24 <- ccplayer24+cicplayer24
perccplayer24 <- ccplayer24/summplayer24
percicplayer24 <- cicplayer24/summplayer24
IDcount10om[24]

## individuals #25 Lukaku
player25n <- "Lukaku"
player25 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[25]){
    player25 <- c(player25, i)
  }
}
player25a <- probsNMwo3[player25,]
ccplayer25 <- sum(player25a[,10])
cicplayer25 <- sum(player25a[,11])
summplayer25 <- ccplayer25+cicplayer25
perccplayer25 <- ccplayer25/summplayer25
percicplayer25 <- cicplayer25/summplayer25
IDcount10om[25]

## individuals #26 Martial
player26n <- "Martial"
player26 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[26]){
    player26 <- c(player26, i)
  }
}
player26a <- probsNMwo3[player26,]
ccplayer26 <- sum(player26a[,10])
cicplayer26 <- sum(player26a[,11])
summplayer26 <- ccplayer26+cicplayer26
perccplayer26 <- ccplayer26/summplayer26
percicplayer26 <- cicplayer26/summplayer26
IDcount10om[26]

## individuals #27 Mbappe
player27n <- "Mbappe"
player27 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[27]){
    player27 <- c(player27, i)
  }
}
player27a <- probsNMwo3[player27,]
ccplayer27 <- sum(player27a[,10])
cicplayer27 <- sum(player27a[,11])
summplayer27 <- ccplayer27+cicplayer27
perccplayer27 <- ccplayer27/summplayer27
percicplayer27 <- cicplayer27/summplayer27
IDcount10om[27]

## individuals #28 Messi
player28n <- "Messi"
player28 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[28]){
    player28 <- c(player28, i)
  }
}
player28a <- probsNMwo3[player28,]
ccplayer28 <- sum(player28a[,10])
cicplayer28 <- sum(player28a[,11])
summplayer28 <- ccplayer28+cicplayer28
perccplayer28 <- ccplayer28/summplayer28
percicplayer28 <- cicplayer28/summplayer28
IDcount10om[28]

## individuals #29 Modric
player29n <- "Modric"
player29 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[29]){
    player29 <- c(player29, i)
  }
}
player29a <- probsNMwo3[player29,]
ccplayer29 <- sum(player29a[,10])
cicplayer29 <- sum(player29a[,11])
summplayer29 <- ccplayer29+cicplayer29
perccplayer29 <- ccplayer29/summplayer29
percicplayer29 <- cicplayer29/summplayer29
IDcount10om[29]

## individuals #30 Müller
player30n <- "Müller"
player30 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[30]){
    player30 <- c(player30, i)
  }
}
player30a <- probsNMwo3[player30,]
ccplayer30 <- sum(player30a[,10])
cicplayer30 <- sum(player30a[,11])
summplayer30 <- ccplayer30+cicplayer30
perccplayer30 <- ccplayer30/summplayer30
percicplayer30 <- cicplayer30/summplayer30
IDcount10om[30]

## individuals #31 Niang
player31n <- "Niang"
player31 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[31]){
    player31 <- c(player31, i)
  }
}
player31a <- probsNMwo3[player31,]
ccplayer31 <- sum(player31a[,10])
cicplayer31 <- sum(player31a[,11])
summplayer31 <- ccplayer31+cicplayer31
perccplayer31 <- ccplayer31/summplayer31
percicplayer31 <- cicplayer31/summplayer31
IDcount10om[31]

## individuals #32 Oyarzabal
player32n <- "Oyarzabal"
player32 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[32]){
    player32 <- c(player32, i)
  }
}
player32a <- probsNMwo3[player32,]
ccplayer32 <- sum(player32a[,10])
cicplayer32 <- sum(player32a[,11])
summplayer32 <- ccplayer32+cicplayer32
perccplayer32 <- ccplayer32/summplayer32
percicplayer32 <- cicplayer32/summplayer32
IDcount10om[32]

## individuals #33 Parejo
player33n <- "Parejo"
player33 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[33]){
    player33 <- c(player33, i)
  }
}
player33a <- probsNMwo3[player33,]
ccplayer33 <- sum(player33a[,10])
cicplayer33 <- sum(player33a[,11])
summplayer33 <- ccplayer33+cicplayer33
perccplayer33 <- ccplayer33/summplayer33
percicplayer33 <- cicplayer33/summplayer33
IDcount10om[33]

## individuals #34 Ramos
player34n <- "Ramos"
player34 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[34]){
    player34 <- c(player34, i)
  }
}
player34a <- probsNMwo3[player34,]
ccplayer34 <- sum(player34a[,10])
cicplayer34 <- sum(player34a[,11])
summplayer34 <- ccplayer34+cicplayer34
perccplayer34 <- ccplayer34/summplayer34
percicplayer34 <- cicplayer34/summplayer34
IDcount10om[34]

## individuals #35 Rashford
player35n <- "Rashford"
player35 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[35]){
    player35 <- c(player35, i)
  }
}
player35a <- probsNMwo3[player35,]
ccplayer35 <- sum(player35a[,10])
cicplayer35 <- sum(player35a[,11])
summplayer35 <- ccplayer35+cicplayer35
perccplayer35 <- ccplayer35/summplayer35
percicplayer35 <- cicplayer35/summplayer35
IDcount10om[35]

## individuals #36 Ronaldo
player36n <- "Ronaldo"
player36 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[36]){
    player36 <- c(player36, i)
  }
}
player36a <- probsNMwo3[player36,]
ccplayer36 <- sum(player36a[,10])
cicplayer36 <- sum(player36a[,11])
summplayer36 <- ccplayer36+cicplayer36
perccplayer36 <- ccplayer36/summplayer36
percicplayer36 <- cicplayer36/summplayer36
IDcount10om[36]

## individuals #37 Salah
player37n <- "Salah"
player37 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[37]){
    player37 <- c(player37, i)
  }
}
player37a <- probsNMwo3[player37,]
ccplayer37 <- sum(player37a[,10])
cicplayer37 <- sum(player37a[,11])
summplayer37 <- ccplayer37+cicplayer37
perccplayer37 <- ccplayer37/summplayer37
percicplayer37 <- cicplayer37/summplayer37
IDcount10om[37]

## individuals #38 Andre Silva
player38n <- "Andre Silva"
player38 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[38]){
    player38 <- c(player38, i)
  }
}
player38a <- probsNMwo3[player38,]
ccplayer38 <- sum(player38a[,10])
cicplayer38 <- sum(player38a[,11])
summplayer38 <- ccplayer38+cicplayer38
perccplayer38 <- ccplayer38/summplayer38
percicplayer38 <- cicplayer38/summplayer38
IDcount10om[38]

## individuals #39 Stindl
player39n <- "Stindl"
player39 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[39]){
    player39 <- c(player39, i)
  }
}
player39a <- probsNMwo3[player39,]
ccplayer39 <- sum(player39a[,10])
cicplayer39 <- sum(player39a[,11])
summplayer39 <- ccplayer39+cicplayer39
perccplayer39 <- ccplayer39/summplayer39
percicplayer39 <- cicplayer39/summplayer39
IDcount10om[39]

## individuals #40 Suarez
player40n <- "Suarez"
player40 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[40]){
    player40 <- c(player40, i)
  }
}
player40a <- probsNMwo3[player40,]
ccplayer40 <- sum(player40a[,10])
cicplayer40 <- sum(player40a[,11])
summplayer40 <- ccplayer40+cicplayer40
perccplayer40 <- ccplayer40/summplayer40
percicplayer40 <- cicplayer40/summplayer40
IDcount10om[40]

## individuals #41 Telles
player41n <- "Telles"
player41 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[41]){
    player41 <- c(player41, i)
  }
}
player41a <- probsNMwo3[player41,]
ccplayer41 <- sum(player41a[,10])
cicplayer41 <- sum(player41a[,11])
summplayer41 <- ccplayer41+cicplayer41
perccplayer41 <- ccplayer41/summplayer41
percicplayer41 <- cicplayer41/summplayer41
IDcount10om[41]

## individuals #42 Vardy
player42n <- "Vardy"
player42 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[42]){
    player42 <- c(player42, i)
  }
}
player42a <- probsNMwo3[player42,]
ccplayer42 <- sum(player42a[,10])
cicplayer42 <- sum(player42a[,11])
summplayer42 <- ccplayer42+cicplayer42
perccplayer42 <- ccplayer42/summplayer42
percicplayer42 <- cicplayer42/summplayer42
IDcount10om[42]

## individuals #43 Vidal
player43n <- "Vidal"
player43 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[43]){
    player43 <- c(player43, i)
  }
}
player43a <- probsNMwo3[player43,]
ccplayer43 <- sum(player43a[,10])
cicplayer43 <- sum(player43a[,11])
summplayer43 <- ccplayer43+cicplayer43
perccplayer43 <- ccplayer43/summplayer43
percicplayer43 <- cicplayer43/summplayer43
IDcount10om[43]

## individuals #44 Waris
player44n <- "Waris"
player44 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[44]){
    player44 <- c(player44, i)
  }
}
player44a <- probsNMwo3[player44,]
ccplayer44 <- sum(player44a[,10])
cicplayer44 <- sum(player44a[,11])
summplayer44 <- ccplayer44+cicplayer44
perccplayer44 <- ccplayer44/summplayer44
percicplayer44 <- cicplayer44/summplayer44
IDcount10om[44]

## individuals #45 Weghorst
player45n <- "Weghorst"
player45 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount10om[45]){
    player45 <- c(player45, i)
  }
}
player45a <- probsNMwo3[player45,]
ccplayer45 <- sum(player45a[,10])
cicplayer45 <- sum(player45a[,11])
summplayer45 <- ccplayer45+cicplayer45
perccplayer45 <- ccplayer45/summplayer45
percicplayer45 <- cicplayer45/summplayer45
IDcount10om[45]

## individuals #46 Alario
player46n <- "Alario"
player46 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[1]){
    player46 <- c(player46, i)
  }
}
player46a <- probsNMwo3[player46,]
ccplayer46 <- sum(player46a[,10])
cicplayer46 <- sum(player46a[,11])
summplayer46 <- ccplayer46+cicplayer46
perccplayer46 <- ccplayer46/summplayer46
percicplayer46 <- cicplayer46/summplayer46
IDcount05omc[1]

## individuals #47 Behrens
player47n <- "Behrens"
player47 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[2]){
    player47 <- c(player47, i)
  }
}
player47a <- probsNMwo3[player47,]
ccplayer47 <- sum(player47a[,10])
cicplayer47 <- sum(player47a[,11])
summplayer47 <- ccplayer47+cicplayer47
perccplayer47 <- ccplayer47/summplayer47
percicplayer47 <- cicplayer47/summplayer47
IDcount05omc[2]

## individuals #48 Bensebaini
player48n <- "Bensebaini"
player48 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[3]){
    player48 <- c(player48, i)
  }
}
player48a <- probsNMwo3[player48,]
ccplayer48 <- sum(player48a[,10])
cicplayer48 <- sum(player48a[,11])
summplayer48 <- ccplayer48+cicplayer48
perccplayer48 <- ccplayer48/summplayer48
percicplayer48 <- cicplayer48/summplayer48
IDcount05omc[3]


## individuals #49 Finnbogason
player49n <- "Finnbogason"
player49 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[4]){
    player49 <- c(player49, i)
  }
}
player49a <- probsNMwo3[player49,]
ccplayer49 <- sum(player49a[,10])
cicplayer49 <- sum(player49a[,11])
summplayer49 <- ccplayer49+cicplayer49
perccplayer49 <- ccplayer49/summplayer49
percicplayer49 <- cicplayer49/summplayer49
IDcount05omc[4]

## individuals #50 Grifo
player50n <- "Grifo"
player50 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[5]){
    player50 <- c(player50, i)
  }
}
player50a <- probsNMwo3[player50,]
ccplayer50 <- sum(player50a[,10])
cicplayer50 <- sum(player50a[,11])
summplayer50 <- ccplayer50+cicplayer50
perccplayer50 <- ccplayer50/summplayer50
percicplayer50 <- cicplayer50/summplayer50
IDcount05omc[5]

## individuals #51 Gyan
player51n <- "Gyan"
player51 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[6]){
    player51 <- c(player51, i)
  }
}
player51a <- probsNMwo3[player51,]
ccplayer51 <- sum(player51a[,10])
cicplayer51 <- sum(player51a[,11])
summplayer51 <- ccplayer51+cicplayer51
perccplayer51 <- ccplayer51/summplayer51
percicplayer51 <- cicplayer51/summplayer51
IDcount05omc[6]

## individuals #52 Haller
player52n <- "Haller"
player52 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[7]){
    player52 <- c(player52, i)
  }
}
player52a <- probsNMwo3[player52,]
ccplayer52 <- sum(player52a[,10])
cicplayer52 <- sum(player52a[,11])
summplayer52 <- ccplayer52+cicplayer52
perccplayer52 <- ccplayer52/summplayer52
percicplayer52 <- cicplayer52/summplayer52
IDcount05omc[7]

## individuals #53 Halstenberg
player53n <- "Halstenberg"
player53 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[8]){
    player53 <- c(player53, i)
  }
}
player53a <- probsNMwo3[player53,]
ccplayer53 <- sum(player53a[,10])
cicplayer53 <- sum(player53a[,11])
summplayer53 <- ccplayer53+cicplayer53
perccplayer53 <- ccplayer53/summplayer53
percicplayer53 <- cicplayer53/summplayer53
IDcount05omc[8]

## individuals #54 Hennings
player54n <- "Hennings"
player54 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[9]){
    player54 <- c(player54, i)
  }
}
player54a <- probsNMwo3[player54,]
ccplayer54 <- sum(player54a[,10])
cicplayer54 <- sum(player54a[,11])
summplayer54 <- ccplayer54+cicplayer54
perccplayer54 <- ccplayer54/summplayer54
percicplayer54 <- cicplayer54/summplayer54
IDcount05omc[9]

## individuals #55 Hofmann
player55n <- "Hofmann"
player55 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[10]){
    player55 <- c(player55, i)
  }
}
player55a <- probsNMwo3[player55,]
ccplayer55 <- sum(player55a[,10])
cicplayer55 <- sum(player55a[,11])
summplayer55 <- ccplayer55+cicplayer55
perccplayer55 <- ccplayer55/summplayer55
percicplayer55 <- cicplayer55/summplayer55
IDcount05omc[10]

## individuals #56 Hummels
player56n <- "Hummels"
player56 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[11]){
    player56 <- c(player56, i)
  }
}
player56a <- probsNMwo3[player56,]
ccplayer56 <- sum(player56a[,10])
cicplayer56 <- sum(player56a[,11])
summplayer56 <- ccplayer56+cicplayer56
perccplayer56 <- ccplayer56/summplayer56
percicplayer56 <- cicplayer56/summplayer56
IDcount05omc[11]

## individuals #57 Klaasen
player57n <- "Klaasen"
player57 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[12]){
    player57 <- c(player57, i)
  }
}
player57a <- probsNMwo3[player57,]
ccplayer57 <- sum(player57a[,10])
cicplayer57 <- sum(player57a[,11])
summplayer57 <- ccplayer57+cicplayer57
perccplayer57 <- ccplayer57/summplayer57
percicplayer57 <- cicplayer57/summplayer57
IDcount05omc[12]

## individuals #58 Lukebakio
player58n <- "Lukebakio"
player58 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[13]){
    player58 <- c(player58, i)
  }
}
player58a <- probsNMwo3[player58,]
ccplayer58 <- sum(player58a[,10])
cicplayer58 <- sum(player58a[,11])
summplayer58 <- ccplayer58+cicplayer58
perccplayer58 <- ccplayer58/summplayer58
percicplayer58 <- cicplayer58/summplayer58
IDcount05omc[13]


## individuals #59 Petersen
player59n <- "Petersen"
player59 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[14]){
    player59 <- c(player59, i)
  }
}
player59a <- probsNMwo3[player59,]
ccplayer59 <- sum(player59a[,10])
cicplayer59 <- sum(player59a[,11])
summplayer59 <- ccplayer59+cicplayer59
perccplayer59 <- ccplayer59/summplayer59
percicplayer59 <- cicplayer59/summplayer59
IDcount05omc[14]

## individuals #60 Rashica
player60n <- "Rashica"
player60 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[15]){
    player60 <- c(player60, i)
  }
}
player60a <- probsNMwo3[player60,]
ccplayer60 <- sum(player60a[,10])
cicplayer60 <- sum(player60a[,11])
summplayer60 <- ccplayer60+cicplayer60
perccplayer60 <- ccplayer60/summplayer60
percicplayer60 <- cicplayer60/summplayer60
IDcount05omc[15]

## individuals #61 Reus
player61n <- "Reus"
player61 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[16]){
    player61 <- c(player61, i)
  }
}
player61a <- probsNMwo3[player61,]
ccplayer61 <- sum(player61a[,10])
cicplayer61 <- sum(player61a[,11])
summplayer61 <- ccplayer61+cicplayer61
perccplayer61 <- ccplayer61/summplayer61
percicplayer61 <- cicplayer61/summplayer61
IDcount05omc[16]

## individuals #62 Sabitzer
player62n <- "Sabitzer"
player62 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[17]){
    player62 <- c(player62, i)
  }
}
player62a <- probsNMwo3[player62,]
ccplayer62 <- sum(player62a[,10])
cicplayer62 <- sum(player62a[,11])
summplayer62 <- ccplayer62+cicplayer62
perccplayer62 <- ccplayer62/summplayer62
percicplayer62 <- cicplayer62/summplayer62
IDcount05omc[17]

## individuals #63 Saul Niguez
player63n <- "Saul Niguez"
player63 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[18]){
    player63 <- c(player63, i)
  }
}
player63a <- probsNMwo3[player63,]
ccplayer63 <- sum(player63a[,10])
cicplayer63 <- sum(player63a[,11])
summplayer63 <- ccplayer63+cicplayer63
perccplayer63 <- ccplayer63/summplayer63
percicplayer63 <- cicplayer63/summplayer63
IDcount05omc[18]

## individuals #64 T.Hazard
player64n <- "T.Hazard"
player64 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[19]){
    player64 <- c(player64, i)
  }
}
player64a <- probsNMwo3[player64,]
ccplayer64 <- sum(player64a[,10])
cicplayer64 <- sum(player64a[,11])
summplayer64 <- ccplayer64+cicplayer64
perccplayer64 <- ccplayer64/summplayer64
percicplayer64 <- cicplayer64/summplayer64
IDcount05omc[19]

## individuals #65 Waldschmidt
player65n <- "Waldschmidt"
player65 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[20]){
    player65 <- c(player65, i)
  }
}
player65a <- probsNMwo3[player65,]
ccplayer65 <- sum(player65a[,10])
cicplayer65 <- sum(player65a[,11])
summplayer65 <- ccplayer65+cicplayer65
perccplayer65 <- ccplayer65/summplayer65
percicplayer65 <- cicplayer65/summplayer65
IDcount05omc[20]

## individuals #66 Wendell
player66n <- "Wendell"
player66 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[21]){
    player66 <- c(player66, i)
  }
}
player66a <- probsNMwo3[player66,]
ccplayer66 <- sum(player66a[,10])
cicplayer66 <- sum(player66a[,11])
summplayer66 <- ccplayer66+cicplayer66
perccplayer66 <- ccplayer66/summplayer66
percicplayer66 <- cicplayer66/summplayer66
IDcount05omc[21]

## individuals #67 Werner
player67n <- "Werner"
player67 <- c()
for (i in 1:2358) {
  if (probsNMwo3[i,1] == IDcount05omc[22]){
    player67 <- c(player67, i)
  }
}
player67a <- probsNMwo3[player67,]
ccplayer67 <- sum(player67a[,10])
cicplayer67 <- sum(player67a[,11])
summplayer67 <- ccplayer67+cicplayer67
perccplayer67 <- ccplayer67/summplayer67
percicplayer67 <- cicplayer67/summplayer67
IDcount05omc[22]


## creating table for overview of all individuals
Playername <- c(player1n,player2n,player3n, player4n, player5n,player6n,player7n, player8n, player9n,player10n,player11n, player12n, player13n, player14n,player15n,player16n,player17n, player18n, player19n, player20n,player21n, player22n, player23n, player24n,player25n,player26n,player27n, player28n, player29n, player30n,player31n, player32n, player33n, player34n,player35n,player36n,player37n, player38n, player39n, player40n,player41n, player42n, player43n, player44n,player45n, player46n,player47n, player48n, player49n, player50n,player51n, player52n, player53n, player54n,player55n,player56n,player57n, player58n, player59n, player60n,player61n, player62n, player63n, player64n,player65n,player66n,player67n )
nocor <- c(ccplayer1,ccplayer2,ccplayer3, ccplayer4,ccplayer5,ccplayer6,ccplayer7, ccplayer8, ccplayer9,ccplayer10,ccplayer11, ccplayer12, ccplayer13, ccplayer14,ccplayer15,ccplayer16,ccplayer17, ccplayer18, ccplayer19, ccplayer20,ccplayer21, ccplayer22, ccplayer23, ccplayer24,ccplayer25,ccplayer26,ccplayer27, ccplayer28, ccplayer29, ccplayer30,ccplayer31, ccplayer32, ccplayer33, ccplayer34,ccplayer35,ccplayer36,ccplayer37, ccplayer38, ccplayer39, ccplayer40,ccplayer41, ccplayer42, ccplayer43, ccplayer44,ccplayer45, ccplayer46,ccplayer47, ccplayer48, ccplayer49, ccplayer50,ccplayer51, ccplayer52, ccplayer53, ccplayer54,ccplayer55,ccplayer56,ccplayer57, ccplayer58, ccplayer59, ccplayer60,ccplayer61, ccplayer62, ccplayer63, ccplayer64,ccplayer65,ccplayer66,ccplayer67)
noicor <- c(cicplayer1,cicplayer2,cicplayer3, cicplayer4,cicplayer5,cicplayer6,cicplayer7, cicplayer8, cicplayer9,cicplayer10,cicplayer11, cicplayer12, cicplayer13, cicplayer14,cicplayer15,cicplayer16,cicplayer17, cicplayer18, cicplayer19, cicplayer20,cicplayer21, cicplayer22, cicplayer23, cicplayer24,cicplayer25,cicplayer26,cicplayer27, cicplayer28, cicplayer29, cicplayer30,cicplayer31, cicplayer32, cicplayer33, cicplayer34,cicplayer35,cicplayer36,cicplayer37, cicplayer38, cicplayer39, cicplayer40,cicplayer41, cicplayer42, cicplayer43, cicplayer44,cicplayer45, cicplayer46,cicplayer47, cicplayer48, cicplayer49, cicplayer50,cicplayer51, cicplayer52, cicplayer53, cicplayer54,cicplayer55,cicplayer56,cicplayer57, cicplayer58, cicplayer59, cicplayer60,cicplayer61, cicplayer62, cicplayer63, cicplayer64,cicplayer65,cicplayer66,cicplayer67)
notot <- c(summplayer1,summplayer2,summplayer3, summplayer4,summplayer5,summplayer6,summplayer7, summplayer8, summplayer9,summplayer10,summplayer11, summplayer12, summplayer13, summplayer14,summplayer15,summplayer16,summplayer17, summplayer18, summplayer19, summplayer20,summplayer21, summplayer22, summplayer23, summplayer24,summplayer25,summplayer26,summplayer27, summplayer28, summplayer29, summplayer30,summplayer31, summplayer32, summplayer33, summplayer34,summplayer35,summplayer36,summplayer37, summplayer38, summplayer39, summplayer40,summplayer41, summplayer42, summplayer43, summplayer44,summplayer45, summplayer46,summplayer47, summplayer48, summplayer49, summplayer50,summplayer51, summplayer52, summplayer53, summplayer54,summplayer55,summplayer56,summplayer57, summplayer58, summplayer59, summplayer60,summplayer61, summplayer62, summplayer63, summplayer64,summplayer65,summplayer66,summplayer67)
percec <- c(perccplayer1,perccplayer2,perccplayer3, perccplayer4,perccplayer5,perccplayer6,perccplayer7, perccplayer8, perccplayer9,perccplayer10,perccplayer11, perccplayer12, perccplayer13, perccplayer14,perccplayer15,perccplayer16,perccplayer17, perccplayer18, perccplayer19, perccplayer20,perccplayer21, perccplayer22, perccplayer23, perccplayer24,perccplayer25,perccplayer26,perccplayer27, perccplayer28, perccplayer29, perccplayer30,perccplayer31, perccplayer32, perccplayer33, perccplayer34,perccplayer35,perccplayer36,perccplayer37, perccplayer38, perccplayer39, perccplayer40,perccplayer41, perccplayer42, perccplayer43, perccplayer44,perccplayer45, perccplayer46,perccplayer47, perccplayer48, perccplayer49, perccplayer50,perccplayer51, perccplayer52, perccplayer53, perccplayer54, perccplayer55,perccplayer56,perccplayer57, perccplayer58, perccplayer59, perccplayer60,perccplayer61, perccplayer62, perccplayer63, perccplayer64,perccplayer65,perccplayer66,perccplayer67)
perceic <- c(percicplayer1,percicplayer2,percicplayer3, percicplayer4,percicplayer5,percicplayer6,percicplayer7, percicplayer8, percicplayer9,percicplayer10,percicplayer11, percicplayer12, percicplayer13, percicplayer14,percicplayer15,percicplayer16,percicplayer17, percicplayer18, percicplayer19, percicplayer20,percicplayer21, percicplayer22, percicplayer23, percicplayer24,percicplayer25,percicplayer26,percicplayer27, percicplayer28, percicplayer29, percicplayer30,percicplayer31, percicplayer32, percicplayer33, percicplayer34,percicplayer35,percicplayer36,percicplayer37, percicplayer38, percicplayer39, percicplayer40,percicplayer41, percicplayer42, percicplayer43, percicplayer44,percicplayer45, percicplayer46,percicplayer47, percicplayer48, percicplayer49, percicplayer50,percicplayer51, percicplayer52, percicplayer53, percicplayer54,percicplayer55,percicplayer56,percicplayer57, percicplayer58, percicplayer59, percicplayer60,percicplayer61, percicplayer62, percicplayer63, percicplayer64,percicplayer65,percicplayer66,percicplayer67)
tableNMwo3 <- rbind(Playername,nocor,noicor,notot,percec,perceic)
tableNMwo3

### analyze average probabilities of single player
JorginhoTL <- mean(player17a[,3])
JorginhoTC <- mean(player17a[,4])
JorginhoTR <- mean(player17a[,5])
JorginhoDL <- mean(player17a[,6])
JorginhoDC <- mean(player17a[,7])
JorginhoDR <- mean(player17a[,8])

SalahTL <- mean(player37a[,3])
SalahTC <- mean(player37a[,4])
SalahTR <- mean(player37a[,5])
SalahDL <- mean(player37a[,6])
SalahDC <- mean(player37a[,7])
SalahDR <- mean(player37a[,8])

TellesTL <- mean(player41a[,3])
TellesTC <- mean(player41a[,4])
TellesTR <- mean(player41a[,5])
TellesDL <- mean(player41a[,6])
TellesDC <- mean(player41a[,7])
TellesDR <- mean(player41a[,8])

TellesTL <- mean(player6a[,3])
TellesTC <- mean(player6a[,4])
TellesTR <- mean(player6a[,5])
TellesDL <- mean(player6a[,6])
TellesDC <- mean(player6a[,7])
TellesDR <- mean(player6a[,8])

SilvaTL <- mean(player38a[,3])
SilvaTC <- mean(player38a[,4])
SilvaTR <- mean(player38a[,5])
SilvaDL <- mean(player38a[,6])
SilvaDC <- mean(player38a[,7])
SilvaDR <- mean(player38a[,8])

SuarezTL <- mean(player40a[,3])
SuarezTC <- mean(player40a[,4])
SuarezTR <- mean(player40a[,5])
SuarezDL <- mean(player40a[,6])
SuarezDC <- mean(player40a[,7])
SuarezDR <- mean(player40a[,8])

CavaniTL <- mean(player5a[,3])
CavaniTC <- mean(player5a[,4])
CavaniTR <- mean(player5a[,5])
CavaniDL <- mean(player5a[,6])
CavaniDC <- mean(player5a[,7])
CavaniDR <- mean(player5a[,8])

ImmobileTL <- mean(player14a[,3])
ImmobileTC <- mean(player14a[,4])
ImmobileTR <- mean(player14a[,5])
ImmobileDL <- mean(player14a[,6])
ImmobileDC <- mean(player14a[,7])
ImmobileDR <- mean(player14a[,8])

IbraTL <- mean(player15a[,3])
IbraTC <- mean(player15a[,4])
IbraTR <- mean(player15a[,5])
IbraDL <- mean(player15a[,6])
IbraDC <- mean(player15a[,7])
IbraDR <- mean(player15a[,8])

RamosTL <- mean(player34a[,3])
RamosTC <- mean(player34a[,4])
RamosTR <- mean(player34a[,5])
RamosDL <- mean(player34a[,6])
RamosDC <- mean(player34a[,7])
RamosDR <- mean(player34a[,8])

HummelsTL <- mean(player56a[,3])
HummelsTC <- mean(player56a[,4])
HummelsTR <- mean(player56a[,5])
HummelsDL <- mean(player56a[,6])
HummelsDC <- mean(player56a[,7])
HummelsDR <- mean(player56a[,8])

WendellTL <- mean(player66a[,3])
WendellTC <- mean(player66a[,4])
WendellTR <- mean(player66a[,5])
WendellDL <- mean(player66a[,6])
WendellDC <- mean(player66a[,7])
WendellDR <- mean(player66a[,8])

WWarisTL <- mean(player44a[,3])
WWarisTC <- mean(player44a[,4])
WWarisTR <- mean(player44a[,5])
WWarisDL <- mean(player44a[,6])
WWarisDC <- mean(player44a[,7])
WWarisDR <- mean(player44a[,8])

WWeghorstTL <- mean(player45a[,3])
WWeghorstTC <- mean(player45a[,4])
WWeghorstTR <- mean(player45a[,5])
WWeghorstDL <- mean(player45a[,6])
WWeghorstDC <- mean(player45a[,7])
WWeghorstDR <- mean(player45a[,8])

WWAlarioTL <- mean(player46a[,3])
WWAlarioTC <- mean(player46a[,4])
WWAlarioTR <- mean(player46a[,5])
WWAlarioDL <- mean(player46a[,6])
WWAlarioDC <- mean(player46a[,7])
WWAlarioDR <- mean(player46a[,8])

AGrieziTL <- mean(player10a[,3])
AGrieziTC <- mean(player10a[,4])
AGrieziTR <- mean(player10a[,5])
AGrieziDL <- mean(player10a[,6])
AGrieziDC <- mean(player10a[,7])
AGrieziDR <- mean(player10a[,8])

# count number how often alternatives are predicted
nNM
countDRc<-0
for (i in 1:2358) {
  if (nNM[i,6] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    if(nNM[i,6] == nNM[i,9]){
    countDRc <- countDRc+1
    }
  }
}
countDRc

countTL<-0
for (i in 1:2358) {
  if (nNM[i,3] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countTL <- countTL+1
  }
}
countTL

countTC<-0
for (i in 1:2358) {
  if (nNM[i,4] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countTC <- countTC+1
  }
}
countTC

countTR<-0
for (i in 1:2358) {
  if (nNM[i,5] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countTR <- countTR+1
  }
}
countTR

countDL<-0
for (i in 1:2358) {
  if (nNM[i,6] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countDL <- countDL+1
  }
}
countDL

countDC<-0
for (i in 1:2358) {
  if (nNM[i,7] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countDC <- countDC+1
  }
}
countDC

countDR<-0
for (i in 1:2358) {
  if (nNM[i,8] == max(nNM[i,3],nNM[i,4],nNM[i,5],nNM[i,6],nNM[i,7],nNM[i,8])){
    countDR <- countDR+1
  }
}
countDR

# calculate probability when alterntive is predicted by model

probsDR<-c()
for (i in 1:2358) {
    if(nNM[i,8] == nNM[i,9]){
      probsDR <- c(probsDR, nNM[i,9])
  }
}
probsDR
max(probsDR)
min(probsDR)
mean(probsDR)

probsDC<-c()
for (i in 1:2358) {
  if(nNM[i,7] == nNM[i,9]){
    probsDC <- c(probsDC, nNM[i,9])
  }
}
probsDC
max(probsDC)
min(probsDC)
mean(probsDC)

probsDL<-c()
for (i in 1:2358) {
  if(nNM[i,6] == nNM[i,9]){
    probsDL <- c(probsDL, nNM[i,9])
  }
}
probsDL
max(probsDL)
min(probsDL)
mean(probsDL)

probsTR<-c()
for (i in 1:2358) {
  if(nNM[i,5] == nNM[i,9]){
    probsTR <- c(probsTR, nNM[i,9])
  }
}
probsTR
max(probsTR)
min(probsTR)
mean(probsTR)

probsTC<-c()
for (i in 1:2358) {
  if(nNM[i,4] == nNM[i,9]){
    probsTC <- c(probsTC, nNM[i,9])
  }
}
probsTC
max(probsTC)
min(probsTC)
mean(probsTC)

probsTL<-c()
for (i in 1:2358) {
  if(nNM[i,3] == nNM[i,9]){
    probsTL <- c(probsTL, nNM[i,9])
  }
}
probsTL
max(probsTL)
min(probsTL)
mean(probsTL)

probsmax <- c(max(nNM[,3]),max(nNM[,4]),max(nNM[,5]),max(nNM[,6]),max(nNM[,7]),max(nNM[,8]))
probsmax

# count for Griezmann and Ramos how often they choose a specific alternative

countGriez<-0
for (i in 1:32) {
  if(player10a[i,5] == player10a[i,9]){
    countGriez <- countGriez+1
  }
}
countGriez

countRamos<-0
for (i in 1:40) {
  if(player34a[i,4] == player34a[i,9]){
    countRamos <- countRamos+1
  }
}
countRamos

# count how often alternative is chosen in database

chosenAlt <- matrix(0,nrow=2358)
for (i in 1:2358) {
  if(nNM[i,3] == nNM[i,9]){
    chosenAlt[i,1]<-1
  }
  if(nNM[i,4] == nNM[i,9]){
    chosenAlt[i,1]<-2
  }
  if(nNM[i,5] == nNM[i,9]){
    chosenAlt[i,1]<-3
  }
  if(nNM[i,6] == nNM[i,9]){
    chosenAlt[i,1]<-4
  }
  if(nNM[i,7] == nNM[i,9]){
    chosenAlt[i,1]<-5
  }
  if(nNM[i,8] == nNM[i,9]){
    chosenAlt[i,1]<-6
  }
}

# maximum and average probability of single alternatives, needed for the boundary of combined method

avgprobTL <- mean(nNM[,3])
avgprobTC <- mean(nNM[,4])
avgprobTR <- mean(nNM[,5])
avgprobDL <- mean(nNM[,6])
avgprobDC <- mean(nNM[,7])
avgprobDR <- mean(nNM[,8])

maxprobTL <- max(nNM[,3])
maxprobTC <- max(nNM[,4])
maxprobTR <- max(nNM[,5])
maxprobDL <- max(nNM[,6])
maxprobDC <- max(nNM[,7])
maxprobDR <- max(nNM[,8])

# test

nNM2 <- matrix(0,nrow=2358,ncol=6)
for (i in 1:2358) {
  nNM2[i,1]<- nNM[i,3]-avgprobTL
  nNM2[i,2]<- nNM[i,4]-avgprobTC
  nNM2[i,3]<- nNM[i,5]-avgprobTR
  nNM2[i,4]<- nNM[i,6]-avgprobDL
  nNM2[i,5]<- nNM[i,7]-avgprobDC
  nNM2[i,6]<- nNM[i,8]-avgprobDR
}
nNM2

# calculate highest increase for second prediction method 
highestIn <- matrix(0,nrow=2358)
for (i in 1:2358) {
  if (nNM2[i,1]==max(nNM2[i,])){
    highestIn[i,1]=1
  }
  if (nNM2[i,2]==max(nNM2[i,])){
    highestIn[i,1]=2
  }
  if (nNM2[i,3]==max(nNM2[i,])){
    highestIn[i,1]=3
  }
  if (nNM2[i,4]==max(nNM2[i,])){
    highestIn[i,1]=4
  }
  if (nNM2[i,5]==max(nNM2[i,])){
    highestIn[i,1]=5
  }
  if (nNM2[i,6]==max(nNM2[i,])){
    highestIn[i,1]=6
  }
}
highestIn
# count correct predictions second method

predict <- cbind(highestIn,chosenAlt)
numcor <- 0
numincor <- 0 
for (i in 1:2358){
  if(predict[i,1]==predict[i,2]){
    numcor = numcor+1
  }
  if(predict[i,1]!=predict[i,2]){
    numincor = numincor+1
  }
}
# calculate correct prediction rate of single alternatives
numTL<-0
numTC<-0
numTR<-0
numDL<-0
numDC<-0
numDR<-0
prozc= numcor/(numcor+numincor)
for (i in 1:2358){
  if(predict[i,1]==1){
    numTL = numTL+1
  }
  if(predict[i,1]==2){
    numTC = numTC+1
  }
  if(predict[i,1]==3){
    numTR = numTR+1
  }
  if(predict[i,1]==4){
    numDL = numDL+1
  }
  if(predict[i,1]==5){
    numDC = numDC+1
  }
  if(predict[i,1]==6){
    numDR = numDR+1
  }
}

numTLc<-0
numTCc<-0
numTRc<-0
numDLc<-0
numDCc<-0
numDRc<-0
for (i in 1:2358){
  if(predict[i,1]==1 & predict[i,2]==1){
    numTLc = numTLc+1
  }
  if(predict[i,1]==2 & predict[i,2]==2){
    numTCc = numTRc+1
  }
  if(predict[i,1]==3 & predict[i,2]==3){
    numTRc = numTCc+1
  }
  if(predict[i,1]==4 & predict[i,2]==4){
    numDLc = numDLc+1
  }
  if(predict[i,1]==5 & predict[i,2]==5){
    numDCc = numDCc+1
  }
  if(predict[i,1]==6 & predict[i,2]==6){
    numDRc = numDRc+1
  }
}
pcTL<- numTLc/numTL
pcTC<- numTCc/numTC
pcTr<- numTRc/numTR
pcDL<- numDLc/numDL
pcDC<- numDCc/numDC
pcDR<- numDRc/numDR

# get alternative with highest probability
highestprob <- matrix(0,nrow=2358)
for (i in 1:2358){
  if (reddataNMwo3[i,1]==max(reddataNMwo3[i,])){
    highestprob[i,1]=1
  }
  if (reddataNMwo3[i,2]==max(reddataNMwo3[i,])){
    highestprob[i,1]=2
  }
  if (reddataNMwo3[i,3]==max(reddataNMwo3[i,])){
    highestprob[i,1]=3
  }
  if (reddataNMwo3[i,4]==max(reddataNMwo3[i,])){
    highestprob[i,1]=4
  }
  if (reddataNMwo3[i,5]==max(reddataNMwo3[i,])){
    highestprob[i,1]=5
  }
  if (reddataNMwo3[i,6]==max(reddataNMwo3[i,])){
    highestprob[i,1]=6
  }
}

# calculate boundary for combined method
fpavgprobTL <- (maxprobTL-avgprobTL)/2.5
fpavgprobTC <- (maxprobTC-avgprobTC)/2.5
fpavgprobTR <- (maxprobTR-avgprobTR)/2.5
fpavgprobDL <- (maxprobDL-avgprobDL)/2.5
fpavgprobDC <- (maxprobDC-avgprobDC)/2.5
fpavgprobDR <- (maxprobDR-avgprobDR)/2.5

# test combined method and calcalute prediction rate of this method
highestInfp <- matrix(0,nrow=2358)
for (i in 1:2358) {
  if (nNM2[i,1]==max(nNM2[i,]) & nNM2[i,1] > fpavgprobTL){
    highestInfp[i,1]=1
  }
  if (nNM2[i,2]==max(nNM2[i,]) & nNM2[i,2] > fpavgprobTC){
    highestInfp[i,1]=2
  }
  if (nNM2[i,3]==max(nNM2[i,]) & nNM2[i,3] > fpavgprobTR){
    highestInfp[i,1]=3
  }
  if (nNM2[i,4]==max(nNM2[i,]) & nNM2[i,4] > fpavgprobDL){
    highestInfp[i,1]=4
  }
  if (nNM2[i,5]==max(nNM2[i,]) & nNM2[i,5] > fpavgprobDC){
    highestInfp[i,1]=5
  }
  if (nNM2[i,6]==max(nNM2[i,]) & nNM2[i,6] > fpavgprobDR){
    highestInfp[i,1]=6
  }
}

for (i in 1:2358){
  if (highestInfp[i,1]==0){
    highestInfp[i,1] <- highestprob[i,1]
  }
}

highestInfp
predictfp <- cbind(highestInfp,chosenAlt)
predictfp
numcorfp <- 0
numincorfp <- 0 
for (i in 1:2358){
  if(predictfp[i,1]==predictfp[i,2]){
    numcorfp = numcorfp+1
  }
  if(predictfp[i,1]!=predictfp[i,2]){
    numincorfp = numincorfp+1
  }
}

prozcfp= numcorfp/(numcorfp+numincorfp)

numTL15 <- 0
numTC15 <- 0
numTR15 <- 0
numDL15 <- 0
numDC15 <- 0
numDR15 <- 0

for (i in 1:2358){
  if(predictfp[i,1]==1){
    numTL15 = numTL15+1
  }
  if(predictfp[i,1]==2){
    numTC15 = numTC15+1
  }
  if(predictfp[i,1]==3){
    numTR15 = numTR15+1
  }
  if(predictfp[i,1]==4){
    numDL15 = numDL15+1
  }
  if(predictfp[i,1]==5){
    numDC15 = numDC15+1
  }
  if(predictfp[i,1]==6){
    numDR15 = numDR15+1
  }
}
nNM2n <- cbind(nNM2,chosenAlt)

