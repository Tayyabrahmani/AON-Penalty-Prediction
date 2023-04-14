#forecast = apollo_prediction(model,
#                              apollo_probabilities,
#                              apollo_inputs)

N3wo3var = forecast3N


### investigating the probabilities forecasted by the model
probNM3n <- N3wo3var
red <- c(1,2,9)
reddataNM3n <- probNM3n[-red]
counterTrue = 0
counterFalse = 0

library(dplyr)
probsNM3n <- mutate(probNM3n, correct = 4, incorrect = 5 )
for (i in 1:2358) {
  if (max(reddataNM3n[i,]) == probNM3n[i,9])
  {probsNM3n[i,10] <- 1 ; probsNM3n[i,11] <- 0}
  else {probsNM3n[i,10] <- 0 ; probsNM3n[i,11] <- 1}
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

#count number correct/incorrect
countCorNM3n <- sum(probsNM3n[,10])
countIncorNM3n <- sum(probsNM3n[,11])

## calculate percentage of True/False
percTrueNM3n = countCorNM3n/(countCorNM3n+countIncorNM3n)
percFalseNM3n = countIncorNM3n/(countCorNM3n+countIncorNM3n)

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
    if (probNM3n[i,1] == IDcount[j]){
      redrow = c(redrow, i)
    }
  }
}
redrowdata <- probsNM3n[-redrow,]
#redrowdata2 <- redrowdata[-red]
#for (i in 1:324) {
#  if (max(redrowdata2[i,]) == redrowdata[i,12])
#  {counterTrue = counterTrue + 1}
#  else {counterFalse = counterFalse +1}  
#}
redrowdata
coucor1p3n <- sum(redrowdata[,10])
couincor1p3n <- sum(redrowdata[,11])
percTrue1p3n = coucor1p3n/(coucor1p3n+couincor1p3n)
percFalse1p3n = couincor1p3n/(coucor1p3n+couincor1p3n)

## Analyze penalty takers with only 50 or more shots in dataset
rowcount <- c()
for (j in 1:9) {
  for (i in 1:2358) {
    if (probsNM3n[i,1] == IDcount50om[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data50om3n <- probsNM3n[rowcount,]

coucor50om3n <- sum(data50om3n[,10])
couincor50om3n <- sum(data50om3n[,11]) 
percTrue50om3n = coucor50om3n/(coucor50om3n+couincor50om3n)
percFalse50om3n = couincor50om3n/(coucor50om3n+couincor50om3n)

## Analyze penalty takers with  30-49 shots in dataset
rowcount <- c()
for (j in 1:10) {
  for (i in 1:2358) {
    if (probsNM3n[i,1] == IDcount30omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data30om3n <- probsNM3n[rowcount,]

coucor30om3n <- sum(data30om3n[,10])
couincor30om3n <- sum(data30om3n[,11]) 
percTrue30om3n = coucor30om3n/(coucor30om3n+couincor30om3n)
percFalse30om3n = couincor30om3n/(coucor30om3n+couincor30om3n)

## Analyze penalty takers with  20-29 shots in dataset
rowcount <- c()
for (j in 1:7) {
  for (i in 1:2358) {
    if (probsNM3n[i,1] == IDcount20omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data20om3n <- probsNM3n[rowcount,]

coucor20om3n <- sum(data20om3n[,10])
couincor20om3n <- sum(data20om3n[,11]) 
percTrue20om3n = coucor20om3n/(coucor20om3n+couincor20om3n)
percFalse20om3n = couincor20om3n/(coucor20om3n+couincor20om3n)

## Analyze penalty takers with  10-19 shots in dataset
rowcount <- c()
for (j in 1:19) {
  for (i in 1:2358) {
    if (probsNM3n[i,1] == IDcount10omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data10om3n <- probsNM3n[rowcount,]

coucor10om3n <- sum(data10om3n[,10])
couincor10om3n <- sum(data10om3n[,11]) 
percTrue10om3n = coucor10om3n/(coucor10om3n+couincor10om3n)
percFalse10om3n = couincor10om3n/(coucor10om3n+couincor10om3n)

## Analyze penalty takers with  5-9 shots in dataset
rowcount <- c()
for (j in 1:22) {
  for (i in 1:2358) {
    if (probsNM3n[i,1] == IDcount05omc[j]){
      rowcount <- c(rowcount, i)
    }
  }
}
data05om3n <- probsNM3n[rowcount,]

coucor05om3n <- sum(data05om3n[,10])
couincor05om3n <- sum(data05om3n[,11]) 
percTrue05om3n = coucor05om3n/(coucor05om3n+couincor05om3n)
percFalse05om3n = couincor05om3n/(coucor05om3n+couincor05om3n)

### average probability assignment
sumvec3n <- c()
for (j in 1:6){
  sumvec3n[j] <- sum(reddataNM3n[,j])
}
sumvecperc3n = sumvec3n/2358
sumvecperc3n
ts3n <- sum(sumvecperc3n)

#################################

# count how often single alternatives are chosen

count3TL<-0
for (i in 1:2358) {
  if (N3wo3var[i,3] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3TL <- count3TL+1
  }
}
count3TL

count3TC<-0
for (i in 1:2358) {
  if (N3wo3var[i,4] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3TC <- count3TC+1
  }
}
count3TC

count3TR<-0
for (i in 1:2358) {
  if (N3wo3var[i,5] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3TR <- count3TR+1
  }
}
count3TR

count3DL<-0
for (i in 1:2358) {
  if (N3wo3var[i,6] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3DL <- count3DL+1
  }
}
count3DL

count3DC<-0
for (i in 1:2358) {
  if (N3wo3var[i,7] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3DC <- count3DC+1
  }
}
count3DC

count3DR<-0
for (i in 1:2358) {
  if (N3wo3var[i,8] == max(N3wo3var[i,3],N3wo3var[i,4],N3wo3var[i,5],N3wo3var[i,6],N3wo3var[i,7],N3wo3var[i,8])){
    count3DR <- count3DR+1
  }
}
count3DR
