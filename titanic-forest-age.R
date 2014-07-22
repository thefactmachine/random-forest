rm(list=ls(all=TRUE))
library(RCurl)
library(randomForest)

x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanic.csv")
mTitanic <- read.csv(text = x)

#mTitanic <- read.csv("mTitanic.csv")

mTitanic$pclass <- 
    factor(mTitanic$pclass, levels = c("1st", "2nd", "3rd"), ordered = FALSE)
#Partition into records with Age and Records without Age
mTitanicNoAge <- mTitanic[is.na(mTitanic$age),]
mTitanicAge <- mTitanic[!is.na(mTitanic$age),]
set.seed(13)
trainSet <- sample(1:nrow(mTitanicAge), 900)
#CHECK following should be 26909.08
#sum(mTitanicAge[trainSet, "age"])
set.seed(4)
forest.age <- randomForest(age~. -survived, data = mTitanicAge, mtry = 2, 
                           importance = TRUE, subset = trainSet, ntree = 1000)
agePredictTest <- predict(forest.age, newdata = mTitanicAge[-trainSet,])
ageTest <- mTitanicAge[-trainSet, "age"]
mean((ageTest - agePredictTest)^2)
#(seed = 4) 1 = 110.65; 2 = 91.72; 3 = 95.12; 4 = 98.99
#(seed = 45) 1= 109.97 2 = 92.66; 3 = 95.12; 4 = 99.37
#Choose 2. MSE = 91.72. Sqrt = 9.5

#we have got an estimate of the error. Use the entire data set.
forest.age.trainAll <- randomForest(age~. -survived, data = mTitanicAge, 
                                    mtry = 2, importance = TRUE,  ntree = 1000)

agePredictAll <- predict(forest.age.trainAll, newdata = mTitanicNoAge)
mTitanicNoAge$age <- agePredictAll
mTitanicAll <- rbind(mTitanicNoAge, mTitanicAge)

#save(mTitanicAll, file = "mTitanicAge.RData")
#write.csv(mTitanic, file = "mTitanicAge.csv", row.names = FALSE)





