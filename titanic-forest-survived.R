rm(list=ls(all=TRUE))
library(randomForest)
library(RCurl)
setwd('/Users/zurich/Google Drive/FactMachine-SITE/FM-Site-STRUCTURE/11-RandomForest/code/random-forest')
#load('mTitanicAge.RData')

x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
mTitanicAll <- read.csv(text = x)

#mTitanicAll <- read.csv('mTitanicAge.csv')
mTitanicAll$survived <- as.factor(mTitanicAll$survived)

set.seed(13)
trainSet <- sample(1:nrow(mTitanicAll), 1100)
#Following evaluates to 33.02. Checking training set includes same rows. 
mean(mTitanicAll[trainSet, "fare"])

set.seed(23)
forest.surive <- randomForest(survived~. , data = mTitanicAll, mtry = 3, 
                           importance = TRUE, subset = trainSet, ntree = 2000)

survive.Predict <- predict(forest.surive, type = "class",
                           newdata = mTitanicAll[-trainSet,])
tab <- table(survive.Predict, mTitanicAll[-trainSet, "survived"])
tab
#83.25 is the score to beat

importance(forest.surive, type=1)
(tab[1] + tab[4]) / sum(tab)
