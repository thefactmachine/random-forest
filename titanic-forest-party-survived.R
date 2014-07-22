rm(list=ls(all=TRUE))

#load('mTitanicAge.RData')

x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
mTitanicAll <- read.csv(text = x)

library(randomForest)

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




#===============TRY Party Package======================
library(party)
print(Sys.time())
data.controls <- cforest_unbiased(ntree=5000, mtry=4) 
set.seed(23) 
data.cforest <- cforest(survived~. , data = mTitanicAll, 
                        controls=data.controls)

data.trp <- Predict(data.cforest, newdata = mTitanicAll[-trainSet,])
tab <- table(data.trp, mTitanicAll[-trainSet, names(mTitanicAll) == "survived"])
(tab[1] + tab[4]) / sum(tab)
print(Sys.time())
varimp(data.cforest)
#===============

