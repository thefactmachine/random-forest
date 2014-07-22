rm(list=ls(all=TRUE))
library(gbm)


x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
mTitanicAll <- read.csv(text = x)

set.seed(13)
trainSet <- sample(1:nrow(mTitanicAll), 1100)
#Following evaluates to 33.02. Checking training set includes same rows. 
mean(mTitanicAll[trainSet, "fare"])

#interaction.depth limits the size of each tree.
ord <- c(2,1,3,4,5,6,7,8,9,10)
trainData <- mTitanicAll[trainSet, ord]
testData <- mTitanicAll[-trainSet, ord]
#drop survived column on test data

bm <- gbm(survived~. , data=trainData, distribution = "multinomial",
        n.trees=5000, interaction.depth=4, shrinkage = 0.001,
        n.cores =4)

yb <- predict(bm, newdata = testData[,2:10], n.trees = 5000,  type="response")
res <- ifelse(yb[,1,1] < 0.5, 1, 0)

testSurv<- testData$survived
tab <- table(testSurv,res)

(tab[1] + tab[4]) / sum(tab)
tab



