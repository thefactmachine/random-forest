rm(list=ls(all=TRUE))
x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
mTitanicAll <- read.csv(text = x)

library(RCurl)
library(caret)
library(pROC)
library(randomForest)

mTitanicAll$survived <- factor(mTitanicAll$survived, levels = c(0,1), 
                               labels = c('perished', 'survived'))
set.seed(13)
trainSet <- sample(1:nrow(mTitanicAll), 1100)
ord <- c(2,1,3,4,5,6,7,8,9,10)
trainData <- mTitanicAll[trainSet, ord]
testData <- mTitanicAll[-trainSet, ord]

print(Sys.time())
set.seed(123)
rf.grid <- expand.grid(mtry = 1:9)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 2, 
                        summaryFunction = twoClassSummary, classProbs = TRUE)

rf.tune <- train(survived~ . , data = trainData, 
                  method = "rf", metric = "ROC",
                  tuneGrid = rf.grid, trControl = cv.ctrl)
print(Sys.time())
#takes about 2minutes 17 seconds. 
rf.tune
plot(rf.tune)
predClass <- predict(rf.tune, newdata = testData)
confusionMatrix(data = predClass, testData$survived)
#81.34%



