rm(list=ls(all=TRUE))

library(caret)
library(pROC)
library(ada)

x <- getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
mTitanicAll <- read.csv(text = x)


mTitanicAll$survived <- factor(mTitanicAll$survived, levels = c(0,1), 
                               labels = c('perished', 'survived'))
set.seed(13)
trainSet <- sample(1:nrow(mTitanicAll), 1100)
ord <- c(2,1,3,4,5,6,7,8,9,10)
trainData <- mTitanicAll[trainSet, ord]
testData <- mTitanicAll[-trainSet, ord]

#took 17 minutes to run
print(Sys.time())
# 3 * 3 * 3 = 27 combos.
#10 fold validation ==> (10-1) * 2 repeats = 18 cvs. 486 models
ada.simple <- expand.grid(maxdepth = c(2,4,6), nu = c(0.01, 0.1, 1), 
                          iter = c(20, 50, 80))
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 2, 
                        summaryFunction = twoClassSummary, classProbs = TRUE)
ada.tune <- train(survived~ . , data = trainData, 
                  method = "ada", metric = "ROC",
                  tuneGrid = ada.simple, trControl = cv.ctrl)
print(Sys.time())
plot(ada.tune)
predClass <- predict(ada.tune, newdata = testData)
confusionMatrix(data = predClass, testData$survived)
#predict 81.82%




