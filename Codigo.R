##Loading the necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(corrplot)

##loading the files from my computer
test <- read.csv("C:/Users/Daniel Lopes/Documents/R/Machine Learning Project/pml-testing.csv",na.strings = c("NA", "#DIV/0!", ""))
train <- read.csv("C:/Users/Daniel Lopes/Documents/R/Machine Learning Project/pml-training.csv",na.strings = c("NA", "#DIV/0!", ""))

##removing N.A values 
train <- train[, colSums(is.na(train)) == 0]
test <- test[, colSums(is.na(test)) == 0] 

##Evaluating the training set dimension
dim(train) ## [1] 19622    54


##Transforming the Date and Time Stamp
train$cvtd_timestamp<- as.Date(train$cvtd_timestamp, format = "%m/%d/%Y %H:%M")

##Removing 
trainRemove<- grepl("^X|timestamp|window", names(train))
train<- train[, !trainRemove]
testRemove<- grepl("^X|timestamp|window", names(test))
test<- test[, !testRemove]

##Evaluating the distribution of classes
table(train$classe) 

##Evaluating the proportion of classes
prop.table(table(train$classe)) 

##Evaluating the users
prop.table(table(train$user_name)) 

##Evaluating the users 
prop.table(table(train$user_name,train$classe),1) 
prop.table(table(train$user_name,train$classe),2) 

##Transforming into numeric
trainCleaned<- train[, sapply(train, is.numeric)]
testCleaned<- test[, sapply(test, is.numeric)]

##Assingn factors
classe<- train$classe
trainCleaned$classe<- classe


##Breaking the train dataset into two new sets, with the objective of prevent overfitng and better understending of the noise.
set.seed(882231)
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]

##seting control parametersand cross validation
controlRf <- trainControl(method="cv", 5)

##using randomForest method to fit the model is due to its abillity to exclude outliers and weight the variables so that latter we can pick the best model.
##The importance of weighting the variables is shown when we train the algorthim. Beacause as the new signals and noise arrives, the weights change  
rfmod<- train(classe ~., data=trainData, method="rf", trControl=controlRf, importance=TRUE, ntree=100)
rfmod

##predcting the outcome with the randomForest model
predictRfmod<- predict(rfmod, testData)
##Notice that the "testData" is being predict uppon, and this particullary data set was created from the original train set

##Ploting the confusion Matrix has the point of explicity showing the behavior of the randomForest model
confusionMatrix(testData$classe, predictRfmod)
##Notice that now we can determine the Sensitivity and Specificity of the model and evaluate the erros 

##Evaluating the acuaracy of the model
accuracy <- postResample(predictRfmod, testData$classe)
accuracy

##Evaluating the overall error of the model
Error <- 1 - as.numeric(confusionMatrix(testData$classe, predictRfmod)$overall[1])
Error

##Predicting the results in the test set
result <- predict(rfmod, testCleaned[, -length(names(testCleaned))])
result
##Notice that this is the original test set, the one extracted from the website

##Better understanding of the outcome
rtree<- rpart(classe ~ ., data=trainData, method="class")
prp(rtree)
##Notice that this decision tree shows all the paths that the algorithm used to predict the outcomes


