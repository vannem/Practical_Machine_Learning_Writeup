## Loading the data and basic setting
setwd("D:/YI_20150421/2_Coursera/Prac_ML/Projects/1")
TrainingData <- read.csv(file="pml-training.csv", head=TRUE, sep=",")
TestingData <- read.csv(file="pml-testing.csv", head=TRUE, sep=",")

library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
set.seed(777)

## Removing those cols which contain NAs and "", and choosing some meaningful parameters as predictors
TrD <- TrainingData[,colSums(is.na(TrainingData))==0] #remove cols with NAs
TeD <- TestingData[,colSums(is.na(TrainingData))==0] #remove cols with NAs

TrainingData <- TrD[,colSums(TrD=="")==0] #remove cols with ""
TestingData <- TeD[,colSums(TrD=="")==0] #remove cols with ""

rm(TrD)
rm(TeD)

names(TrainingData)
names(TestingData)

TrainingData <- TrainingData[,7:60]
TestingData <- TestingData[,7:60]
# TrainingData$classe <- as.factor(TrainingData$classe) if they are not factor!!!

## Applying 5-folds cross validation in 5 repetitions and training the data using random forest method, and checking how well this modelFit is.
ctrl <- trainControl(method="repeatedcv", number=5, repeats=5) 
# number: Either the number of folds or number of resampling iterations
modelFit <- train(classe ~ ., data=TrainingData, method="rf", trControl=ctrl)
modelFit
summary(modelFit)
plot(varImp(modelFit))

## Calculating the errors using the TrainingData Set, generating data for the prediction vector for the Assigment Submission
predictionsTr <- predict(modelFit, newdata=TrainingData)
confusionMatrix(predictionsTr,TrainingData$classe)

## Predicting the rsults for the testing data
predict(modelFit, newdata=TestingData)
