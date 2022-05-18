#Shreyas Ravi
#DSC 424
#Milestone 3
library(MASS)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(vcd)
library(ca)
library(janitor)
library(car)
library(dplyr)
library(ggplot2)
library(caret)

#opening cleaned version of data 
votes <- read.csv("Final_Votes1.csv")
head(votes)
#adding totals of gop, dem and total votes to the df (from 2012 and 2016)
votes$total_all <- votes$X2016.Total+votes$X2012.Total
votes$total_dem <- votes$X2016.Dem.Votes + votes$X2012.Dem.Votes
votes$total_gop <- votes$X2016.GOP.Votes+votes$X2012.GOP.Votes
head(votes)
votes1 <- subset(votes, select = -c(1, 2, 3, 7, 8, 9))
head(votes1)
votes2 <- subset(votes1, select = -c(1, 4))
head(votes2)

votes_vector <- factor(c(votes1$State))
View(votes_vector)
votes_vector_new <- unclass(votes_vector)
votes_vector_new
votes1$states_dummies <- votes_vector
View(votes1)
#splitting into train and test
train <- createDataPartition(y=votes1$total_all, p=0.7, list = FALSE)
training <- votes1[train,]
testing <- votes1[-train,]
library(glmnet)

#converting to matrices 
xTrain <- as.matrix(training[, c(1, 4, 6, 7)])
yTrain <- as.matrix(training[, c(5)])
xTest <- as.matrix(testing[, c(1, 4, 6, 7)])
yTest <- as.matrix(testing[, c(5)])
elasticFit <- cv.glmnet(xTrain, yTrain, alpha=0.5, nfolds=5) #does not work due to dummy column
#removing the state dummies column from xTrain
xTrain <- as.matrix(training[, c(4, 5)])
xTest <- as.matrix(testing[, c(4, 5)])
#running elastic net reg again
elasticFit <- cv.glmnet(xTrain, yTrain, alpha=0.5, nfolds=7)
elasticFit1 <- cv.glmnet(xTrain, yTrain, alpha=0.75, nfolds=7)
elasticFit2 <- cv.glmnet(xTrain, yTrain, alpha=0.25, nfolds=7)
plot(elasticFit2)
summary(elasticFit2)
elasticFit2$lambda.1se
elasticFit2$lambda.min
#making predictions
elasticPred <- predict(elasticFit1, newx=xTest, s="lambda.min")
rmseElastic <- sqrt(mean((elasticPred - yTest)^2))
rmseElastic
