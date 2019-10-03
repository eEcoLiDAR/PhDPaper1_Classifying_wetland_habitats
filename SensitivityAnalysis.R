"
@author: Zsofia Koma, UvA
Aim: Sensitivity analysis for resolution and number of selected training data
"

library(randomForest)
library(caret)
library(rfUtilities)

library(ggplot2)
library(gridExtra)
library(ggrepel)

library(reshape2)
library(corrplot)

#source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") 
source("C:/Koma/Github/komazsofi/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") #set where the Function*.R file located


# Set global variables
#setwd("D:/Koma/Paper1_v2/Run4_2019April/")
#setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_17April/")
setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/")

# Import

featuretable_l1_100=read.csv("featuretable_level1_100.csv")
featuretable_l2_100=read.csv("featuretable_level2_100.csv")
featuretable_l3_100=read.csv("featuretable_level3_100.csv")

featuretable_l1_500=read.csv("featuretable_level1_500.csv")
featuretable_l2_500=read.csv("featuretable_level2_500.csv")
featuretable_l3_500=read.csv("featuretable_level3_500.csv")

featuretable_l1_1000=read.csv("featuretable_level1_1000.csv")
featuretable_l2_1000=read.csv("featuretable_level2_1000.csv")
featuretable_l3_1000=read.csv("featuretable_level3_1000.csv")

# One runs level 1
##
trainIndex_l1_100 <- caret::createDataPartition(y=featuretable_l1_100$layer, p=0.75, list=FALSE)
trainingSet_l1_100 <- featuretable_l1_100[trainIndex_l1_100,]
testingSet_l1_100 <- featuretable_l1_100[-trainIndex_l1_100,]

modelFit_l1_100 <- randomForest(trainingSet_l1_100[,1:32],factor(trainingSet_l1_100$layer),ntree=100,importance = TRUE)
prediction_l1_100 <- predict(modelFit_l1_100,testingSet_l1_100[ ,1:32])

conf_m_l1_100=confusionMatrix(factor(prediction_l1_100), factor(testingSet_l1_100$layer),mode = "everything")
##
trainIndex_l1_500 <- caret::createDataPartition(y=featuretable_l1_500$layer, p=0.75, list=FALSE)
trainingSet_l1_500 <- featuretable_l1_500[trainIndex_l1_500,]
testingSet_l1_500 <- featuretable_l1_500[-trainIndex_l1_500,]

modelFit_l1_500 <- randomForest(trainingSet_l1_500[,1:32],factor(trainingSet_l1_500$layer),ntree=100,importance = TRUE)
prediction_l1_500 <- predict(modelFit_l1_500,testingSet_l1_500[ ,1:32])

conf_m_l1_500=confusionMatrix(factor(prediction_l1_500), factor(testingSet_l1_500$layer),mode = "everything")
##
trainIndex_l1_1000 <- caret::createDataPartition(y=featuretable_l1_1000$layer, p=0.75, list=FALSE)
trainingSet_l1_1000 <- featuretable_l1_1000[trainIndex_l1_1000,]
testingSet_l1_1000 <- featuretable_l1_100[-trainIndex_l1_1000,]

modelFit_l1_1000 <- randomForest(trainingSet_l1_1000[,1:32],factor(trainingSet_l1_1000$layer),ntree=100,importance = TRUE)
prediction_l1_1000 <- predict(modelFit_l1_1000,testingSet_l1_1000[ ,1:32])

conf_m_l1_1000=confusionMatrix(factor(prediction_l1_1000), factor(testingSet_l1_1000$layer),mode = "everything")

print(conf_m_l1_100)
print(conf_m_l1_500)
print(conf_m_l1_1000)

# One runs level 2
##
trainIndex_l2_100 <- caret::createDataPartition(y=featuretable_l2_100$layer, p=0.75, list=FALSE)
trainingSet_l2_100 <- featuretable_l2_100[trainIndex_l2_100,]
testingSet_l2_100 <- featuretable_l2_100[-trainIndex_l2_100,]

modelFit_l2_100 <- randomForest(trainingSet_l2_100[,1:32],factor(trainingSet_l2_100$layer),ntree=100,importance = TRUE)
prediction_l2_100 <- predict(modelFit_l2_100,testingSet_l2_100[ ,1:32])

conf_m_l2_100=confusionMatrix(factor(prediction_l2_100), factor(testingSet_l2_100$layer),mode = "everything")
##
trainIndex_l2_500 <- caret::createDataPartition(y=featuretable_l2_500$layer, p=0.75, list=FALSE)
trainingSet_l2_500 <- featuretable_l2_500[trainIndex_l2_500,]
testingSet_l2_500 <- featuretable_l2_500[-trainIndex_l2_500,]

modelFit_l2_500 <- randomForest(trainingSet_l2_500[,1:32],factor(trainingSet_l2_500$layer),ntree=100,importance = TRUE)
prediction_l2_500 <- predict(modelFit_l2_500,testingSet_l2_500[ ,1:32])

conf_m_l2_500=confusionMatrix(factor(prediction_l2_500), factor(testingSet_l2_500$layer),mode = "everything")
##
trainIndex_l2_1000 <- caret::createDataPartition(y=featuretable_l2_1000$layer, p=0.75, list=FALSE)
trainingSet_l2_1000 <- featuretable_l2_1000[trainIndex_l2_1000,]
testingSet_l2_1000 <- featuretable_l2_100[-trainIndex_l2_1000,]

modelFit_l2_1000 <- randomForest(trainingSet_l2_1000[,1:32],factor(trainingSet_l2_1000$layer),ntree=100,importance = TRUE)
prediction_l2_1000 <- predict(modelFit_l2_1000,testingSet_l2_1000[ ,1:32])

conf_m_l2_1000=confusionMatrix(factor(prediction_l2_1000), factor(testingSet_l2_1000$layer),mode = "everything")

print(conf_m_l2_100)
print(conf_m_l2_500)
print(conf_m_l2_1000)

# One runs level 3
##
trainIndex_l3_100 <- caret::createDataPartition(y=featuretable_l3_100$layer, p=0.75, list=FALSE)
trainingSet_l3_100 <- featuretable_l3_100[trainIndex_l3_100,]
testingSet_l3_100 <- featuretable_l3_100[-trainIndex_l3_100,]

modelFit_l3_100 <- randomForest(trainingSet_l3_100[,1:32],factor(trainingSet_l3_100$layer),ntree=100,importance = TRUE)
prediction_l3_100 <- predict(modelFit_l3_100,testingSet_l3_100[ ,1:32])

conf_m_l3_100=confusionMatrix(factor(prediction_l3_100), factor(testingSet_l3_100$layer),mode = "everything")
##
trainIndex_l3_500 <- caret::createDataPartition(y=featuretable_l3_500$layer, p=0.75, list=FALSE)
trainingSet_l3_500 <- featuretable_l3_500[trainIndex_l3_500,]
testingSet_l3_500 <- featuretable_l3_500[-trainIndex_l3_500,]

modelFit_l3_500 <- randomForest(trainingSet_l3_500[,1:32],factor(trainingSet_l3_500$layer),ntree=100,importance = TRUE)
prediction_l3_500 <- predict(modelFit_l3_500,testingSet_l3_500[ ,1:32])

conf_m_l3_500=confusionMatrix(factor(prediction_l3_500), factor(testingSet_l3_500$layer),mode = "everything")
##
trainIndex_l3_1000 <- caret::createDataPartition(y=featuretable_l3_1000$layer, p=0.75, list=FALSE)
trainingSet_l3_1000 <- featuretable_l3_1000[trainIndex_l3_1000,]
testingSet_l3_1000 <- featuretable_l3_100[-trainIndex_l3_1000,]

modelFit_l3_1000 <- randomForest(trainingSet_l3_1000[,1:32],factor(trainingSet_l3_1000$layer),ntree=100,importance = TRUE)
prediction_l3_1000 <- predict(modelFit_l3_1000,testingSet_l3_1000[ ,1:32])

conf_m_l3_1000=confusionMatrix(factor(prediction_l3_1000), factor(testingSet_l3_1000$layer),mode = "everything")

print(conf_m_l3_100)
print(conf_m_l3_500)
print(conf_m_l3_1000)