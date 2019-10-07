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
#source("C:/Koma/Github/komazsofi/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") #set where the Function*.R file located
source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R")

res=5

# Set global variables
#setwd("D:/Koma/Paper1_v2/Run4_2019April/")
#setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_17April/")
setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/")

# Import

featuretable_l1_100=read.csv(paste("featuretable_l1_",res,"_100.csv",sep=""))
featuretable_l2_100=read.csv(paste("featuretable_l2_",res,"_100.csv",sep=""))
featuretable_l3_100=read.csv(paste("featuretable_l3_",res,"_100.csv",sep=""))

featuretable_l1_500=read.csv(paste("featuretable_l1_",res,"_500.csv",sep=""))
featuretable_l2_500=read.csv(paste("featuretable_l2_",res,"_500.csv",sep=""))
featuretable_l3_500=read.csv(paste("featuretable_l3_",res,"_500.csv",sep=""))

featuretable_l1_1000=read.csv(paste("featuretable_l1_",res,"_1000.csv",sep=""))
featuretable_l2_1000=read.csv(paste("featuretable_l2_",res,"_1000.csv",sep=""))
featuretable_l3_1000=read.csv(paste("featuretable_l3_",res,"_1000.csv",sep=""))

# One runs level 1
##
first_seed <- 5
accuracies_l1_100 <-c()
kappa_l1_100 <-c()
confm_m_100 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l1_100 <- caret::createDataPartition(y=featuretable_l1_100$V3, p=0.75, list=FALSE)
  trainingSet_l1_100 <- featuretable_l1_100[trainIndex_l1_100,]
  testingSet_l1_100 <- featuretable_l1_100[-trainIndex_l1_100,]
  
  modelFit_l1_100 <- randomForest(trainingSet_l1_100[,1:32],factor(trainingSet_l1_100$V3),ntree=100,importance = TRUE)
  prediction_l1_100 <- predict(modelFit_l1_100,testingSet_l1_100[ ,1:32])
  
  conf_m_l1_100=confusionMatrix(factor(prediction_l1_100), factor(testingSet_l1_100$V3),mode = "everything")
  
  accuracies_l1_100 <- c(accuracies_l1_100,conf_m_l1_100$overall["Accuracy"])
  kappa_l1_100 <- c(kappa_l1_100,conf_m_l1_100$overall["Kappa"])
  confm_m_100[i,"TP"]=conf_m_l1_100$table[1]
  confm_m_100[i,"FN"]=conf_m_l1_100$table[2]
  confm_m_100[i,"FP"]=conf_m_l1_100$table[3]
  confm_m_100[i,"TN"]=conf_m_l1_100$table[4]
}

confm_m_100$useracc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FP)
confm_m_100$prodacc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FN)

confm_m_100$useracc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FN)
confm_m_100$prodacc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FP)

sink(paste("acc_l1_multi_",res,"_100.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l1_100),3)*100,"+-",round(sd(accuracies_l1_100),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l1_100),3),"+-",round(sd(kappa_l1_100),2)))
sink()

##
first_seed <- 5
accuracies_l1_500 <-c()
kappa_l1_500 <-c()
confm_m_500 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l1_500 <- caret::createDataPartition(y=featuretable_l1_500$V3, p=0.75, list=FALSE)
  trainingSet_l1_500 <- featuretable_l1_500[trainIndex_l1_500,]
  testingSet_l1_500 <- featuretable_l1_500[-trainIndex_l1_500,]
  
  modelFit_l1_500 <- randomForest(trainingSet_l1_500[,1:32],factor(trainingSet_l1_500$V3),ntree=100,importance = TRUE)
  prediction_l1_500 <- predict(modelFit_l1_500,testingSet_l1_500[ ,1:32])
  
  conf_m_l1_500=confusionMatrix(factor(prediction_l1_500), factor(testingSet_l1_500$V3),mode = "everything")
  
  accuracies_l1_500 <- c(accuracies_l1_500,conf_m_l1_500$overall["Accuracy"])
  kappa_l1_500 <- c(kappa_l1_500,conf_m_l1_500$overall["Kappa"])
  confm_m_500[i,"TP"]=conf_m_l1_500$table[1]
  confm_m_500[i,"FN"]=conf_m_l1_500$table[2]
  confm_m_500[i,"FP"]=conf_m_l1_500$table[3]
  confm_m_500[i,"TN"]=conf_m_l1_500$table[4]
}

confm_m_500$useracc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FP)
confm_m_500$prodacc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FN)

confm_m_500$useracc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FN)
confm_m_500$prodacc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FP)

sink(paste("acc_l1_multi_",res,"_500.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l1_500),3)*100,"+-",round(sd(accuracies_l1_500),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l1_500),3),"+-",round(sd(kappa_l1_500),2)))
sink()
##
first_seed <- 5
accuracies_l1_1000 <-c()
kappa_l1_1000 <-c()
confm_m_1000 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l1_1000 <- caret::createDataPartition(y=featuretable_l1_1000$V3, p=0.75, list=FALSE)
  trainingSet_l1_1000 <- featuretable_l1_1000[trainIndex_l1_1000,]
  testingSet_l1_1000 <- featuretable_l1_1000[-trainIndex_l1_1000,]
  
  modelFit_l1_1000 <- randomForest(trainingSet_l1_1000[,1:32],factor(trainingSet_l1_1000$V3),ntree=100,importance = TRUE)
  prediction_l1_1000 <- predict(modelFit_l1_1000,testingSet_l1_1000[ ,1:32])
  
  conf_m_l1_1000=confusionMatrix(factor(prediction_l1_1000), factor(testingSet_l1_1000$V3),mode = "everything")
  
  accuracies_l1_1000 <- c(accuracies_l1_1000,conf_m_l1_1000$overall["Accuracy"])
  kappa_l1_1000 <- c(kappa_l1_1000,conf_m_l1_1000$overall["Kappa"])
  confm_m_1000[i,"TP"]=conf_m_l1_1000$table[1]
  confm_m_1000[i,"FN"]=conf_m_l1_1000$table[2]
  confm_m_1000[i,"FP"]=conf_m_l1_1000$table[3]
  confm_m_1000[i,"TN"]=conf_m_l1_1000$table[4]
}

confm_m_1000$useracc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FP)
confm_m_1000$prodacc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FN)

confm_m_1000$useracc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FN)
confm_m_1000$prodacc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FP)

sink(paste("acc_l1_multi_",res,"_1000.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l1_1000),3)*100,"+-",round(sd(accuracies_l1_1000),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l1_1000),3),"+-",round(sd(kappa_l1_1000),2)))
sink()

# One runs level 2
##
first_seed <- 5
accuracies_l2_100 <-c()
kappa_l2_100 <-c()
confm_m_100 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l2_100 <- caret::createDataPartition(y=featuretable_l2_100$V3, p=0.75, list=FALSE)
  trainingSet_l2_100 <- featuretable_l2_100[trainIndex_l2_100,]
  testingSet_l2_100 <- featuretable_l2_100[-trainIndex_l2_100,]
  
  modelFit_l2_100 <- randomForest(trainingSet_l2_100[,1:32],factor(trainingSet_l2_100$V3),ntree=100,importance = TRUE)
  prediction_l2_100 <- predict(modelFit_l2_100,testingSet_l2_100[ ,1:32])
  
  conf_m_l2_100=confusionMatrix(factor(prediction_l2_100), factor(testingSet_l2_100$V3),mode = "everything")
  
  accuracies_l2_100 <- c(accuracies_l2_100,conf_m_l2_100$overall["Accuracy"])
  kappa_l2_100 <- c(kappa_l2_100,conf_m_l2_100$overall["Kappa"])
  confm_m_100[i,"TP"]=conf_m_l2_100$table[1]
  confm_m_100[i,"FN"]=conf_m_l2_100$table[2]
  confm_m_100[i,"FP"]=conf_m_l2_100$table[3]
  confm_m_100[i,"TN"]=conf_m_l2_100$table[4]
}

confm_m_100$useracc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FP)
confm_m_100$prodacc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FN)

confm_m_100$useracc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FN)
confm_m_100$prodacc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FP)

sink(paste("acc_l2_multi_",res,"_100.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l2_100),3)*100,"+-",round(sd(accuracies_l2_100),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l2_100),3),"+-",round(sd(kappa_l2_100),2)))
sink()

##
first_seed <- 5
accuracies_l2_500 <-c()
kappa_l2_500 <-c()
confm_m_500 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l2_500 <- caret::createDataPartition(y=featuretable_l2_500$V3, p=0.75, list=FALSE)
  trainingSet_l2_500 <- featuretable_l2_500[trainIndex_l2_500,]
  testingSet_l2_500 <- featuretable_l2_500[-trainIndex_l2_500,]
  
  modelFit_l2_500 <- randomForest(trainingSet_l2_500[,1:32],factor(trainingSet_l2_500$V3),ntree=100,importance = TRUE)
  prediction_l2_500 <- predict(modelFit_l2_500,testingSet_l2_500[ ,1:32])
  
  conf_m_l2_500=confusionMatrix(factor(prediction_l2_500), factor(testingSet_l2_500$V3),mode = "everything")
  
  accuracies_l2_500 <- c(accuracies_l2_500,conf_m_l2_500$overall["Accuracy"])
  kappa_l2_500 <- c(kappa_l2_500,conf_m_l2_500$overall["Kappa"])
  confm_m_500[i,"TP"]=conf_m_l2_500$table[1]
  confm_m_500[i,"FN"]=conf_m_l2_500$table[2]
  confm_m_500[i,"FP"]=conf_m_l2_500$table[3]
  confm_m_500[i,"TN"]=conf_m_l2_500$table[4]
}

confm_m_500$useracc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FP)
confm_m_500$prodacc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FN)

confm_m_500$useracc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FN)
confm_m_500$prodacc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FP)

sink(paste("acc_l2_multi_",res,"_500.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l2_500),3)*100,"+-",round(sd(accuracies_l2_500),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l2_500),3),"+-",round(sd(kappa_l2_500),2)))
sink()
##
first_seed <- 5
accuracies_l2_1000 <-c()
kappa_l2_1000 <-c()
confm_m_1000 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l2_1000 <- caret::createDataPartition(y=featuretable_l2_1000$V3, p=0.75, list=FALSE)
  trainingSet_l2_1000 <- featuretable_l2_1000[trainIndex_l2_1000,]
  testingSet_l2_1000 <- featuretable_l2_1000[-trainIndex_l2_1000,]
  
  modelFit_l2_1000 <- randomForest(trainingSet_l2_1000[,1:32],factor(trainingSet_l2_1000$V3),ntree=100,importance = TRUE)
  prediction_l2_1000 <- predict(modelFit_l2_1000,testingSet_l2_1000[ ,1:32])
  
  conf_m_l2_1000=confusionMatrix(factor(prediction_l2_1000), factor(testingSet_l2_1000$V3),mode = "everything")
  
  accuracies_l2_1000 <- c(accuracies_l2_1000,conf_m_l2_1000$overall["Accuracy"])
  kappa_l2_1000 <- c(kappa_l2_1000,conf_m_l2_1000$overall["Kappa"])
  confm_m_1000[i,"TP"]=conf_m_l2_1000$table[1]
  confm_m_1000[i,"FN"]=conf_m_l2_1000$table[2]
  confm_m_1000[i,"FP"]=conf_m_l2_1000$table[3]
  confm_m_1000[i,"TN"]=conf_m_l2_1000$table[4]
}

confm_m_1000$useracc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FP)
confm_m_1000$prodacc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FN)

confm_m_1000$useracc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FN)
confm_m_1000$prodacc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FP)

sink(paste("acc_l2_multi_",res,"_1000.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l2_1000),3)*100,"+-",round(sd(accuracies_l2_1000),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l2_1000),3),"+-",round(sd(kappa_l2_1000),2)))
sink()

# One runs level 3
##
first_seed <- 5
accuracies_l3_100 <-c()
kappa_l3_100 <-c()
confm_m_100 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l3_100 <- caret::createDataPartition(y=featuretable_l3_100$V3, p=0.75, list=FALSE)
  trainingSet_l3_100 <- featuretable_l3_100[trainIndex_l3_100,]
  testingSet_l3_100 <- featuretable_l3_100[-trainIndex_l3_100,]
  
  modelFit_l3_100 <- randomForest(trainingSet_l3_100[,1:32],factor(trainingSet_l3_100$V3),ntree=100,importance = TRUE)
  prediction_l3_100 <- predict(modelFit_l3_100,testingSet_l3_100[ ,1:32])
  
  conf_m_l3_100=confusionMatrix(factor(prediction_l3_100), factor(testingSet_l3_100$V3),mode = "everything")
  
  accuracies_l3_100 <- c(accuracies_l3_100,conf_m_l3_100$overall["Accuracy"])
  kappa_l3_100 <- c(kappa_l3_100,conf_m_l3_100$overall["Kappa"])
  confm_m_100[i,"TP"]=conf_m_l3_100$table[1]
  confm_m_100[i,"FN"]=conf_m_l3_100$table[2]
  confm_m_100[i,"FP"]=conf_m_l3_100$table[3]
  confm_m_100[i,"TN"]=conf_m_l3_100$table[4]
}

confm_m_100$useracc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FP)
confm_m_100$prodacc_p = confm_m_100$TP/(confm_m_100$TP+confm_m_100$FN)

confm_m_100$useracc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FN)
confm_m_100$prodacc_n = confm_m_100$TN/(confm_m_100$TN+confm_m_100$FP)

sink(paste("acc_l3_multi_",res,"_100.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l3_100),3)*100,"+-",round(sd(accuracies_l3_100),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l3_100),3),"+-",round(sd(kappa_l3_100),2)))
sink()

##
first_seed <- 5
accuracies_l3_500 <-c()
kappa_l3_500 <-c()
confm_m_500 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l3_500 <- caret::createDataPartition(y=featuretable_l3_500$V3, p=0.75, list=FALSE)
  trainingSet_l3_500 <- featuretable_l3_500[trainIndex_l3_500,]
  testingSet_l3_500 <- featuretable_l3_500[-trainIndex_l3_500,]
  
  modelFit_l3_500 <- randomForest(trainingSet_l3_500[,1:32],factor(trainingSet_l3_500$V3),ntree=100,importance = TRUE)
  prediction_l3_500 <- predict(modelFit_l3_500,testingSet_l3_500[ ,1:32])
  
  conf_m_l3_500=confusionMatrix(factor(prediction_l3_500), factor(testingSet_l3_500$V3),mode = "everything")
  
  accuracies_l3_500 <- c(accuracies_l3_500,conf_m_l3_500$overall["Accuracy"])
  kappa_l3_500 <- c(kappa_l3_500,conf_m_l3_500$overall["Kappa"])
  confm_m_500[i,"TP"]=conf_m_l3_500$table[1]
  confm_m_500[i,"FN"]=conf_m_l3_500$table[2]
  confm_m_500[i,"FP"]=conf_m_l3_500$table[3]
  confm_m_500[i,"TN"]=conf_m_l3_500$table[4]
}

confm_m_500$useracc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FP)
confm_m_500$prodacc_p = confm_m_500$TP/(confm_m_500$TP+confm_m_500$FN)

confm_m_500$useracc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FN)
confm_m_500$prodacc_n = confm_m_500$TN/(confm_m_500$TN+confm_m_500$FP)

sink(paste("acc_l3_multi_",res,"_500.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l3_500),3)*100,"+-",round(sd(accuracies_l3_500),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l3_500),3),"+-",round(sd(kappa_l3_500),2)))
sink()
##
first_seed <- 5
accuracies_l3_1000 <-c()
kappa_l3_1000 <-c()
confm_m_1000 <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:100){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l3_1000 <- caret::createDataPartition(y=featuretable_l3_1000$V3, p=0.75, list=FALSE)
  trainingSet_l3_1000 <- featuretable_l3_1000[trainIndex_l3_1000,]
  testingSet_l3_1000 <- featuretable_l3_1000[-trainIndex_l3_1000,]
  
  modelFit_l3_1000 <- randomForest(trainingSet_l3_1000[,1:32],factor(trainingSet_l3_1000$V3),ntree=100,importance = TRUE)
  prediction_l3_1000 <- predict(modelFit_l3_1000,testingSet_l3_1000[ ,1:32])
  
  conf_m_l3_1000=confusionMatrix(factor(prediction_l3_1000), factor(testingSet_l3_1000$V3),mode = "everything")
  
  accuracies_l3_1000 <- c(accuracies_l3_1000,conf_m_l3_1000$overall["Accuracy"])
  kappa_l3_1000 <- c(kappa_l3_1000,conf_m_l3_1000$overall["Kappa"])
  confm_m_1000[i,"TP"]=conf_m_l3_1000$table[1]
  confm_m_1000[i,"FN"]=conf_m_l3_1000$table[2]
  confm_m_1000[i,"FP"]=conf_m_l3_1000$table[3]
  confm_m_1000[i,"TN"]=conf_m_l3_1000$table[4]
}

confm_m_1000$useracc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FP)
confm_m_1000$prodacc_p = confm_m_1000$TP/(confm_m_1000$TP+confm_m_1000$FN)

confm_m_1000$useracc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FN)
confm_m_1000$prodacc_n = confm_m_1000$TN/(confm_m_1000$TN+confm_m_1000$FP)

sink(paste("acc_l3_multi_",res,"_1000.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l3_1000),3)*100,"+-",round(sd(accuracies_l3_1000),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l3_1000),3),"+-",round(sd(kappa_l3_1000),2)))
sink()