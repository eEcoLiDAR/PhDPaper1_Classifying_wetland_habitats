"
@author: Zsofia Koma, UvA
Aim: Analyse the results of the classification - feature importance and response curves
"

library(randomForest)
library(caret)
library(rfUtilities)

library(ggplot2)
library(gridExtra)
library(ggrepel)

library(reshape2)
library(corrplot)

source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R")
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") 
#source("C:/Koma/Github/komazsofi/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") #set where the Function*.R file located


# Set global variables
setwd("D:/Koma/Paper1/Revision/Results/5m/")
#setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/")
#setwd("C:/Koma/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_17April/")

# Import

featuretable_l1=read.csv("featuretable_level1_500_5.csv")
featuretable_l2=read.csv("featuretable_level2_500_5.csv")
featuretable_l3=read.csv("featuretable_level3_500_5.csv")

featuretable_l1=featuretable_l1[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","curvature","zvar","zskew","zkurto","canrelrat","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l2=featuretable_l2[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","curvature","zvar","zskew","zkurto","canrelrat","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l3=featuretable_l3[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","curvature","zvar","zskew","zkurto","canrelrat","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]

# Pre-process - rename coloumns, add feature classes

names(featuretable_l1) <- c("C_can","C_b2","C_2-5","S_lin","S_curv","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l2) <- c("C_can","C_b2","C_2-5","S_lin","S_curv","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l3) <- c("C_can","C_b2","C_2-5","S_lin","S_curv","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")


# RFE

# level 1
control <- rfeControl(functions=rfFuncs, method="cv", number=100,returnResamp = "all")
#set.seed(50)
rfe_l1 <- rfe(featuretable_l1[,1:16], factor(featuretable_l1$V3), rfeControl=control,sizes=c(1:16),ntree=100,maximize = TRUE)

# level 2
control <- rfeControl(functions=rfFuncs, method="cv", number=100,returnResamp = "all")
#set.seed(50)
rfe_l2 <- rfe(featuretable_l2[,1:16], factor(featuretable_l2$V3), rfeControl=control,sizes=c(1:16),ntree=100,maximize = TRUE)

# level 3
control <- rfeControl(functions=rfFuncs, method="cv", number=100,returnResamp = "all")
#set.seed(50)
rfe_l3 <- rfe(featuretable_l3[,1:16], factor(featuretable_l3$V3), rfeControl=control,sizes=c(1:16),ntree=100,maximize = TRUE)


absoluteBest_l1 <- pickSizeBest(rfe_l1$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l1 <- pickSizeTolerance(rfe_l1$results, metric = "Accuracy", maximize = TRUE,tol = 2.5)

absoluteBest_l2 <- pickSizeBest(rfe_l2$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l2 <- pickSizeTolerance(rfe_l2$results, metric = "Accuracy", maximize = TRUE,tol = 2.5)

absoluteBest_l3 <- pickSizeBest(rfe_l3$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l3 <- pickSizeTolerance(rfe_l3$results, metric = "Accuracy", maximize = TRUE,tol = 2.5)

save(rfe_l1,file="rfe_l1.RData")
save(rfe_l2,file="rfe_l2.RData")
save(rfe_l3,file="rfe_l3.RData")

# Get RF with min number of features

load("rfe_l1.RData")
load("rfe_l2.RData")
load("rfe_l3.RData")

# RFE with opt. nof fea -- create valid confusion matrix

# level 1
control <- rfeControl(functions=rfFuncs, method="cv", number=50,returnResamp = "all")
#set.seed(50)
rfe_l1_facc <- rfe(featuretable_l1[,rfe_l1$optVariables[1:5]],factor(featuretable_l1$layer), rfeControl=control,sizes=c(1:5),ntree=100,maximize = TRUE)

conf_m_fromrfe_l1_cl1=confusionMatrix(rfe_l1_facc[["fit"]][["predicted"]], rfe_l1_facc[["fit"]][["y"]],mode = "everything",positive = "1")
conf_m_fromrfe_l1_cl2=confusionMatrix(rfe_l1_facc[["fit"]][["predicted"]], rfe_l1_facc[["fit"]][["y"]],mode = "everything",positive = "2")

sink(paste("acc_l1_rfe.txt",sep=""))
print("pos class 1")
print(conf_m_fromrfe_l1_cl1)
print("pos class 2")
print(conf_m_fromrfe_l1_cl2)
sink()

# level 2
control <- rfeControl(functions=rfFuncs, method="cv", number=50,returnResamp = "all")
set.seed(50)
rfe_l2_facc <- rfe(featuretable_l2[,rfe_l2$optVariables[1:10]],factor(featuretable_l2$layer), rfeControl=control,sizes=c(1,10),ntree=100,maximize = TRUE)

conf_m_fromrfe_l2=confusionMatrix(rfe_l2_facc[["fit"]][["predicted"]], rfe_l2_facc[["fit"]][["y"]],mode = "everything")

sink(paste("acc_l2_rfe.txt",sep=""))
print(conf_m_fromrfe_l2)
sink()

# level 3
control <- rfeControl(functions=rfFuncs, method="cv", number=50,returnResamp = "all")
set.seed(50)
rfe_l3_facc <- rfe(featuretable_l3[,rfe_l3$optVariables[1:10]],factor(featuretable_l3$layer), rfeControl=control,sizes=c(1,10),ntree=100,maximize = TRUE)

conf_m_fromrfe_l3=confusionMatrix(rfe_l3_facc[["fit"]][["predicted"]], rfe_l3_facc[["fit"]][["y"]],mode = "everything")

sink(paste("acc_l3_rfe.txt",sep=""))
print(conf_m_fromrfe_l3)
sink()

# level 1

# multiple run
first_seed <- 5
accuracies_l1 <-c()
kappa_l1 <-c()
confm_m <- data.frame(TP=integer(),FN=integer(),FP=integer(),TN=integer())

for (i in 1:50){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l1 <- caret::createDataPartition(y=featuretable_l1$layer, p=0.75, list=FALSE)
  trainingSet_l1 <- featuretable_l1[trainIndex_l1,]
  testingSet_l1 <- featuretable_l1[-trainIndex_l1,]
  
  modelFit_l1 <- randomForest(trainingSet_l1[,rfe_l1$optVariables[1:5]],factor(trainingSet_l1$layer),ntree=100,importance = TRUE)
  prediction_l1 <- predict(modelFit_l1,testingSet_l1[ ,rfe_l1$optVariables[1:5]])
  
  conf_m_l1=confusionMatrix(factor(prediction_l1), factor(testingSet_l1$layer),mode = "everything")
  
  accuracies_l1 <- c(accuracies_l1,conf_m_l1$overall["Accuracy"])
  kappa_l1 <- c(kappa_l1,conf_m_l1$overall["Kappa"])
  confm_m[i,"TP"]=conf_m_l1$table[1]
  confm_m[i,"FN"]=conf_m_l1$table[2]
  confm_m[i,"FP"]=conf_m_l1$table[3]
  confm_m[i,"TN"]=conf_m_l1$table[4]
}

confm_m$useracc_p = confm_m$TP/(confm_m$TP+confm_m$FP)
confm_m$prodacc_p = confm_m$TP/(confm_m$TP+confm_m$FN)

confm_m$useracc_n = confm_m$TN/(confm_m$TN+confm_m$FN)
confm_m$prodacc_n = confm_m$TN/(confm_m$TN+confm_m$FP)

sink(paste("acc_l1_multi.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l1),3)*100,"+-",round(sd(accuracies_l1),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l1),3),"+-",round(sd(kappa_l1),2)))
print(paste("Multi run user acc class1:",round(mean(confm_m$useracc_p),3)*100,"+-",round(sd(confm_m$useracc_p),2)*100))
print(paste("Multi run pred acc acc class1:",round(mean(confm_m$prodacc_p),3)*100,"+-",round(sd(confm_m$prodacc_p),2)*100))
print(paste("Multi run user acc acc class2:",round(mean(confm_m$useracc_n),3)*100,"+-",round(sd(confm_m$useracc_n),2)*100))
print(paste("Multi run pred acc acc class2:",round(mean(confm_m$prodacc_n),3)*100,"+-",round(sd(confm_m$prodacc_n),2)*100))
sink()

# 1 run
trainIndex_l1 <- caret::createDataPartition(y=featuretable_l1$layer, p=0.75, list=FALSE)
trainingSet_l1 <- featuretable_l1[trainIndex_l1,]
testingSet_l1 <- featuretable_l1[-trainIndex_l1,]

modelFit_l1 <- randomForest(trainingSet_l1[,rfe_l1$optVariables[1:within5Pct_l1]],factor(trainingSet_l1$layer),ntree=100,importance = TRUE)
prediction_l1 <- predict(modelFit_l1,testingSet_l1[ ,rfe_l1$optVariables[1:within5Pct_l1]])

conf_m_l1=confusionMatrix(factor(prediction_l1), factor(testingSet_l1$layer),mode = "everything")

# level 2

# multiple run
first_seed <- 5
accuracies_l2 <-c()
kappa_l2 <-c()
usersacc_c1_l2 <-c()
usersacc_c2_l2 <-c()
usersacc_c3_l2 <-c()
usersacc_c4_l2 <-c()

prodacc_c1_l2 <-c()
prodacc_c2_l2 <-c()
prodacc_c3_l2 <-c()
prodacc_c4_l2 <-c()

for (i in 1:50){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l2 <- caret::createDataPartition(y=featuretable_l2$layer, p=0.75, list=FALSE)
  trainingSet_l2 <- featuretable_l2[trainIndex_l2,]
  testingSet_l2 <- featuretable_l2[-trainIndex_l2,]
  
  modelFit_l2 <- randomForest(trainingSet_l2[,rfe_l2$optVariables[1:10]],factor(trainingSet_l2$layer),ntree=100,importance = TRUE)
  prediction_l2 <- predict(modelFit_l2,testingSet_l2[ ,rfe_l2$optVariables[1:10]])
  
  conf_m_l2=confusionMatrix(factor(prediction_l2), factor(testingSet_l2$layer),mode = "everything")
  
  accuracies_l2 <- c(accuracies_l2,conf_m_l2$overall["Accuracy"])
  kappa_l2 <- c(kappa_l2,conf_m_l2$overall["Kappa"])
  
  usersacc_c1_l2 <- c(usersacc_c1_l2,conf_m_l2$byClass[1,"Precision"])
  usersacc_c2_l2 <- c(usersacc_c2_l2,conf_m_l2$byClass[2,"Precision"])
  usersacc_c3_l2 <- c(usersacc_c3_l2,conf_m_l2$byClass[3,"Precision"])
  usersacc_c4_l2 <- c(usersacc_c4_l2,conf_m_l2$byClass[4,"Precision"])
  
  prodacc_c1_l2 <- c(prodacc_c1_l2,conf_m_l2$byClass[1,"Recall"])
  prodacc_c2_l2 <- c(prodacc_c2_l2,conf_m_l2$byClass[2,"Recall"])
  prodacc_c3_l2 <- c(prodacc_c3_l2,conf_m_l2$byClass[3,"Recall"])
  prodacc_c4_l2 <- c(prodacc_c4_l2,conf_m_l2$byClass[4,"Recall"])
  
}

sink(paste("acc_l2_multi.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l2),3)*100,round(sd(accuracies_l2),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l2),3),round(sd(kappa_l2),2)))

print(paste("Multi run user acc class1:",round(mean(usersacc_c1_l2),3)*100,round(sd(usersacc_c1_l2),2)*100))
print(paste("Multi run user acc class2:",round(mean(usersacc_c2_l2),3)*100,round(sd(usersacc_c2_l2),2)*100))
print(paste("Multi run user acc class3:",round(mean(usersacc_c3_l2),3)*100,round(sd(usersacc_c3_l2),2)*100))
print(paste("Multi run user acc class4:",round(mean(usersacc_c4_l2),3)*100,round(sd(usersacc_c4_l2),2)*100))

print(paste("Multi run prod acc class1:",round(mean(prodacc_c1_l2),3)*100,round(sd(prodacc_c1_l2),2)*100))
print(paste("Multi run prod acc class2:",round(mean(prodacc_c2_l2),3)*100,round(sd(prodacc_c2_l2),2)*100))
print(paste("Multi run prod acc class3:",round(mean(prodacc_c3_l2),3)*100,round(sd(prodacc_c3_l2),2)*100))
print(paste("Multi run prod acc class4:",round(mean(prodacc_c4_l2),3)*100,round(sd(prodacc_c4_l2),2)*100))
sink()

# 1 run
trainIndex_l2 <- caret::createDataPartition(y=featuretable_l2$layer, p=0.75, list=FALSE)
trainingSet_l2 <- featuretable_l2[trainIndex_l2,]
testingSet_l2 <- featuretable_l2[-trainIndex_l2,]

modelFit_l2 <- randomForest(trainingSet_l2[,rfe_l2$optVariables[1:within5Pct_l2]],factor(trainingSet_l2$layer),ntree=100,importance = TRUE)
prediction_l2 <- predict(modelFit_l2,testingSet_l2[ ,rfe_l2$optVariables[1:within5Pct_l2]])

conf_m_l2=confusionMatrix(factor(prediction_l2), factor(testingSet_l2$layer),mode = "everything")

# level 3

# multiple run
first_seed <- 50
accuracies_l3 <-c()
kappa_l3 <-c()
usersacc_c1_l3 <-c()
usersacc_c2_l3 <-c()
usersacc_c3_l3 <-c()

prodacc_c1_l3 <-c()
prodacc_c2_l3 <-c()
prodacc_c3_l3 <-c()

for (i in 1:50){
  set.seed(first_seed)
  first_seed <- first_seed+1
  
  trainIndex_l3 <- caret::createDataPartition(y=featuretable_l3$layer, p=0.75, list=FALSE)
  trainingSet_l3 <- featuretable_l3[trainIndex_l3,]
  testingSet_l3 <- featuretable_l3[-trainIndex_l3,]
  
  modelFit_l3 <- randomForest(trainingSet_l3[,rfe_l3$optVariables[1:10]],factor(trainingSet_l3$layer),ntree=100,importance = TRUE)
  prediction_l3 <- predict(modelFit_l3,testingSet_l3[ ,rfe_l3$optVariables[1:10]])
  
  conf_m_l3=confusionMatrix(factor(prediction_l3), factor(testingSet_l3$layer),mode = "everything")
  
  accuracies_l3 <- c(accuracies_l3,conf_m_l3$overall["Accuracy"])
  kappa_l3 <- c(kappa_l3,conf_m_l3$overall["Kappa"])
  
  usersacc_c1_l3 <- c(usersacc_c1_l3,conf_m_l3$byClass[1,"Precision"])
  usersacc_c2_l3 <- c(usersacc_c2_l3,conf_m_l3$byClass[2,"Precision"])
  usersacc_c3_l3 <- c(usersacc_c3_l3,conf_m_l3$byClass[3,"Precision"])
  
  prodacc_c1_l3 <- c(prodacc_c1_l3,conf_m_l3$byClass[1,"Recall"])
  prodacc_c2_l3 <- c(prodacc_c2_l3,conf_m_l3$byClass[2,"Recall"])
  prodacc_c3_l3 <- c(prodacc_c3_l3,conf_m_l3$byClass[3,"Recall"])
  
}

sink(paste("acc_l3_multi.txt",sep=""))
print(paste("Multi run OA:",round(mean(accuracies_l3),3)*100,round(sd(accuracies_l3),2)*100))
print(paste("Multi run Kappa:",round(mean(kappa_l3),3),round(sd(kappa_l3),2)))

print(paste("Multi run user acc class1:",round(mean(usersacc_c1_l3),3)*100,round(sd(usersacc_c1_l3),2)*100))
print(paste("Multi run user acc class2:",round(mean(usersacc_c2_l3),3)*100,round(sd(usersacc_c2_l3),2)*100))
print(paste("Multi run user acc class3:",round(mean(usersacc_c3_l3),3)*100,round(sd(usersacc_c3_l3),2)*100))

print(paste("Multi run prod acc class1:",round(mean(prodacc_c1_l3),3)*100,round(sd(prodacc_c1_l3),2)*100))
print(paste("Multi run prod acc class2:",round(mean(prodacc_c2_l3),3)*100,round(sd(prodacc_c2_l3),2)*100))
print(paste("Multi run prod acc class3:",round(mean(prodacc_c3_l3),3)*100,round(sd(prodacc_c3_l3),2)*100))

sink()

# 1 run
trainIndex_l3 <- caret::createDataPartition(y=featuretable_l3$layer, p=0.75, list=FALSE)
trainingSet_l3 <- featuretable_l3[trainIndex_l3,]
testingSet_l3 <- featuretable_l3[-trainIndex_l3,]

modelFit_l3 <- randomForest(trainingSet_l3[,rfe_l3$optVariables[1:within5Pct_l3]],factor(trainingSet_l3$layer),ntree=100,importance = TRUE)
prediction_l3 <- predict(modelFit_l3,testingSet_l3[ ,rfe_l3$optVariables[1:within5Pct_l3]])

conf_m_l3=confusionMatrix(factor(prediction_l3), factor(testingSet_l3$layer),mode = "everything")

# Export

save(rfe_l1,file = "rfe_l1.RData")
save(rfe_l2,file = "rfe_l2.RData")
save(rfe_l3,file = "rfe_l3.RData")

sink(paste("acc_l1.txt",sep=""))
print(conf_m_l1)
sink()

sink(paste("acc_l2.txt",sep=""))
print(conf_m_l2)
sink()

sink(paste("acc_l3.txt",sep=""))
print(conf_m_l3)
sink()

save(modelFit_l1,file = "modelFit_l1.RData")
save(modelFit_l2,file = "modelFit_l2.RData")
save(modelFit_l3,file = "modelFit_l3.RData")

save(conf_m_l1,file = "conf_m_l1.RData")
save(conf_m_l2,file = "conf_m_l2.RData")
save(conf_m_l3,file = "conf_m_l3.RData")