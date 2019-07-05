"
@author: Zsofia Koma, UvA
Aim: Analyse the results of the classification - feature importance and response curves - functions
"

Analysis_FeatureImportance = function(forest)
{
  library(randomForestExplainer)
  
  importance_frame <- measure_importance(forest)
  
  importance_frame$norm_accuracy_decrease <- (importance_frame$accuracy_decrease-min(importance_frame$accuracy_decrease))/(max(importance_frame$accuracy_decrease)-min(importance_frame$accuracy_decrease))
  importance_frame$norm_gini_decrease <- (importance_frame$gini_decrease-min(importance_frame$gini_decrease))/(max(importance_frame$gini_decrease)-min(importance_frame$gini_decrease))
  
  return(importance_frame)
}

Response_l1 = function(forest_l1,featuretable_l1,id) {
  p1=partialPlot(forest_l1, featuretable_l1, impvar[id], 1, plot=FALSE)
  p2=partialPlot(forest_l1, featuretable_l1, impvar[id], 2, plot=FALSE)
  
  response_l1_c1 <- data.frame(p1[["x"]], p1[["y"]])
  names(response_l1_c1)[1]<-"class_1_x"
  names(response_l1_c1)[2]<-"class_1_y"
  response_l1_c1$class <- 1
  
  response_l1_c2 <- data.frame(p2[["x"]], p2[["y"]])
  names(response_l1_c2)[1]<-"class_1_x"
  names(response_l1_c2)[2]<-"class_1_y"
  response_l1_c2$class <- 2
  
  response_l1 <- rbind(response_l1_c1, response_l1_c2)
  
  return(response_l1)
}

Response_l2 = function(forest_l1,featuretable_l1,id) {
  p1=partialPlot(forest_l1, featuretable_l1, impvar[id], 1, plot=FALSE)
  p2=partialPlot(forest_l1, featuretable_l1, impvar[id], 2, plot=FALSE)
  p3=partialPlot(forest_l1, featuretable_l1, impvar[id], 3, plot=FALSE)
  p4=partialPlot(forest_l1, featuretable_l1, impvar[id], 4, plot=FALSE)
  p5=partialPlot(forest_l1, featuretable_l1, impvar[id], 5, plot=FALSE)
  
  response_l1_c1 <- data.frame(p1[["x"]], p1[["y"]])
  names(response_l1_c1)[1]<-"class_1_x"
  names(response_l1_c1)[2]<-"class_1_y"
  response_l1_c1$class <- 1
  
  response_l1_c2 <- data.frame(p2[["x"]], p2[["y"]])
  names(response_l1_c2)[1]<-"class_1_x"
  names(response_l1_c2)[2]<-"class_1_y"
  response_l1_c2$class <- 2
  
  response_l1_c3 <- data.frame(p3[["x"]], p3[["y"]])
  names(response_l1_c3)[1]<-"class_1_x"
  names(response_l1_c3)[2]<-"class_1_y"
  response_l1_c3$class <- 3
  
  response_l1_c4 <- data.frame(p4[["x"]], p4[["y"]])
  names(response_l1_c4)[1]<-"class_1_x"
  names(response_l1_c4)[2]<-"class_1_y"
  response_l1_c4$class <- 4
  
  response_l1_c5 <- data.frame(p5[["x"]], p5[["y"]])
  names(response_l1_c5)[1]<-"class_1_x"
  names(response_l1_c5)[2]<-"class_1_y"
  response_l1_c5$class <- 5
  
  response_l1 <- rbind(response_l1_c1, response_l1_c2, response_l1_c3, response_l1_c4, response_l1_c5)
  
  return(response_l1)
}

Response_l3 = function(forest_l1,featuretable_l1,id) {
  p1=partialPlot(forest_l1, featuretable_l1, impvar[id], 1, plot=FALSE)
  p2=partialPlot(forest_l1, featuretable_l1, impvar[id], 2, plot=FALSE)
  p3=partialPlot(forest_l1, featuretable_l1, impvar[id], 3, plot=FALSE)
  
  response_l1_c1 <- data.frame(p1[["x"]], p1[["y"]])
  names(response_l1_c1)[1]<-"class_1_x"
  names(response_l1_c1)[2]<-"class_1_y"
  response_l1_c1$class <- 1
  
  response_l1_c2 <- data.frame(p2[["x"]], p2[["y"]])
  names(response_l1_c2)[1]<-"class_1_x"
  names(response_l1_c2)[2]<-"class_1_y"
  response_l1_c2$class <- 2
  
  response_l1_c3 <- data.frame(p3[["x"]], p3[["y"]])
  names(response_l1_c3)[1]<-"class_1_x"
  names(response_l1_c3)[2]<-"class_1_y"
  response_l1_c3$class <- 3
  
  response_l1 <- rbind(response_l1_c1, response_l1_c2, response_l1_c3)
  
  return(response_l1)
}

add_varclass = function(importance_frame) {
  
  importance_frame$varclass <- NA
  importance_frame$varclass[importance_frame$variable=="C_can" | importance_frame$variable=="C_puls"] <- 1
  importance_frame$varclass[importance_frame$variable=="3S_curv" | importance_frame$variable == "3S_lin" | importance_frame$variable == "S_plan" | importance_frame$variable == "3S_sph" | importance_frame$variable == "3S_ani"] <-2
  importance_frame$varclass[importance_frame$variable == "VV_sd" | importance_frame$variable == "VV_var" | importance_frame$variable=="VV_skew" | importance_frame$variable == "VV_kurt" | importance_frame$variable == "VV_cr" | importance_frame$variable == "VV_vdr"
                                 | importance_frame$variable == "VV_simp" | importance_frame$variable == "VV_shan"] <- 3
  importance_frame$varclass[importance_frame$variable=="H_max" | importance_frame$variable=="H_mean" | importance_frame$variable == "H_med" | importance_frame$variable == "H_25p" | importance_frame$variable == "H_75p"
                                 | importance_frame$variable == "H_90p" ] <- 4
  importance_frame$varclass[importance_frame$variable=="HV_rough" | importance_frame$variable=="HV_tpi" | importance_frame$variable == "HV_tri" | importance_frame$variable == "HV_sd" | importance_frame$variable == "HV_var"] <- 5
  
  return(importance_frame)
  
}

