"
@author: Zsofia Koma, UvA
Aim: Functions for classification process
"

Create_FieldTraining = function(vegetation,level) 
{
  
  vegetation_poly <- vegetation[is.na(vegetation@data[,level]) == FALSE,]
  classes=unique(vegetation_poly@data[,level])
  
  for (cat in classes) { 
    print(cat)
    sel_poly <- vegetation_poly[vegetation_poly@data[,level] == cat,]
    points_inpoly=spsample(sel_poly, n = 75, "random")
    points_inpoly_df=as.data.frame(points_inpoly)
    points_inpoly_df$level=cat
    write.table(points_inpoly_df, file = paste(cat,"_selpolyper",names(vegetation@data)[level],"v2.csv",sep="_"),row.names=FALSE,col.names=FALSE,sep=",")
  }
  
  files <- list.files(pattern = paste("_selpolyper",names(vegetation_poly@data)[level],"v2.csv",sep="_"))
  
  allcsv <- lapply(files,function(i){
    read.csv(i, header=FALSE)
  })
  
  allcsv_df <- do.call(rbind.data.frame, allcsv)
  
  coordinates(allcsv_df)=~V1+V2
  proj4string(allcsv_df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  # Add buffer zone
  
  allcsv_df_buff <- gBuffer( allcsv_df, width=2.5, byid=TRUE )
  
  # Export shapefile
  rgdal::writeOGR(allcsv_df_buff, '.', paste("selpolyper",names(vegetation_poly@data)[level],"vtest",sep="_"), 'ESRI Shapefile',overwrite_layer = TRUE)
  
}

Create_Intersection = function(classes,lidarmetrics)
{
  library(raster)
  library(sp)
  library(spatialEco)
  
  classes@data$V4=as.numeric(factor(classes@data$V3))
  
  # Masking
  classes_rast <- rasterize(classes, lidarmetrics[[1]],field="V4")
  masked <- mask(lidarmetrics, classes_rast)
  
  # convert training data into the right format
  trainingbrick <- addLayer(masked, classes_rast)
  featuretable <- getValues(trainingbrick)
  featuretable <- na.omit(featuretable)
  featuretable <- as.data.frame(featuretable)
  
  return(featuretable)
}

Classification_werrorass = function(featuretable_level1,level_id,modelFit) 
{
  library(randomForest)
  library(caret)
  
  trainIndex <- caret::createDataPartition(y=featuretable_level1$layer, p=0.75, list=FALSE)
  trainingSet<- featuretable_level1[trainIndex,]
  testingSet<- featuretable_level1[-trainIndex,]
  
  prediction <- predict(modelFit,testingSet[ ,c(1:26)])
  
  conf_m=confusionMatrix(factor(prediction), factor(testingSet$layer),mode = "everything")
  
  sink(paste(level_id,"acc.txt",sep=""))
  print(conf_m)
  sink()
}

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
  
  response_l1 <- rbind(response_l1_c1, response_l1_c2, response_l1_c3, response_l1_c4)
  
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
  importance_frame$varclass[importance_frame$variable=="S_curv" | importance_frame$variable == "S_lin" | importance_frame$variable == "S_plan" | importance_frame$variable == "S_sph" | importance_frame$variable == "S_ani"] <-2
  importance_frame$varclass[importance_frame$variable == "VV_sd" | importance_frame$variable == "VV_var" | importance_frame$variable=="VV_skew" | importance_frame$variable == "VV_kurt" | importance_frame$variable == "VV_cr" | importance_frame$variable == "VV_vdr"
                            | importance_frame$variable == "VV_simp" | importance_frame$variable == "VV_shan"] <- 3
  importance_frame$varclass[importance_frame$variable=="H_max" | importance_frame$variable=="H_mean" | importance_frame$variable == "H_med" | importance_frame$variable == "H_25p" | importance_frame$variable == "H_75p"
                            | importance_frame$variable == "H_90p" ] <- 4
  importance_frame$varclass[importance_frame$variable=="HV_rough" | importance_frame$variable=="HV_tpi" | importance_frame$variable == "HV_tri" | importance_frame$variable == "HV_sd" | importance_frame$variable == "HV_var"] <- 5
  
  return(importance_frame)
  
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}