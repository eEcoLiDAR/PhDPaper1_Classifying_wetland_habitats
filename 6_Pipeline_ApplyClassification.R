"
@author: Zsofia Koma, UvA
Aim: Classify the data
"
library(caret)
library(randomForest)
library(raster)
library(rgdal)
source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") #set where the Function*.R file located

# Set working dirctory
workingdirectory="D:/Koma/Paper1_v2/Results_17April/"
setwd(workingdirectory)

#Skipped: selection of training data from polygon + buffer and recategorization

# Input

lidarmetrics_forl1=stack("lidarmetrics_l1_masked.grd")
names(lidarmetrics_forl1) <- c("C_puls","C_can","S_curv","S_lin","S_plan","S_sph","S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                               "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p")
lidarmetrics_forl23=stack("lidarmetrics_l2l3_masked_wgr.grd")
names(lidarmetrics_forl23) <- c("C_puls","C_can","S_curv","S_lin","S_plan","S_sph","S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                               "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p")

featuretable_l1=read.csv("featuretable_level1_b2o5.csv")
featuretable_l2=read.csv("featuretable_level2_b2o5.csv")
featuretable_l3=read.csv("featuretable_level3_b2o5.csv")

names(featuretable_l1) <- c("C_puls","C_can","S_curv","S_lin","S_plan","S_sph","S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

names(featuretable_l2) <- c("C_puls","C_can","S_curv","S_lin","S_plan","S_sph","S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

names(featuretable_l3) <- c("C_puls","C_can","S_curv","S_lin","S_plan","S_sph","S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

### Level1 ###

# Classification

lidarmetrics_forl1_sel=lidarmetrics_forl1[[c("HV_var","HV_sd","HV_rough","S_plan","C_can")]]
featuretable_l1_sel=featuretable_l1[,c("HV_var","HV_sd","HV_rough","S_plan","C_can","layer")]

modelFit_1 <- randomForest(factor(layer)~.,data=featuretable_l1_sel,ntree=100)

Pred_l1 <- predict(lidarmetrics_forl1_sel, model=modelFit_1, na.rm=TRUE)
writeRaster(Pred_l1, filename="classified_level1.tif", format="GTiff",overwrite=TRUE)

# Mask 
formask <- setValues(raster(lidarmetrics_forl23), NA)
formask[Pred_l1==2] <- 1

lidarmetrics_masked1 <- mask(lidarmetrics_forl23,formask)
#writeRaster(lidarmetrics_masked1, filename="lidarmetrics_forlevel2.grd",overwrite=TRUE)

### Level2 ###

# Classification

lidarmetrics_masked1_sel=lidarmetrics_masked1[[c("H_max","H_75p","H_90p","HV_rough","HV_sd","VV_shan","H_mean","HV_var","VV_sd","VV_var")]]
featuretable_l2_sel=featuretable_l2[,c("H_max","H_75p","H_90p","HV_rough","HV_sd","VV_shan","H_mean","HV_var","VV_sd","VV_var","layer")]

modelFit_2 <- randomForest(factor(layer)~.,data=featuretable_l2_sel,ntree=100)

Pred_l2 <- predict(lidarmetrics_masked1_sel, model=modelFit_2, na.rm=TRUE)
writeRaster(Pred_l2, filename="classified_level2.tif", format="GTiff",overwrite=TRUE)

# Mask 
formask2 <- setValues(raster(lidarmetrics_forl23), NA)
#formask2[Pred_l2==4 | Pred_l2==5] <- 1
formask2[Pred_l2==3] <- 1

lidarmetrics_masked2 <- mask(lidarmetrics_masked1,formask2)
#writeRaster(lidarmetrics_masked2, filename="lidarmetrics_forlevel3.grd",overwrite=TRUE)

### Level3 ###

# Classification

lidarmetrics_masked2_sel=lidarmetrics_masked2[[c("H_90p","H_max","C_puls","HV_sd","C_can","HV_var","H_25p","H_75p","HV_rough","H_med")]]
featuretable_l3_sel=featuretable_l3[,c("H_90p","H_max","C_puls","HV_sd","C_can","HV_var","H_25p","H_75p","HV_rough","H_med","layer")]

modelFit_3 <- randomForest(factor(layer)~.,data=featuretable_l3_sel,ntree=100)

Pred_l3 <- predict(lidarmetrics_masked2_sel, model=modelFit_3, na.rm=TRUE)
writeRaster(Pred_l3, filename="classified_level3.tif", format="GTiff",overwrite=TRUE)