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
workingdirectory="D:/Koma/Paper1/Revision/Results/5m/"
setwd(workingdirectory)

#Skipped: selection of training data from polygon + buffer and recategorization

# Input
featuretable_l1=read.csv("featuretable_l1_5_500.csv")
featuretable_l2=read.csv("featuretable_l2_5_500.csv")
featuretable_l3=read.csv("featuretable_l3_5_500.csv")

featuretable_l1=featuretable_l1[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l2=featuretable_l2[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l3=featuretable_l3[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]

# Pre-process - rename coloumns, add feature classes

names(featuretable_l1) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l2) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l3) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

lidarmetrics_forl1=stack("lidarmetrics_l1_5m.grd")
lidarmetrics_forl23=stack("lidarmetrics_l23_5m.grd")

lidarmetrics_forl1 <- subset(lidarmetrics_forl1, c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect"), drop=FALSE)
names(lidarmetrics_forl1) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp")

lidarmetrics_forl23 <- subset(lidarmetrics_forl23, c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect"), drop=FALSE)
names(lidarmetrics_forl23) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp")


### Level1 ###

# Classification

lidarmetrics_forl1_sel=lidarmetrics_forl1[[c("VV_vdr","VV_var")]]
featuretable_l1_sel=featuretable_l1[,c("VV_vdr","VV_var","V3")]

modelFit_1 <- randomForest(factor(V3)~.,data=featuretable_l1_sel,ntree=100)

Pred_l1 <- predict(lidarmetrics_forl1, model=modelFit_1, na.rm=TRUE)
writeRaster(Pred_l1, filename="classified_level1.tif", format="GTiff",overwrite=TRUE)

# Mask 
formask <- setValues(raster(lidarmetrics_forl23), NA)
formask[Pred_l1==2] <- 1

lidarmetrics_masked1 <- mask(lidarmetrics_forl23,formask)
#writeRaster(lidarmetrics_masked1, filename="lidarmetrics_forlevel2.grd",overwrite=TRUE)

### Level2 ###

# Classification
lidarmetrics_masked1_sel=lidarmetrics_masked1[[c("VV_var","H_25p","C_can","HV_var","C_b2","C_2.5","VV_vdr","S_lin","VV_coefvar","VV_skew","HV_tpi","VV_kurt")]]
featuretable_l2_sel=featuretable_l2[,c("VV_var","H_25p","C_can","HV_var","C_b2","C_2.5","VV_vdr","S_lin","VV_coefvar","VV_skew","HV_tpi","VV_kurt","V3")]

modelFit_2 <- randomForest(factor(V3)~.,data=featuretable_l2_sel,ntree=100)

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

lidarmetrics_masked2_sel=lidarmetrics_masked2[[c("VV_var","H_25p","C_can","HV_var","C_b2")]]
featuretable_l3_sel=featuretable_l3[,c("VV_var","H_25p","C_can","HV_var","C_b2","V3")]

modelFit_3 <- randomForest(factor(V3)~.,data=featuretable_l3_sel,ntree=100)

Pred_l3 <- predict(lidarmetrics_masked2_sel, model=modelFit_3, na.rm=TRUE)
writeRaster(Pred_l3, filename="classified_level3.tif", format="GTiff",overwrite=TRUE)