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

# Input
load("modelFit_l1.RData")
load("modelFit_l2.RData")
load("modelFit_l3.RData")

# Pre-process - rename coloumns, add feature classes

lidarmetrics_forl1=stack("lidarmetrics_l1_5m.grd")
lidarmetrics_forl23=stack("lidarmetrics_l23_5m.grd")

lidarmetrics_forl1 <- subset(lidarmetrics_forl1, c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect"), drop=FALSE)
names(lidarmetrics_forl1) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp")

lidarmetrics_forl23 <- subset(lidarmetrics_forl23, c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect"), drop=FALSE)
names(lidarmetrics_forl23) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp")


### Level1 ###

# Classification

Pred_l1 <- predict(lidarmetrics_forl1, model=modelFit_l1, na.rm=TRUE)
writeRaster(Pred_l1, filename="classified_level1.tif", format="GTiff",overwrite=TRUE)

# Mask 
formask <- setValues(raster(lidarmetrics_forl23), NA)
formask[Pred_l1==2] <- 1

lidarmetrics_masked1 <- mask(lidarmetrics_forl23,formask)
#writeRaster(lidarmetrics_masked1, filename="lidarmetrics_forlevel2.grd",overwrite=TRUE)

### Level2 ###

# Classification

Pred_l2 <- predict(lidarmetrics_masked1, model=modelFit_l2, na.rm=TRUE)
writeRaster(Pred_l2, filename="classified_level2.tif", format="GTiff",overwrite=TRUE)

# Mask 
formask2 <- setValues(raster(lidarmetrics_forl23), NA)
#formask2[Pred_l2==4 | Pred_l2==5] <- 1
formask2[Pred_l2==3] <- 1

lidarmetrics_masked2 <- mask(lidarmetrics_masked1,formask2)
#writeRaster(lidarmetrics_masked2, filename="lidarmetrics_forlevel3.grd",overwrite=TRUE)

### Level3 ###

# Classification

Pred_l3 <- predict(lidarmetrics_masked2, model=modelFit_l3, na.rm=TRUE)
writeRaster(Pred_l3, filename="classified_level3.tif", format="GTiff",overwrite=TRUE)