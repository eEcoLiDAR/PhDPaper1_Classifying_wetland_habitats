"
@author: Zsofia Koma, UvA
Aim: Prepare training data
"

library(raster)
library(rgdal)
library(rgeos)
source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R") #set where the Function*.R file located
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")

# Set working dirctory
workingdirectory="D:/Koma/Paper1_v2/Run4_2019April/"
#workingdirectory="D:/Koma/Paper1_ReedStructure/Results_2019March/"
setwd(workingdirectory)

# Import
lidarmetrics_l1=stack("lidarmetrics_l1_masked.grd")
lidarmetrics_l23=stack("lidarmetrics_l2l3_masked_wgr.grd")

vegetation=readOGR(dsn="vlakken_union_structuur.shp")

### Create the defined classes

# Level 1
vegetation@data$level1=NA

vegetation@data$level1[vegetation@data$StructDef=='K' | vegetation@data$StructDef=='P' | vegetation@data$StructDef=='Gl' | vegetation@data$StructDef=='A']="O"
vegetation@data$level1[vegetation@data$StructDef=='Rkd' | vegetation@data$StructDef=='Rko' | vegetation@data$StructDef=='Rld'
                    | vegetation@data$StructDef=='Rlo' | vegetation@data$StructDef=='Rwd' | vegetation@data$StructDef=='Rwo'
                    | vegetation@data$StructDef=='U' | vegetation@data$StructDef=='Gh'
                    | vegetation@data$StructDef=='Slo' | vegetation@data$StructDef=='Sld'
                    | vegetation@data$StructDef=='Smo' | vegetation@data$StructDef=='Smd' | vegetation@data$StructDef=='Sho' | vegetation@data$StructDef=='Shd'
                    | vegetation@data$StructDef=='Bo' | vegetation@data$StructDef=='Bd']="V"

sort(unique(vegetation@data$level1))

# Level 2
vegetation@data$level2=NA

vegetation@data$level2[vegetation@data$StructDef=='Rkd' | vegetation@data$StructDef=='Rld' | vegetation@data$StructDef=='Rwd']="R"
#vegetation@data$level2[vegetation@data$StructDef=='Rwd']="Rw"
vegetation@data$level2[vegetation@data$StructDef=='Gh']="G"
vegetation@data$level2[vegetation@data$StructDef=='Sld'| vegetation@data$StructDef=='Smd'| vegetation@data$StructDef=='Shd'] = "S"
vegetation@data$level2[vegetation@data$StructDef=='Bd']="B"

sort(unique(vegetation@data$level2))

# Level 3
vegetation@data$level3=NA

vegetation@data$level3[vegetation@data$StructDef=='Rkd']="Rk"
vegetation@data$level3[vegetation@data$StructDef=='Rld']="Rl"
vegetation@data$level3[vegetation@data$StructDef=='Rwd']="Rw"

sort(unique(vegetation@data$level3))

# Sampling polygons randomly
ext=extent(lidarmetrics_l1[[25]])
vegetation <- crop(vegetation, ext)

Create_FieldTraining(vegetation,25)
Create_FieldTraining(vegetation,26)
Create_FieldTraining(vegetation,27)

### Create intersection

classes1 = rgdal::readOGR("selpolyper_level1_vtest.shp")
classes2 = rgdal::readOGR("selpolyper_level2_vtest.shp")
classes3 = rgdal::readOGR("selpolyper_level3_vtest.shp")

# Intersection for classification
featuretable_l1=Create_Intersection(classes1,lidarmetrics_l1)
write.table(featuretable_l1,"featuretable_level1_b2o5_test.csv",row.names=FALSE,sep=",")

featuretable_l2=Create_Intersection(classes2,lidarmetrics_l23)
write.table(featuretable_l2,"featuretable_level2_b2o5_test.csv",row.names=FALSE,sep=",")

featuretable_l3=Create_Intersection(classes3,lidarmetrics_l23)
write.table(featuretable_l3,"featuretable_level3_b2o5_test.csv",row.names=FALSE,sep=",")

# Intersection for feature analysis gr and wgr

# Create level 1 same extent as level23
formask <- setValues(raster(lidarmetrics_l1[[21]]), 1)
formask[is.na(lidarmetrics_l23[[21]])] <- NA

lidarmetrics_masked_wl12 <- mask(lidarmetrics_l1, formask)
writeRaster(lidarmetrics_masked_wl12,"lidarmetrics_l1_masked_wl12.grd",overwrite=TRUE)

# Intersection
ex_m=extent(lidarmetrics_l23)

lidarmetrics_masked_wl12_m=crop(lidarmetrics_masked_wl12,ex_m)

foranal=stack(lidarmetrics_l23,lidarmetrics_masked_wl12_m)

featuretable_fea_anal=Create_Intersection(classes1,foranal)
write.table(featuretable_fea_anal,"featuretable_b2o5_wgr_whgr.csv",row.names=FALSE,sep=",")
