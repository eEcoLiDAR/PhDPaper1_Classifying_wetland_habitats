"
@author: Zsofia Koma, UvA
Aim: Prepare training data
"

library(raster)
library(rgdal)
library(rgeos)
source("D:/GitHub/eEcoLiDAR/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R") #set where the Function*.R file located
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")

# Set working dirctory
workingdirectory="D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/"
#workingdirectory="D:/Koma/Paper1_ReedStructure/Results_2019March/"
setwd(workingdirectory)

n=100 #number of sample

# Import
lidarmetrics_l1=stack("covermetrics_gr_5m.grd")
lidarmetrics_l23=stack("height_metrics_whgr_gr_norm_5m.grd")

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
ext=extent(lidarmetrics_l1[[1]])
vegetation <- crop(vegetation, ext)

Create_FieldTraining(vegetation,25,n)
Create_FieldTraining(vegetation,26,n)
Create_FieldTraining(vegetation,27,n)

### Create intersection

classes1 = rgdal::readOGR(paste("selpolyper_level1_vtest_",n,".shp",sep=""))
classes2 = rgdal::readOGR(paste("selpolyper_level2_vtest_",n,".shp",sep=""))
classes3 = rgdal::readOGR(paste("selpolyper_level3_vtest_",n,".shp",sep=""))

# Intersection for classification
featuretable_l1=Create_Intersection(classes1,lidarmetrics_l1)
write.table(featuretable_l1,"featuretable_level1_b2o5_test.csv",row.names=FALSE,sep=",")

featuretable_l2=Create_Intersection(classes2,lidarmetrics_l23)
write.table(featuretable_l2,"featuretable_level2_b2o5_test.csv",row.names=FALSE,sep=",")

featuretable_l3=Create_Intersection(classes3,lidarmetrics_l23)
write.table(featuretable_l3,"featuretable_level3_b2o5_test.csv",row.names=FALSE,sep=",")

# Check amount of valid training per class


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
