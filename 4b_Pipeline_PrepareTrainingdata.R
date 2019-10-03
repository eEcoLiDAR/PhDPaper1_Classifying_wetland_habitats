"
@author: Zsofia Koma, UvA
Aim: Prepare training data
"

library(raster)
library(rgdal)
library(rgeos)
library(spatialEco)
library(sf)
library(dplyr)
library(sdm)

#source("D:/GitHub/eEcoLiDAR/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R") #set where the Function*.R file located
source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R")

res=1

# Set working dirctory
workingdirectory=paste("D:/Koma/Paper1/Revision/Results/",res,"m/",sep="")
#workingdirectory="D:/Koma/Paper1_ReedStructure/Results_2019March/"
setwd(workingdirectory)

n=1000 #number of sample

# Import
lidarmetrics_l1=stack(paste("lidarmetrics_l1_masked_",res,"m.grd",sep=""))
lidarmetrics_l23=stack(paste("lidarmetrics_l23_masked_",res,"m.grd",sep=""))

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

d <- sdmData(V3~.,train=classes1,predictors = lidarmetrics_l1)