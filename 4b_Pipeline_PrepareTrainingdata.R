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

res=5

# Set working dirctory
workingdirectory=paste("D:/Koma/Paper1/Revision/Results/",res,"m/",sep="")
#workingdirectory="D:/Koma/Paper1_ReedStructure/Results_2019March/"
setwd(workingdirectory)

n=500 #number of sample

# Import
lidarmetrics_l1=stack(paste("lidarmetrics_l1_masked_",res,"m.grd",sep=""))
lidarmetrics_l23=stack(paste("lidarmetrics_l23_masked_",res,"m.grd",sep=""))

lidarmetrics_l23[is.na(lidarmetrics_l23)]<-0

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

vegetation@data$level3[vegetation@data$StructDef=='Rkd']="R"
vegetation@data$level3[vegetation@data$StructDef=='Rld']="P"
vegetation@data$level3[vegetation@data$StructDef=='Rwd']="W"

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

classes1.df <- as(classes1, "data.frame")
classes1.df$id <- seq(1,length(classes1.df$V3))
coordinates(classes1.df)=~coords.x1+coords.x2
proj4string(classes1.df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

d1 <- sdmData(V3~.,train=classes1.df,predictors = lidarmetrics_l23)
data=d1@features
classes1.df2 <- as(classes1.df, "data.frame")

datamerge_l1=merge(x = data, y = classes1.df2, by = c("id"), all.x = TRUE)
datamerge_l1=subset(datamerge_l1, select=c(3:37))
datamerge_l1=na.omit(datamerge_l1)
write.table(datamerge_l1,paste("featuretable_level1_",n,"_",res,".csv",sep=""),row.names=FALSE,sep=",")

classes2.df <- as(classes2, "data.frame")
classes2.df$id <- seq(1,length(classes2.df$V3))
coordinates(classes2.df)=~coords.x1+coords.x2
proj4string(classes2.df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

d2 <- sdmData(V3~.,train=classes2.df,predictors = lidarmetrics_l23)
data2=d2@features
classes2.df2 <- as(classes2.df, "data.frame")

datamerge_l2=merge(x = data2, y = classes2.df2, by = c("id"), all.x = TRUE)
datamerge_l2=subset(datamerge_l2, select=c(3:37))
datamerge_l2=na.omit(datamerge_l2)
write.table(datamerge_l2,paste("featuretable_level2_",n,"_",res,".csv",sep=""),row.names=FALSE,sep=",")

classes3.df <- as(classes3, "data.frame")
classes3.df$id <- seq(1,length(classes3.df$V3))
coordinates(classes3.df)=~coords.x1+coords.x2
proj4string(classes3.df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

d3 <- sdmData(V3~.,train=classes3.df,predictors = lidarmetrics_l23)
data3=d3@features
classes3.df2 <- as(classes3.df, "data.frame")

datamerge_l3=merge(x = data2, y = classes3.df2, by = c("id"), all.x = TRUE)
datamerge_l3=subset(datamerge_l3, select=c(3:37))
datamerge_l3=na.omit(datamerge_l3) 
write.table(datamerge_l3,paste("featuretable_level3_",n,"_",res,".csv",sep=""),row.names=FALSE,sep=",")

# how many?
l1_count <- datamerge_l1 %>%
  group_by(V3) %>%
  summarise(nofobs = length(V3))

print(l1_count)

l2_count <- datamerge_l2 %>%
  group_by(V3) %>%
  summarise(nofobs = length(V3))

print(l2_count)

l3_count <- datamerge_l3 %>%
  group_by(V3) %>%
  summarise(nofobs = length(V3))

print(l3_count)

