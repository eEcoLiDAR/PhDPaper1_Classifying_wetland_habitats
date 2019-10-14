"
@author: Zsofia Koma, UvA
Aim: Analyse multicollinearity
"

library(randomForest)
library(caret)
library(rfUtilities)

library(ggplot2)
library(gridExtra)
library(ggrepel)

library(reshape2)
library(corrplot)

library(rgdal)
library(usdm)
library(raster) 
library(ENMTools)

library(corrplot)

# Set global variables
workdir="D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/"
res=5

setwd(paste(workdir,res,"m/",sep=""))

# Import 
lidarmetrics=stack(paste("lidarmetrics_l23_masked_",res,"m.grd",sep=""))
lidarmetrics=dropLayer(lidarmetrics,c(30))

# Raster based
v <- vifstep(lidarmetrics,th=5)
v

v <- vifcor(lidarmetrics,th=0.7,maxobservations=10000)
v

saveRDS(v,file="vif_results_5m.rds")
