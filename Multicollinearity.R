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
workdir="D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_forreview2019Oct/"
res=5

setwd(workdir)

# Import 
lidarmetrics=stack(paste("lidarmetrics_l23_masked_",res,"m.grd",sep=""))
lidarmetrics=dropLayer(lidarmetrics,c(30))

# Raster based
v <- vifstep(lidarmetrics,th=5)
v

v <- vifcor(lidarmetrics,th=0.6,maxobservations=10000)
v

#saveRDS(v,file="vif_results_5m.rds")




