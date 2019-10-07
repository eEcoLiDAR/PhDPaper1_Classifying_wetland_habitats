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
workdir="D:/Koma/Paper1/Revision/Results/"
res=2.5

setwd(paste(workdir,res,"m/",sep=""))

# Import 
lidarmetrics=stack(paste("lidarmetrics_l23_masked_",res,"m.grd",sep=""))
lidarmetrics_table=read.csv(paste("featuretable_level2_3000_",res,".csv",sep=""))

lidarmetrics_table=lidarmetrics_table[,c(1:32)] 

# Raster based
v <- vifstep(lidarmetrics,th=5)
v

# spearson correlations for raster
corrmatrix=raster.cor.matrix(lidarmetrics, method = "spearman")
corrmatrix_plot=raster.cor.plot(lidarmetrics, method = "spearman")

# Training database
v <- vifstep(lidarmetrics_table,th=5)
v

correlationMatrix <- cor(lidarmetrics_table,lidarmetrics_table,method="spearman")

col <- colorRampPalette(c("#77AADD", "#4477AA", "#FFFFFF", "#EE9988","#BB4444"))

corrplot(correlationMatrix, method="color", col=col(200),order = "hclust",  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, font=30, #Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
