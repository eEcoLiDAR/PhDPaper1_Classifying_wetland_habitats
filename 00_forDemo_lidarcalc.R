"
@author: Zsofia Koma, UvA
Aim: This script is aimed to extract the liDAR metrics every possible way (with and without ground points, with and without homogenization etc.)
"

# Import required R packages
library("lidR")
library("rgdal")
#source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_LiDARMetricsCalc.R") #set where the Function*.R file located
source("D:/GitHub/eEcoLiDAR/PhDPaper1_Classifying_wetland_habitats/Function_LiDARMetricsCalc.R")

# Set working directory
workingdirectory="D:/Koma/for_lifawatch/" ## set this directory where your input las files are located
setwd(workingdirectory)

cores=4
chunksize=2000
buffer=1
resolution=10

rasterOptions(maxmemory = 200000000000)

### Ground run

ground_ctg <- catalog(workingdirectory)

opt_chunk_buffer(ground_ctg) <- buffer
opt_chunk_size(ground_ctg) <- chunksize
opt_cores(ground_ctg) <- cores

library(future)
plan(multisession, workers = cores)
set_lidr_threads(cores)

# Calculate metrics

pdf("Metrics_l1.pdf") 

par(mfrow=c(1,1))

covermetrics = grid_metrics(ground_ctg,  CoverageMetrics(Z,Classification), res = resolution)
plot(covermetrics)
writeRaster(covermetrics,paste("covermetrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

vertdistr_metrics = grid_metrics(ground_ctg, VertDistr_Metrics(Z),res=resolution)
plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,paste("vertdistr_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

height_metrics = grid_metrics(ground_ctg, HeightMetrics(Z),res=resolution)
plot(height_metrics)
writeRaster(height_metrics,paste("height_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

proj4string(height_metrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontal_metrics = HorizontalMetrics(height_metrics$zmax)
plot(horizontal_metrics)
writeRaster(horizontal_metrics,paste("horizontal_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

dev.off() 
