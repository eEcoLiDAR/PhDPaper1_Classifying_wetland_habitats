"
@author: Zsofia Koma, UvA
Aim: This script is aimed to extract the liDAR metrics every possible way (with and without ground points, with and without homogenization etc.)
"

# Import required R packages
library("lidR")
library("rgdal")
source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_LiDARMetricsCalc.R") #set where the Function*.R file located
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis//Paper1_inR_v2/Function_LiDARMetricsCalc.R")

# Set working directory
workingdirectory="D:/Koma/Paper1/Revision/input/process/" ## set this directory where your input las files are located
#workingdirectory="D:/Koma/Paper1/ALS/"
setwd(workingdirectory)

cores=18
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
plan(multisession, workers = 6L)
set_lidr_threads(6L)

# Calculate metrics

covermetrics = grid_metrics(ground_ctg,  CoverageMetrics(Z,Classification), res = resolution)
#plot(covermetrics)
writeRaster(covermetrics,paste("covermetrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

shapemetrics = grid_metrics(ground_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics)
writeRaster(shapemetrics,paste("shapemetrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

vertdistr_metrics = grid_metrics(ground_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,paste("vertdistr_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

height_metrics = grid_metrics(ground_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics,paste("height_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

proj4string(height_metrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontal_metrics = HorizontalMetrics(height_metrics$zmax)
#plot(horizontal_metrics)
writeRaster(horizontal_metrics,paste("horizontal_metrics_gr_",resolution,"m.grd",sep=""),overwrite=TRUE)

### Normlized-ground run

normalized_ctg <- catalog(paste(workingdirectory,"normalized_neibased/",sep=""))

opt_chunk_buffer(normalized_ctg) <- buffer
opt_chunk_size(normalized_ctg) <- chunksize
opt_cores(normalized_ctg) <- cores

covermetrics = grid_metrics(normalized_ctg,  CoverageMetrics(Z,Classification), res = resolution)
#plot(covermetrics)
writeRaster(covermetrics,paste("covermetrics_gr_norm_",resolution,"m.grd",sep=""),overwrite=TRUE)

# Only for vegetation

opt_filter(normalized_ctg) <- "-keep_class 1"

shapemetrics_whgr = grid_metrics(normalized_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics_whgr)
writeRaster(shapemetrics_whgr,paste("shapemetrics_whgr_gr_norm_",resolution,"m.grd",sep=""),overwrite=TRUE)

vertdistr_metrics_whgr = grid_metrics(normalized_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics_whgr)
writeRaster(vertdistr_metrics_whgr,paste("vertdistr_metrics_whgr_gr_norm_",resolution,"m.grd",sep=""),overwrite=TRUE)

height_metrics_whgr = grid_metrics(normalized_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics_whgr ,paste("height_metrics_whgr_gr_norm_",resolution,"m.grd",sep=""),overwrite=TRUE)

horizontal_metrics_whgr = HorizontalMetrics(height_metrics_whgr$zmax)
#plot(horizontal_metrics)
writeRaster(horizontal_metrics_whgr,paste("horizontal_metrics_whgr_gr_norm_",resolution,"m.grd"),overwrite=TRUE)

# Only dtm

opt_filter(ground_ctg) <- "-keep_class 2"
dtm = grid_metrics(ground_ctg,min(Z),res=resolution)
crs(dtm) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"

slope_dtm=terrain(dtm,opt="slope",unit="degrees",neighbors=4)
aspect_dtm=terrain(dtm,opt="aspect",unit="degrees",neighbors=4)
rough_dtm=terrain(dtm,opt="roughness",neighbors=4)

dtm_metrics=stack(slope_dtm,aspect_dtm,rough_dtm) 

writeRaster(dtm_metrics,paste("dtm_metrics",resolution,"m.grd",sep=""),overwrite=TRUE)
