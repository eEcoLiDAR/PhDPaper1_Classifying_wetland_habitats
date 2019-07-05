"
@author: Zsofia Koma, UvA
Aim: This script is aimed to extract the liDAR metrics every possible way (with and without ground points, with and without homogenization etc.)
"

# Import required R packages
library("lidR")
library("rgdal")
source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_LiDARMetricsCalc.R") #set where the Function*.R file located
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis//Paper1_inR_v2/Function_LiDARMetricsCalc.R")

# Set working directory
workingdirectory="D:/Koma/Paper1_v2/ALS/" ## set this directory where your input las files are located
#workingdirectory="D:/Koma/Paper1/ALS/"
setwd(workingdirectory)

cores=18
chunksize=2000
buffer=2.5
resolution=2.5

rasterOptions(maxmemory = 200000000000)

### Ground run

ground_ctg <- catalog(paste(workingdirectory,"ground/",sep=""))

opt_chunk_buffer(ground_ctg) <- buffer
opt_chunk_size(ground_ctg) <- chunksize
opt_cores(ground_ctg) <- cores

# Calculate metrics

covermetrics = grid_metrics(ground_ctg,  CoverageMetrics(Z,Classification), res = resolution)
#plot(covermetrics)
writeRaster(covermetrics,"covermetrics_gr.grd",overwrite=TRUE)

shapemetrics = grid_metrics(ground_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics)
writeRaster(shapemetrics,"shapemetrics_gr.grd",overwrite=TRUE)

vertdistr_metrics = grid_metrics(ground_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,"vertdistr_metrics_gr.grd",overwrite=TRUE)

height_metrics = grid_metrics(ground_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics,"height_metrics_gr.grd",overwrite=TRUE)

proj4string(height_metrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontal_metrics = HorizontalMetrics(height_metrics$zmax)
#plot(horizontal_metrics)
writeRaster(horizontal_metrics,"horizontal_metrics_gr.grd",overwrite=TRUE)

# Only for vegetation

opt_filter(ground_ctg) <- "-keep_class 1"

shapemetrics_whgr = grid_metrics(ground_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics_whgr)
writeRaster(shapemetrics_whgr,"shapemetrics_whgr_gr.grd",overwrite=TRUE)

vertdistr_metrics_whgr = grid_metrics(ground_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics_whgr)
writeRaster(vertdistr_metrics_whgr,"vertdistr_metrics_whgr_gr.grd",overwrite=TRUE)

height_metrics_whgr = grid_metrics(ground_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics_whgr ,"height_metrics_whgr_gr.grd",overwrite=TRUE)

### Normlized-ground run

normalized_ctg <- catalog(paste(workingdirectory,"normalized_neibased/",sep=""))

opt_chunk_buffer(normalized_ctg) <- buffer
opt_chunk_size(normalized_ctg) <- chunksize
opt_cores(normalized_ctg) <- cores

covermetrics = grid_metrics(normalized_ctg,  CoverageMetrics(Z,Classification), res = resolution)
#plot(covermetrics)
writeRaster(covermetrics,"covermetrics_gr_norm.grd",overwrite=TRUE)

shapemetrics = grid_metrics(normalized_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics)
writeRaster(shapemetrics,"shapemetrics_gr_norm.grd",overwrite=TRUE)

vertdistr_metrics = grid_metrics(normalized_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,"vertdistr_metrics_gr_norm.grd",overwrite=TRUE)

height_metrics = grid_metrics(normalized_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics,"height_metrics_gr_norm.grd",overwrite=TRUE)

horizontal_metrics = HorizontalMetrics(height_metrics$zmax)
#plot(horizontal_metrics)
writeRaster(horizontal_metrics,"horizontal_metrics_gr_norm.grd",overwrite=TRUE)

# Only for vegetation

opt_filter(normalized_ctg) <- "-keep_class 1"

shapemetrics_whgr = grid_metrics(normalized_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics_whgr)
writeRaster(shapemetrics_whgr,"shapemetrics_whgr_gr_norm.grd",overwrite=TRUE)

vertdistr_metrics_whgr = grid_metrics(normalized_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics_whgr)
writeRaster(vertdistr_metrics_whgr,"vertdistr_metrics_whgr_gr_norm.grd",overwrite=TRUE)

height_metrics_whgr = grid_metrics(normalized_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics_whgr ,"height_metrics_whgr_gr_norm.grd",overwrite=TRUE)

horizontal_metrics_whgr = HorizontalMetrics(height_metrics_whgr$zmax)
#plot(horizontal_metrics)
writeRaster(horizontal_metrics_whgr,"horizontal_metrics_whgr_gr_norm.grd",overwrite=TRUE)

### Homogenized-normlized-ground run

homo_normalized_ctg <- catalog(paste(workingdirectory,"homogenized/",sep=""))

opt_chunk_buffer(homo_normalized_ctg) <- buffer
opt_chunk_size(homo_normalized_ctg) <- chunksize
opt_cores(homo_normalized_ctg) <- cores

shapemetrics = grid_metrics(homo_normalized_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics)
writeRaster(shapemetrics,"shapemetrics_gr_norm_homo.grd",overwrite=TRUE)

vertdistr_metrics = grid_metrics(homo_normalized_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,"vertdistr_metrics_gr_norm_homo.grd",overwrite=TRUE)

height_metrics = grid_metrics(homo_normalized_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics,"height_metrics_gr_norm_homo.grd",overwrite=TRUE)

# Only for vegetation

opt_filter(homo_normalized_ctg) <- "-keep_class 1"

shapemetrics_whgr = grid_metrics(homo_normalized_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics_whgr)
writeRaster(shapemetrics_whgr,"shapemetrics_whgr_gr_norm_homo.grd",overwrite=TRUE)

vertdistr_metrics_whgr = grid_metrics(homo_normalized_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics_whgr)
writeRaster(vertdistr_metrics_whgr,"vertdistr_metrics_whgr_gr_norm_homo.grd",overwrite=TRUE)

height_metrics_whgr = grid_metrics(homo_normalized_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics_whgr ,"height_metrics_whgr_gr_norm_homo.grd",overwrite=TRUE)

### Homogenized-ground run

homo_ctg <- catalog(paste(workingdirectory,"ground_homogenized/",sep=""))

opt_chunk_buffer(homo_ctg) <- buffer
opt_chunk_size(homo_ctg) <- chunksize
opt_cores(homo_ctg) <- cores

shapemetrics = grid_metrics(homo_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics)
writeRaster(shapemetrics,"shapemetrics_gr_homo.grd",overwrite=TRUE)

vertdistr_metrics = grid_metrics(homo_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics)
writeRaster(vertdistr_metrics,"vertdistr_metrics_gr_homo.grd",overwrite=TRUE)

height_metrics = grid_metrics(homo_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics,"height_metrics_gr_homo.grd",overwrite=TRUE)

# Only for vegetation

opt_filter(homo_ctg) <- "-keep_class 1"

shapemetrics_whgr = grid_metrics(homo_ctg,  EigenMetrics(X,Y,Z), res = resolution)
#plot(shapemetrics_whgr)
writeRaster(shapemetrics_whgr,"shapemetrics_whgr_gr_homo.grd",overwrite=TRUE)

vertdistr_metrics_whgr = grid_metrics(homo_ctg, VertDistr_Metrics(Z),res=resolution)
#plot(vertdistr_metrics_whgr)
writeRaster(vertdistr_metrics_whgr,"vertdistr_metrics_whgr_gr_homo.grd",overwrite=TRUE)

height_metrics_whgr = grid_metrics(homo_ctg, HeightMetrics(Z),res=resolution)
#plot(height_metrics)
writeRaster(height_metrics_whgr ,"height_metrics_whgr_gr_homo.grd",overwrite=TRUE)

