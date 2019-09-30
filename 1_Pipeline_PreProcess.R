"
@author: Zsofia Koma, UvA
Aim: This script is aimed the pre-process AHN2 data (tile, extract ground points, create DTM and apply point neighborhood based normalization)
"

# Import required R packages
library("lidR")
library("rgdal")

# Set working directory
workingdirectory="D:/Koma/Paper1/Revision/input/process/" ## set this directory where your input las files are located
#workingdirectory="D:/Koma/Paper1/ALS/"
setwd(workingdirectory)

cores=18
chunksize=2000
buffer=1
resolution=1

rasterOptions(maxmemory = 200000000000)

library(future)
plan(multisession, workers = 4L)
set_lidr_threads(4L)

# Create catalog 
ctg <- catalog(workingdirectory)

# Normalize with point neighborhood

opt_chunk_buffer(ctg) <- buffer
opt_chunk_size(ctg) <- chunksize
opt_cores(ctg) <- cores
opt_output_files(ctg) <- paste(workingdirectory,"normalized_neibased/{XLEFT}_{YBOTTOM}_gr_norm",sep="")

normalized_ctg=lasnormalize(ctg,knnidw(k=20,p=2))

# Generate DTM

opt_chunk_size(ctg) <- chunksize
opt_output_files(ctg)=""

dtm = grid_terrain(ctg, algorithm =knnidw(k=20,p=2), res=1, keep_lowest = TRUE)
crs(dtm) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
writeRaster(dtm, "dtm_1.tif",overwrite=TRUE)

# Hillshade

slope <- terrain(dtm, opt='slope')
aspect <- terrain(dtm, opt='aspect')
dtm_shd <- hillShade(slope, aspect, 40, 270)

writeRaster(dtm_shd, "dtm_shd_1.tif",overwrite=TRUE)