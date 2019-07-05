"
@author: Zsofia Koma, UvA
Aim: This script is aimed the pre-process AHN2 data (tile, extract ground points, create DTM and apply point neighborhood based normalization)
"

# Import required R packages
library("lidR")
library("rgdal")

# Set working directory
workingdirectory="D:/Koma/Paper1_v2/ALS/" ## set this directory where your input las files are located
#workingdirectory="D:/Koma/Paper1/ALS/"
setwd(workingdirectory)

cores=18
chunksize=2000
buffer=2.5
resolution=2.5

rasterOptions(maxmemory = 200000000000)

# Create catalog 
ctg <- catalog(workingdirectory)

# Classify ground

opt_chunk_buffer(ctg) <- buffer
opt_chunk_size(ctg) <- chunksize
opt_cores(ctg) <- cores
opt_output_files(ctg) <- paste(workingdirectory,"ground/{XLEFT}_{YBOTTOM}_gr",sep="")

ground_ctg <- lasground(ctg, pmf(2.5,0.1))

# Normalize with point neighborhood

opt_chunk_buffer(ground_ctg) <- buffer
opt_chunk_size(ground_ctg) <- chunksize
opt_cores(ground_ctg) <- cores
opt_output_files(ground_ctg) <- paste(workingdirectory,"normalized_neibased/{XLEFT}_{YBOTTOM}_gr_norm",sep="")

normalized_ctg=lasnormalize(ground_ctg,knnidw(k=20,p=2))

# Generate DTM

opt_output_files(ground_ctg)=""

dtm = grid_terrain(ground_ctg, algorithm =knnidw(k=20,p=2), res=2.5, keep_lowest = TRUE)
crs(dtm) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
writeRaster(dtm, "dtm.tif",overwrite=TRUE)

# Hillshade

slope <- terrain(dtm, opt='slope')
aspect <- terrain(dtm, opt='aspect')
dtm_shd <- hillShade(slope, aspect, 40, 270)

writeRaster(dtm_shd, "dtm_shd.tif",overwrite=TRUE)

# Create DSM

dsm = grid_metrics(ground_ctg,max(Z),res=2.5)
crs(dsm) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
writeRaster(dsm, "dsm.tif",overwrite=TRUE)

# Hillshade

slope <- terrain(dsm, opt='slope')
aspect <- terrain(dsm, opt='aspect')
dsm_shd <- hillShade(slope, aspect, 40, 270)

writeRaster(dsm_shd, "dsm_shd.tif",overwrite=TRUE)

# Homogenization
normalized_ctg <- catalog(paste(workingdirectory,"normalized_neibased/",sep=""))
opt_output_files(normalized_ctg) <- paste(workingdirectory,"homogenized/{XLEFT}_{YBOTTOM}_gr_norm_homo",sep="")

homogenized_ctg=lasfilterdecimate(normalized_ctg,homogenize(12,1))

# Homogenization without normalization
normalized_ctg2 <- catalog(paste(workingdirectory,"ground/",sep=""))
opt_output_files(normalized_ctg2) <- paste(workingdirectory,"ground_homogenized/{XLEFT}_{YBOTTOM}_gr_homo",sep="")

homogenized_ctg2=lasfilterdecimate(normalized_ctg2,homogenize(12,1))