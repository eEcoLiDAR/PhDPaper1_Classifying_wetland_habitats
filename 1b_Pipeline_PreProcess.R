"
@author: Zsofia Koma, UvA
Aim: This script is aimed the pre-process AHN2 data (tile, extract ground points, create DTM and apply point neighborhood based normalization)
"

# Import required R packages
library("lidR")
library("rgdal")

# Set working directory
#workingdirectory="D:/Koma/Paper1_revision/" ## set this directory where your input las files are located
workingdirectory="D:/Koma/Paper1/Revision/"
setwd(workingdirectory)

laz_files_names <- dir(workingdirectory,pattern="^[g]")

for (i in 1:length(laz_files_names)) {
  
  laz=readLAS(paste(workingdirectory,laz_files_names[i],sep=""))
  laz$Classification <- 2L
  writeLAS(laz,paste(workingdirectory,"/input/","gr_",laz_files_names[i],sep=""))
  
}

laz_files_names2 <- dir(workingdirectory,pattern="^[u]")

for (i in 1:length(laz_files_names2)) {
  
  laz=readLAS(paste(workingdirectory,laz_files_names2[i],sep=""))
  laz$Classification <- 1L
  writeLAS(laz,paste(workingdirectory,"/input/","ngr_",laz_files_names[i],sep=""))
  
}

cores=18
chunksize=2000
buffer=1

# Create catalog 
ctg <- catalog(paste(workingdirectory,"/input/",sep=""))

# Normalize with point neighborhood

opt_chunk_buffer(ctg) <- buffer
opt_chunk_size(ctg) <- chunksize
opt_cores(ctg) <- cores
opt_output_files(ctg) <- paste(workingdirectory,"normalized_neibased/{XLEFT}_{YBOTTOM}_gr_norm",sep="")

normalized_ctg=lasnormalize(ctg,knnidw(k=20,p=2))