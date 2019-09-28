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
  writeLAS(laz,paste(workingdirectory,"/input/","ngr_",laz_files_names2[i],sep=""))
  
}

laz1=readLAS(paste(workingdirectory,"/input/","ngr_u02gz2.laz",sep=""))
dtm=raster(paste(workingdirectory,"/input/dtm/","ahn2_5_02gz2.tif",sep=""))

laz_norm=lasnormalize(laz1, dtm, na.rm = TRUE)
laz_whw=lasunnormalize(laz_norm)

writeLAS(laz_norm,paste(workingdirectory,"/input/","norm_u02gz2.laz",sep=""))
writeLAS(laz_whw,paste(workingdirectory,"/input/","nowater_u02gz2.laz",sep=""))

#
laz1=readLAS(paste(workingdirectory,"/input/","ngr_u02hz1.laz",sep=""))
dtm=raster(paste(workingdirectory,"/input/dtm/","ahn2_5_02hz1.tif",sep=""))

laz_norm=lasnormalize(laz1, dtm, na.rm = TRUE)
laz_whw=lasunnormalize(laz_norm)

writeLAS(laz_norm,paste(workingdirectory,"/input/","norm_u02hz1.laz",sep=""))
writeLAS(laz_whw,paste(workingdirectory,"/input/","nowater_u02hz1.laz",sep=""))

#
laz1=readLAS(paste(workingdirectory,"/input/","ngr_u06fn1.laz",sep=""))
dtm=raster(paste(workingdirectory,"/input/dtm/","ahn2_5_06fn1.tif",sep=""))

laz_norm=lasnormalize(laz1, dtm, na.rm = TRUE)
laz_whw=lasunnormalize(laz_norm)

writeLAS(laz_norm,paste(workingdirectory,"/input/","norm_u06fn1.laz",sep=""))
writeLAS(laz_whw,paste(workingdirectory,"/input/","nowater_u06fn1.laz",sep=""))

#
laz1=readLAS(paste(workingdirectory,"/input/","ngr_u06en2.laz",sep=""))
dtm=raster(paste(workingdirectory,"/input/dtm/","ahn2_5_06en2.tif",sep=""))

laz_norm=lasnormalize(laz1, dtm, na.rm = TRUE)
laz_whw=lasunnormalize(laz_norm)

writeLAS(laz_norm,paste(workingdirectory,"/input/","norm_u06en2.laz",sep=""))
writeLAS(laz_whw,paste(workingdirectory,"/input/","nowater_u06en2.laz",sep=""))