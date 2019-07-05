"
@author: Zsofia Koma, UvA
Aim: This script is aimed to organize the derived metrics into one single lidarmetrics file
"

# Import required R packages
library("sp")
library("rgdal")
library("raster")
library("spatialEco")
library("rgeos")
library("plyr")
library("dplyr")
library("gridExtra")

library("ggplot2")


# Set working directory
workingdirectory="D:/Koma/Paper1_v2/Run1_2019March/LiDAR_metrics/" ## set this directory where your input las files are located
#workingdirectory="C:/Koma/Paper1/Paper1_DataProcess/"
setwd(workingdirectory)

## Level 1
# Read the separate lidarmetrics files into memory

covermetrics=stack("covermetrics_gr_norm.grd")
proj4string(covermetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

heightmetrics=stack("height_metrics_gr_norm.grd")
proj4string(heightmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontalmetrics=stack("horizontal_metrics_gr_norm.grd")
proj4string(horizontalmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

shapemetrics=stack("shapemetrics_gr_norm.grd")
proj4string(shapemetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

vertdistrmetrics=stack("vertdistr_metrics_gr_norm.grd")
proj4string(vertdistrmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

buildmask=stack("building_formask.grd")
proj4string(buildmask)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Merge files into one lidarmetrics file
ex_m=extent(shapemetrics)

covermetrics_m=crop(covermetrics,ex_m)
heightmetrics_mod_m=crop(heightmetrics,ex_m)
horizontalmetrics_mod_m=crop(horizontalmetrics,ex_m)
vertdistrmetrics_mod_m=crop(vertdistrmetrics,ex_m)

vertdistrmetrics_mod_m$zskew[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$zkurto[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$canrelrat[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$vertdenrat[vertdistrmetrics_mod_m$zstd==0]<-0

lidarmetrics=stack(covermetrics_m,shapemetrics,vertdistrmetrics_mod_m,horizontalmetrics_mod_m,heightmetrics_mod_m)
#writeRaster(lidarmetrics,"lidarmetrics.grd",overwrite=TRUE)

# Exclude buildings

#buildings=readOGR(dsn="buldings.shp")

#building_rast <- rasterize(buildings, lidarmetrics[[1]],field="DN")
formask <- setValues(raster(lidarmetrics[[1]]), 1)
formask[is.na(buildmask)] <- NA

#writeRaster(formask,"building_rast.grd",overwrite=TRUE)

lidarmetrics_masked <- mask(lidarmetrics, formask)

lidarmetrics_l1=dropLayer(lidarmetrics_masked,c(3,4,29,30))

writeRaster(lidarmetrics_l1,"lidarmetrics_l1_masked.grd",overwrite=TRUE)

# Read the separate lidarmetrics files into memory (excluding ground points)

covermetrics_wgr=stack("covermetrics_gr_norm.grd")
proj4string(covermetrics_wgr)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

heightmetrics_wgr=stack("height_metrics_whgr_gr_norm.grd")
proj4string(heightmetrics_wgr)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontalmetrics_wgr=stack("horizontal_metrics_whgr_gr_norm.grd")
proj4string(horizontalmetrics_wgr)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

shapemetrics_wgr=stack("shapemetrics_whgr_gr_norm.grd")
proj4string(shapemetrics_wgr)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

vertdistrmetrics_wgr=stack("vertdistr_metrics_whgr_gr_norm.grd")
proj4string(vertdistrmetrics_wgr)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

ex_m=extent(shapemetrics_wgr)

covermetrics_m_wgr=crop(covermetrics_wgr,ex_m)
heightmetrics_mod_m_wgr=crop(heightmetrics_wgr,ex_m)
horizontalmetrics_mod_m_wgr=crop(horizontalmetrics_wgr,ex_m)
vertdistrmetrics_mod_m_wgr=crop(vertdistrmetrics_wgr,ex_m)
shapemetrics_mod_m_wgr=crop(shapemetrics_wgr,ex_m)

lidarmetrics_wgr=stack(covermetrics_m_wgr,shapemetrics_mod_m_wgr,vertdistrmetrics_mod_m_wgr,horizontalmetrics_mod_m_wgr,heightmetrics_mod_m_wgr)

formask <- setValues(raster(lidarmetrics_wgr[[1]]), 1)
formask[is.na(buildmask)] <- NA

lidarmetrics_masked_wgr <- mask(lidarmetrics_wgr, formask)

lidarmetrics_l23=dropLayer(lidarmetrics_masked_wgr,c(3,4,29,30))

writeRaster(lidarmetrics_l23,"lidarmetrics_l2l3_masked_wgr.grd",overwrite=TRUE)