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


# Set working directory
workingdirectory="D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Paper1_revision/5m/" ## set this directory where your input las files are located
#workingdirectory="C:/Koma/Paper1/Paper1_DataProcess/"
setwd(workingdirectory)

## Level 1
# Read the separate lidarmetrics files into memory

covermetrics=stack("covermetrics_gr_5m.grd")
proj4string(covermetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

heightmetrics=stack("height_metrics_gr_5m.grd")
proj4string(heightmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontalmetrics=stack("horizontal_metrics_gr_5m.grd")
proj4string(horizontalmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

shapemetrics=stack("shapemetrics_gr_5m.grd")
proj4string(shapemetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

vertdistrmetrics=stack("vertdistr_metrics_gr_5m.grd")
proj4string(vertdistrmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

dtmmetrics=stack("dtm_metrics5m.grd")
proj4string(dtmmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

buildmask=stack("building_formask.grd")
proj4string(buildmask)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Merge files into one lidarmetrics file
ex_m=extent(shapemetrics)

covermetrics_m=crop(covermetrics,ex_m)
heightmetrics_mod_m=crop(heightmetrics,ex_m)
horizontalmetrics_mod_m=crop(horizontalmetrics,ex_m)
vertdistrmetrics_mod_m=crop(vertdistrmetrics,ex_m)
dtmmetrics_mod_m=crop(dtmmetrics,ex_m)

vertdistrmetrics_mod_m$zskew[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$zkurto[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$canrelrat[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$vertdenrat[vertdistrmetrics_mod_m$zstd==0]<-0

lidarmetrics_l1=stack(covermetrics_m,shapemetrics,vertdistrmetrics_mod_m,horizontalmetrics_mod_m,heightmetrics_mod_m,dtmmetrics_mod_m)

# Exclude buildings
formask <- setValues(raster(lidarmetrics_l1[[1]]), 1)
formask[is.na(buildmask)] <- NA

lidarmetrics_masked <- mask(lidarmetrics_l1, formask)
lidarmetrics_exp=dropLayer(lidarmetrics_masked,c(30))
writeRaster(lidarmetrics_exp,"lidarmetrics_l1_masked_5m.grd",overwrite=TRUE)

## Level 2-3
# Read the separate lidarmetrics files into memory

covermetrics=stack("covermetrics_gr_norm_5m.grd")
proj4string(covermetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

heightmetrics=stack("height_metrics_whgr_gr_norm_5m.grd")
proj4string(heightmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

horizontalmetrics=stack("horizontal_metrics_whgr_gr_norm_ 5 m.grd")
proj4string(horizontalmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

shapemetrics=stack("shapemetrics_whgr_gr_norm_5m.grd")
proj4string(shapemetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

vertdistrmetrics=stack("vertdistr_metrics_whgr_gr_norm_5m.grd")
proj4string(vertdistrmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

dtmmetrics=stack("dtm_metrics5m.grd")
proj4string(dtmmetrics)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

buildmask=stack("building_formask.grd")
proj4string(buildmask)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Merge files into one lidarmetrics file
ex_m=extent(shapemetrics)

covermetrics_m=crop(covermetrics,ex_m)
heightmetrics_mod_m=crop(heightmetrics,ex_m)
horizontalmetrics_mod_m=crop(horizontalmetrics,ex_m)
vertdistrmetrics_mod_m=crop(vertdistrmetrics,ex_m)
dtmmetrics_mod_m=crop(dtmmetrics,ex_m)

vertdistrmetrics_mod_m$zskew[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$zkurto[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$canrelrat[vertdistrmetrics_mod_m$zstd==0]<-0
vertdistrmetrics_mod_m$vertdenrat[vertdistrmetrics_mod_m$zstd==0]<-0

lidarmetrics_l1=stack(covermetrics_m,shapemetrics,vertdistrmetrics_mod_m,horizontalmetrics_mod_m,heightmetrics_mod_m,dtmmetrics_mod_m)

# Exclude buildings
formask <- setValues(raster(lidarmetrics_l1[[1]]), 1)
formask[is.na(buildmask)] <- NA

lidarmetrics_masked <- mask(lidarmetrics_l1, formask)
lidarmetrics_exp=dropLayer(lidarmetrics_masked,c(30))
writeRaster(lidarmetrics_exp,"lidarmetrics_l23_masked_5m.grd",overwrite=TRUE)

