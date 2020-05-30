# PhDPaper1: Classifying wetland habitats

This repository consist the scripts which was used for 

Koma, Z., Seijmonsbergen, A.C. & Kissling, W.D. (2020): Classifying wetland-related land cover types and habitats using fine-scale lidar metrics derived from country-wide Airborne Laser Scanning. Remote Sensing in Ecology and Conservation (in press)

The codes are still under a cleaning process. The scripts were built using the 2.0.3 version of lidR.

# Instructions for usage

The workflow:
- 0_AHN2_downloader.R (download the required tiles) 
- 1_Pipeline_PreProcess.R (preprocess of lidar data classify, normalize) 
- 2_Pipeline_LiDARmetrics.R (calculation of metrics with all possible variation) 
- 3_Pipeline_PrepareLiDARMetrics.R (put together the input LiDAR metrics file)
- 4_Pipeline_PrepareTrainingdata.R (create random training data from field polygons)
- 5_Pipeline_ClassificationAnalysis.R (run RFE and determine optimal number of features)
- 6_Pipeline_ApplyClassification.R (apply the classifier on the whole study area)

- Visualization of the results (Visualization_Analysis.R, Visualization_ClassAnalFigures.R, Visualization_FeatureAnal.R, Visualization_PrintOutMaps.R) 

Functions:
- Function_Classification.R : functions helping in RFE and applying RF
- Function_LiDARMetricsCalc.R : LiDAR metrics calculation 
