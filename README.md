# PhDPaper1: Classifying wetland habitats

This repository consist the scripts which was used for 

Zs. Koma, A. C. Seijmonsbergen and W. D. Kissling (submitted): Classifying wetland habitats using fine-scale LiDAR metrics derived from country-wide Airborne Laser Scanning manuscript

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
