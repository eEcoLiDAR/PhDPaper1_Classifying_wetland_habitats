"
@author: Zsofia Koma, UvA
Aim: Analyse the results of the classification - feature importance and response curves
"

library(randomForest)
library(caret)

library(ggplot2)
library(gridExtra)
library(ggrepel)

library(reshape2)

#source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")
source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")


# Set global variables
#setwd("D:/Koma/Paper1_v2/Run4_2019April/")
setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_05April/")

# Import

featuretable_l1=read.csv("featuretable_level1_b2o5.csv")
featuretable_l2=read.csv("featuretable_level2_b2o5.csv")
featuretable_l3=read.csv("featuretable_level3_b2o5.csv")

featuretable_l1_foranal=read.csv("featuretable_b2o5_wgr_whgr.csv")
featuretable_l1_foranal=featuretable_l1_foranal[featuretable_l1_foranal$layer==2,]

featuretable_l1_a=featuretable_l1_foranal[ ,c(1:26)]
featuretable_l1_b=featuretable_l1_foranal[ ,c(27:52)]

# Pre-process - rename coloumns, add feature classes

names(featuretable_l1) <- c("C_puls","C_can","3S_curv","3S_lin","S_plan","3S_sph","3S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

names(featuretable_l2) <- c("C_puls","C_can","3S_curv","3S_lin","S_plan","3S_sph","3S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

names(featuretable_l3) <- c("C_puls","C_can","3S_curv","3S_lin","S_plan","3S_sph","3S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                            "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p","layer")

names(featuretable_l1_a) <- c("C_puls","C_can","3S_curv","3S_lin","S_plan","3S_sph","3S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                              "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p")

names(featuretable_l1_b) <- c("C_puls","C_can","3S_curv","3S_lin","S_plan","3S_sph","3S_ani","VV_sd","VV_var","VV_skew","VV_kurt","VV_cr","VV_vdr","VV_simp","VV_shan","HV_rough","HV_tpi","HV_tri",
                              "HV_sd","HV_var","H_max","H_mean","H_med","H_25p","H_75p","H_90p")

# App.1.. : boxplots ground whout ground
vegetation_wgr=featuretable_l1_a[ ,c(1:2)]
vegetation_whgr=featuretable_l1_b[,c(1:2)]

vegetation_wgr_f=melt(vegetation_wgr)
vegetation_whgr_f=melt(vegetation_whgr)

vegetation_wgr_f$class <-1
vegetation_whgr_f$class <-2

vegetation_var=rbind(vegetation_wgr_f,vegetation_whgr_f)

p1=ggplot(data = vegetation_var, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Cover") + ylab("Ratio[%]") +
  theme_bw(base_size = 17)

vegetation_wgr_h=featuretable_l1_a[ ,c(3:7)]
vegetation_whgr_h=featuretable_l1_b[,c(3:7)]

vegetation_wgr_f_h=melt(vegetation_wgr_h)
vegetation_whgr_f_h=melt(vegetation_whgr_h)

vegetation_wgr_f_h$class <-1
vegetation_whgr_f_h$class <-2

vegetation_var_h=rbind(vegetation_wgr_f_h,vegetation_whgr_f_h)

p2=ggplot(data = vegetation_var_h, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: 3D shape") + ylab("Ratio between eigenvalues") +
  theme_bw(base_size = 17)

vegetation_wgr_h=featuretable_l1_a[ ,c(8:11)]
vegetation_whgr_h=featuretable_l1_b[,c(8:11)]

vegetation_wgr_f_h=melt(vegetation_wgr_h)
vegetation_whgr_f_h=melt(vegetation_whgr_h)

vegetation_wgr_f_h$class <-1
vegetation_whgr_f_h$class <-2

vegetation_var_h=rbind(vegetation_wgr_f_h,vegetation_whgr_f_h)

p3a=ggplot(data = vegetation_var_h, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

vegetation_wgr_h=featuretable_l1_a[ ,c(12:15)]
vegetation_whgr_h=featuretable_l1_b[,c(12:15)]

vegetation_wgr_f_h=melt(vegetation_wgr_h)
vegetation_whgr_f_h=melt(vegetation_whgr_h)

vegetation_wgr_f_h$class <-1
vegetation_whgr_f_h$class <-2

vegetation_var_h=rbind(vegetation_wgr_f_h,vegetation_whgr_f_h)

p3b=ggplot(data = vegetation_var_h, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

vegetation_wgr_h=featuretable_l1_a[ ,c(16:20)]
vegetation_whgr_h=featuretable_l1_b[,c(16:20)]

vegetation_wgr_f_h=melt(vegetation_wgr_h)
vegetation_whgr_f_h=melt(vegetation_whgr_h)

vegetation_wgr_f_h$class <-1
vegetation_whgr_f_h$class <-2

vegetation_var_h=rbind(vegetation_wgr_f_h,vegetation_whgr_f_h)

p4=ggplot(data = vegetation_var_h, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Horizontal variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

vegetation_wgr_h=featuretable_l1_a[ ,c(21:26)]
vegetation_whgr_h=featuretable_l1_b[,c(21:26)]

vegetation_wgr_f_h=melt(vegetation_wgr_h)
vegetation_whgr_f_h=melt(vegetation_whgr_h)

vegetation_wgr_f_h$class <-1
vegetation_whgr_f_h$class <-2

vegetation_var_h=rbind(vegetation_wgr_f_h,vegetation_whgr_f_h)

p5=ggplot(data = vegetation_var_h, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

p0=ggplot(data = vegetation_var, aes(x=variable, y=value,fill=factor(class))) + geom_boxplot(show.legend = TRUE)+
  scale_fill_manual(values = c("1" = "coral1", "2" = "cyan2"),name="Calculation type",labels=c("With ground","Without ground")) +
  xlab("Feature class: Cover") + ylab("Ratio[%]") +
  theme_bw(base_size = 17)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

grid.arrange(
  p2,
  p5,
  p4,
  p3a,
  p3b,
  legend,
  ncol=2,
  nrow=3,
  layout_matrix=rbind(c(1,2),c(3,4),c(5,6))
)

# boxplots per classes

# level1
featuretable_l1_c=featuretable_l1[ ,c(1:2,27)]
featuretable_l1_c_b=melt(featuretable_l1_c,id.vars="layer")

featuretable_l1_3s=featuretable_l1[ ,c(3:7,27)]
featuretable_l1_3s_b=melt(featuretable_l1_3s,id.vars="layer")

featuretable_l1_vva=featuretable_l1[ ,c(8:11,27)]
featuretable_l1_vva_b=melt(featuretable_l1_vva,id.vars="layer")

featuretable_l1_vvb=featuretable_l1[ ,c(12:15,27)]
featuretable_l1_vvb_b=melt(featuretable_l1_vvb,id.vars="layer")

featuretable_l1_hv=featuretable_l1[ ,c(16:20,27)]
featuretable_l1_hv_b=melt(featuretable_l1_hv,id.vars="layer")

featuretable_l1_h=featuretable_l1[ ,c(21:26,27)]
featuretable_l1_h_b=melt(featuretable_l1_h,id.vars="layer")

p11=ggplot(data = featuretable_l1_c_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: Cover") + ylab("Ratio [%]") +
  theme_bw(base_size = 17)

p12=ggplot(data = featuretable_l1_3s_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: 3D shape") + ylab("Ratio of eigenvalues") +
  theme_bw(base_size = 17)

p13=ggplot(data = featuretable_l1_vva_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p14=ggplot(data = featuretable_l1_vvb_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p15=ggplot(data = featuretable_l1_hv_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: Horizontal variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17) + ylim(0,10)

p16=ggplot(data = featuretable_l1_h_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgrey", "2" = "aquamarine4"),name="Level 1",labels=c("Planar surfaces","Vegetation")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

grid.arrange(
  p11,
  p12,
  p13,
  p14,
  p15,
  p16,
  ncol=3,
  nrow=2,
  layout_matrix=rbind(c(1,2,3),c(4,5,6))
)

# level2

featuretable_l2_c=featuretable_l2[ ,c(1:2,27)]
featuretable_l2_c_b=melt(featuretable_l2_c,id.vars="layer")

featuretable_l2_3s=featuretable_l2[ ,c(3:7,27)]
featuretable_l2_3s_b=melt(featuretable_l2_3s,id.vars="layer")

featuretable_l2_vva=featuretable_l2[ ,c(8:11,27)]
featuretable_l2_vva_b=melt(featuretable_l2_vva,id.vars="layer")

featuretable_l2_vvb=featuretable_l2[ ,c(12:15,27)]
featuretable_l2_vvb_b=melt(featuretable_l2_vvb,id.vars="layer")

featuretable_l2_hv=featuretable_l2[ ,c(16:20,27)]
featuretable_l2_hv_b=melt(featuretable_l2_hv,id.vars="layer")

featuretable_l2_h=featuretable_l2[ ,c(21:26,27)]
featuretable_l2_h_b=melt(featuretable_l2_h,id.vars="layer")

p11=ggplot(data = featuretable_l2_c_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Cover") + ylab("Ratio [%]") +
  theme_bw(base_size = 17)

p12=ggplot(data = featuretable_l2_3s_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: 3D shape") + ylab("Ratio of eigenvalues") +
  theme_bw(base_size = 17)

p13=ggplot(data = featuretable_l2_vva_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p14=ggplot(data = featuretable_l2_vvb_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p15=ggplot(data = featuretable_l2_hv_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Horizontal variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17) + ylim(0,10)

p16=ggplot(data = featuretable_l2_h_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

p00=ggplot(data = featuretable_l2_h_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = TRUE)+
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "chartreuse", "3" = "darkorange3", "4" = "darkolivegreen3"),name="Level 2",labels=c("Forest","Grassland","Reedbed","Shrub")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p00)

grid.arrange(
  p11,
  p12,
  p13,
  p14,
  p15,
  p16,
  legend,
  ncol=4,
  nrow=2,
  layout_matrix=rbind(c(1,2,3,7),c(4,5,6,7))
)

# level3

featuretable_l3_c=featuretable_l3[ ,c(1:2,27)]
featuretable_l3_c_b=melt(featuretable_l3_c,id.vars="layer")

featuretable_l3_3s=featuretable_l3[ ,c(3:7,27)]
featuretable_l3_3s_b=melt(featuretable_l3_3s,id.vars="layer")

featuretable_l3_vva=featuretable_l3[ ,c(8:11,27)]
featuretable_l3_vva_b=melt(featuretable_l3_vva,id.vars="layer")

featuretable_l3_vvb=featuretable_l3[ ,c(12:15,27)]
featuretable_l3_vvb_b=melt(featuretable_l3_vvb,id.vars="layer")

featuretable_l3_hv=featuretable_l3[ ,c(16:20,27)]
featuretable_l3_hv_b=melt(featuretable_l3_hv,id.vars="layer")

featuretable_l3_h=featuretable_l3[ ,c(21:26,27)]
featuretable_l3_h_b=melt(featuretable_l3_h,id.vars="layer")

p11=ggplot(data = featuretable_l3_c_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Cover") + ylab("Ratio [%]") +
  theme_bw(base_size = 17)

p12=ggplot(data = featuretable_l3_3s_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: 3D shape") + ylab("Ratio of eigenvalues") +
  theme_bw(base_size = 17)

p13=ggplot(data = featuretable_l3_vva_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p14=ggplot(data = featuretable_l3_vvb_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Vertical variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17)

p15=ggplot(data = featuretable_l3_hv_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Horizontal variability") + ylab("Variability [m]") +
  theme_bw(base_size = 17) + ylim(0,10)

p16=ggplot(data = featuretable_l3_h_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

p00=ggplot(data = featuretable_l3_h_b, aes(x=variable, y=value,fill=factor(layer))) + geom_boxplot(show.legend = TRUE)+
  scale_fill_manual(values = c("1" = "darkgoldenrod1", "2" = "darkorange", "3" = "darkorange4"),name="Level 3",labels=c("Landreed structurally poor","Landreed structurally rich","Water reed")) +
  xlab("Feature class: Height") + ylab("Height [m]") +
  theme_bw(base_size = 17)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p00)

grid.arrange(
  p11,
  p12,
  p13,
  p14,
  p15,
  p16,
  legend,
  ncol=4,
  nrow=2,
  layout_matrix=rbind(c(1,2,3,7),c(4,5,6,7))
)

# Partial dependence plots