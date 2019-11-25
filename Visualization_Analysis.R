"
@author: Zsofia Koma, UvA
Aim: Analyse the results of the classification - feature importance and response curves
"

library(randomForest)
library(caret)

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

library(reshape2)
library(dplyr)

library(sjPlot)
library(corrplot)
library(GMCM)

#source("D:/Koma/GitHub/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R")
source("D:/GitHub/eEcoLiDAR/PhDPaper1_Classifying_wetland_habitats/Function_Classification.R")
#source("C:/Koma/Github/komazsofi/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")

# Set global variables
setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_forreview2019Oct/")
#setwd("D:/Koma/Paper1/Revision/Results/5m/")
#setwd("C:/Koma/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_09April/")

# Import

featuretable_l1=read.csv("featuretable_level1_500_5.csv")
featuretable_l2=read.csv("featuretable_level2_500_5.csv")
featuretable_l3=read.csv("featuretable_level3_500_5.csv")

featuretable_l1=featuretable_l1[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l2=featuretable_l2[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]
featuretable_l3=featuretable_l3[c("cancov","dens_perc_b2","dens_perc_b2_5","linearity","zvar","zskew","zkurto","vertdenrat","tpi","var_dsm","z025quantile","zcoeffvar","roughness.2","aspect","V3")]

# Pre-process - rename coloumns, add feature classes

names(featuretable_l1) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l2) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

names(featuretable_l3) <- c("C_can","C_b2","C_2.5","S_lin","VV_var","VV_skew","VV_kurt","VV_vdr","HV_tpi","HV_var","H_25p","VV_coefvar","T_rough","T_asp","V3")

load("rfe_l1.RData")
load("rfe_l2.RData")
load("rfe_l3.RData")

load("modelFit_l1.RData")
load("modelFit_l2.RData")
load("modelFit_l3.RData")

# RFE results with feature importance + all ranked feature importance
rfe_l1_df=data.frame(rfe_l1$results$Variables, rfe_l1$results$Accuracy, rfe_l1$results$AccuracySD)
rfe_l2_df=data.frame(rfe_l2$results$Variables, rfe_l2$results$Accuracy, rfe_l2$results$AccuracySD)
rfe_l3_df=data.frame(rfe_l3$results$Variables, rfe_l3$results$Accuracy, rfe_l3$results$AccuracySD)

absoluteBest_l1 <- pickSizeBest(rfe_l1$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l1 <- pickSizeTolerance(rfe_l1$results, metric = "Accuracy", maximize = TRUE,tol=5)

absoluteBest_l2 <- pickSizeBest(rfe_l2$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l2 <- pickSizeTolerance(rfe_l2$results, metric = "Accuracy", maximize = TRUE,tol=5)

absoluteBest_l3 <- pickSizeBest(rfe_l3$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l3 <- pickSizeTolerance(rfe_l3$results, metric = "Accuracy", maximize = TRUE,tol=5)

p1=ggplot(rfe_l1_df,aes(x=rfe_l1$results$Variables,y=rfe_l1$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l1, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l1$results$Accuracy-rfe_l1$results$AccuracySD, ymax=rfe_l1$results$Accuracy+rfe_l1$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of lidar metrics") + ylab("Overall Accuracy") + ylim(0, 1) + theme_bw(base_size = 22) + ggtitle("a)")
p2=ggplot(rfe_l2_df,aes(x=rfe_l2$results$Variables,y=rfe_l2$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l2, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l2$results$Accuracy-rfe_l2$results$AccuracySD, ymax=rfe_l2$results$Accuracy+rfe_l2$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of lidar metrics") + ylab("Overall Accuracy") + ylim(0, 1) + theme_bw(base_size = 22)  + ggtitle("b)")
p3=ggplot(rfe_l3_df,aes(x=rfe_l3$results$Variables,y=rfe_l3$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l3, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l3$results$Accuracy-rfe_l3$results$AccuracySD, ymax=rfe_l3$results$Accuracy+rfe_l3$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of lidar metrics") + ylab("Overall Accuracy") + ylim(0, 1) + theme_bw(base_size = 22)  + ggtitle("c)")

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)

t_l1 <- textGrob("Level 1: Vegetation",gp=gpar(fontsize=30, col="black", fontface="bold"))
t_l2 <- textGrob("Level 2: Wetland habitat",gp=gpar(fontsize=30, col="black", fontface="bold"))
t_l3 <- textGrob("Level 3: Reedbed habitat",gp=gpar(fontsize=30, col="black", fontface="bold"))

# Fig: every results from RFE
#l1
feaimp_l1=rfe_l1[["variables"]]

feaimp_l1_all=feaimp_l1[feaimp_l1$Variables==14,]

feaimp_l1_all_pfea <- feaimp_l1_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l1_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l1_all_pfea_clas=add_varclass(feaimp_l1_all_pfea)

#l2
feaimp_l2=rfe_l2[["variables"]]

feaimp_l2_all=feaimp_l2[feaimp_l2$Variables==14,]

feaimp_l2_all_pfea <- feaimp_l2_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l2_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l2_all_pfea_clas=add_varclass(feaimp_l2_all_pfea)

#l3
feaimp_l3=rfe_l3[["variables"]]

feaimp_l3_all=feaimp_l3[feaimp_l3$Variables==14,]

feaimp_l3_all_pfea <- feaimp_l3_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l3_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l3_all_pfea_clas=add_varclass(feaimp_l3_all_pfea)

p4=ggplot(feaimp_l1_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 25) +
  geom_hline(yintercept = feaimp_l1_all_pfea_clas$mean_imp[feaimp_l1_all_pfea_clas$variable=="HV_var"], color="red", size=1.5) + ggtitle("d)") +
  xlab("Lidar metrics") + ylab("Feature importance (MDI)") + ylim(-2,13) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",14-within5Pct_l1), rep("red",within5Pct_l1)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "blueviolet","4"="blue", "5"="darkolivegreen3", "6"="chocolate4"),name="Feature class",labels=c("Cover (C_*)","3D shape (S_*)", "Vertical variability (VV_*)","Horizontal variability (HV_*)","Height (H_*)","Topography (T_*)"))

p5=ggplot(feaimp_l2_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 25) +
  geom_hline(yintercept = feaimp_l2_all_pfea_clas$mean_imp[feaimp_l2_all_pfea_clas$variable=="H_25p"], color="red", size=1.5) + ggtitle("e)") +
  xlab("Lidar metrics") + ylab("Feature importance (MDI)") + ylim(-2,13) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",14-within5Pct_l2), rep("red",within5Pct_l2)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "blueviolet","4"="blue", "5"="darkolivegreen3", "6"="chocolate4"),name="Feature class",labels=c("Cover (C_*)","3D shape (S_*)", "Vertical variability (VV_*)","Horizontal variability (HV_*)","Height (H_*)","Topography (T_*)"))

p6=ggplot(feaimp_l3_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 25) +
  geom_hline(yintercept = feaimp_l3_all_pfea_clas$mean_imp[feaimp_l3_all_pfea_clas$variable=="C_b2"], color="red", size=1.5) + ggtitle("f)") +
  xlab("Lidar metrics") + ylab("Feature importance (MDI)") + ylim(-2,13) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",14-within5Pct_l3), rep("red",within5Pct_l3)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "blueviolet","4"="blue", "5"="darkolivegreen3", "6"="chocolate4"),name="Feature class",labels=c("Cover (C_*)","3D shape (S_*)", "Vertical variability (VV_*)","Horizontal variability (HV_*)","Height (H_*)","Topography (T_*)"))

p0=ggplot(feaimp_l1_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = TRUE) + coord_flip() + theme_bw(base_size = 30) +
  xlab("Lidar metrics") + ylab("Feature importance") +
  scale_color_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "blueviolet","4"="blue", "5"="darkolivegreen3", "6"="chocolate4"),name="Feature class",labels=c("Cover (C_*)","3D shape (S_*)", "Vertical variability (VV_*)","Height (H_*)","Horizontal variability (HV_*)","Topography (T_*)")) +
  theme(axis.text.y=element_text(angle=0)) + theme(legend.position="bottom")

legend <- get_legend(p0)

fig4=grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  legend,
  t_l1,
  t_l2,
  t_l3,
  ncol=3,
  nrow=4,
  layout_matrix=rbind(c(8,9,10),c(1,2,3),c(4,5,6), c(7,7,7)),
  widths = c(2,2,2),
  heights = c(0.2,4,4,1)
)

ggsave("Fig3.png",plot = fig4,width = 18, height = 22)

# Response curves - partial dependence plot
# level 1
imp <- importance(modelFit_l1)
impvar <- rownames(imp)[order(imp[, 3], decreasing=TRUE)]

id=1
response_l1_imp1 <- Response_l1(modelFit_l1,featuretable_l1,id)
response_l1_imp1$trlog_y=GMCM:::inv.logit(response_l1_imp1$class_1_y)

pp1=ggplot(response_l1_imp1,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("25th percentile of height [H_25p (m)]") + ylab("Partial dependence (probability)") + scale_color_manual(values = c("1" = "gray", "2" = "aquamarine4"),name="General classes",labels=c("Planar surface", "Vegetation")) + theme_bw(base_size = 30) + ggtitle("a)")

# level 2
imp <- importance(modelFit_l2)
impvar <- rownames(imp)[order(imp[, 3], decreasing=TRUE)]

id=1
response_l2_imp1 <- Response_l2(modelFit_l2,featuretable_l2,id)
response_l2_imp1$trlog_y=GMCM:::inv.logit(response_l2_imp1$class_1_y)

id=2
response_l2_imp2 <- Response_l2(modelFit_l2,featuretable_l2,id)
response_l2_imp2$trlog_y=GMCM:::inv.logit(response_l2_imp2$class_1_y)

pp3=ggplot(response_l2_imp1,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("Variance of DSM [HV_var (m)]") + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1" = "darkgreen", "2" = "green1", "3" = "gold","4"="darkolivegreen4"),
                                                                                                                                                                    name="Wetland",labels=c("Forest", "Grassland","Reedbed","Shrub")) + theme_bw(base_size = 30) + ggtitle("b)")+ xlim(0, 25)
pp4=ggplot(response_l2_imp2,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("Variance of height [VV_var (m)]") + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1" = "darkgreen", "2" = "green1", "3" = "gold","4"="darkolivegreen4"),
                                                                                                                                                                     name="Wetland",labels=c("Forest", "Grassland","Reedbed","Shrub")) + theme_bw(base_size = 30) + ggtitle("c)") + xlim(0, 25)


#level 3
imp <- importance(modelFit_l3)
impvar <- rownames(imp)[order(imp[, 3], decreasing=TRUE)]

id=2
response_l3_imp1 <- Response_l3(modelFit_l3,featuretable_l3,id)
response_l3_imp1$trlog_y=GMCM:::inv.logit(response_l3_imp1$class_1_y)

id=3
response_l3_imp2 <- Response_l3(modelFit_l3,featuretable_l3,id)
response_l3_imp2$trlog_y=GMCM:::inv.logit(response_l3_imp2$class_1_y)

id=2
response_l3_imp3 <- Response_l3(modelFit_l3,featuretable_l3,id)
response_l3_imp3$trlog_y=GMCM:::inv.logit(response_l3_imp3$class_1_y)

pp5=ggplot(response_l3_imp1,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("Variance of height [VV_var (m)]") + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1"="tan2","2"="gold","3"="chocolate4"),name="Reedbed",labels=c("Land reed rich","Land reed poor","Water reed")) + theme_bw(base_size = 30) + ggtitle("d)") + xlim(0, 15)
pp6=ggplot(response_l3_imp2,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("25th percentile of height [H_25p (m)]") + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1"="tan2","2"="gold","3"="chocolate4"),name="Reedbed",labels=c("Land reed rich","Land reed poor","Water reed")) + theme_bw(base_size = 30)  + ggtitle("e)") + xlim(0, 10)
pp7=ggplot(response_l3_imp3,aes(x=class_1_x,y=trlog_y,color=factor(class))) + geom_line(size=2,show.legend = FALSE) + xlab("Density of vegetation below 2 m [C_b2 (%)]") + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1"="tan2","2"="gold","3"="chocolate4"),name="Reedbed",labels=c("Land reed rich","Land reed poor","Water reed")) + theme_bw(base_size = 30) + ggtitle("f)")

#legends

p00_b=ggplot(response_l1_imp1,aes(x=class_1_x,y=class_1_y,color=factor(class))) + geom_line(size=5,show.legend = TRUE) + xlab(impvar[3]) + ylab("Partial dependence (logit of probability)") + scale_color_manual(values = c("1" = "gray", "2" = "aquamarine4"),name="Vegetation",labels=c("Planar surface", "Vegetation")) + theme_bw(base_size = 35) + theme(legend.position="bottom")
legend_00_b <- get_legend(p00_b)

p02_b=ggplot(response_l2_imp1,aes(x=class_1_x,y=class_1_y,color=factor(class))) + geom_line(size=5,show.legend = TRUE) + xlab(impvar[2]) + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1" = "darkgreen", "2" = "green1", "3" = "gold","4"="darkolivegreen4"),name="Wetland",labels=c("Forest", "Grassland","Reedbed","Shrub")) + theme_bw(base_size = 35) + theme(legend.position="bottom")
legend_02_b <- get_legend(p02_b)

p03_b=ggplot(response_l3_imp1,aes(x=class_1_x,y=class_1_y,color=factor(class))) + geom_line(size=5,show.legend = TRUE) + xlab(impvar[6]) + ylab("Partial dependence (probability)")+ scale_color_manual(values = c("1"="tan2","2"="gold","3"="chocolate4"),name="Reedbed",labels=c("Land reed rich","Land reed poor","Water reed")) + theme_bw(base_size = 35) + theme(legend.position="bottom")
legend_03_b <- get_legend(p03_b)

#selected parc.dep.

fig5=grid.arrange(
  pp1,
  pp3,
  pp4,
  pp5,
  pp6,
  pp7,
  legend_00_b,
  legend_02_b,
  legend_03_b,
  t_l1,
  t_l2,
  t_l3,
  ncol=3,
  nrow=6,
  layout_matrix=rbind(c(10,11,11),c(1,2,3),c(7,8,8),c(12,12,12),c(4,5,6),c(9,9,9)),
  widths = c(1,1,1),
  heights = c(0.2,3,0.2,0.2,3,0.2)
)

ggsave("Fig5.png",plot = fig5,width = 28, height = 22)
