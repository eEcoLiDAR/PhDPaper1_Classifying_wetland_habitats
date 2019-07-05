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

#source("D:/Koma/GitHub/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")
#source("D:/GitHub/eEcoLiDAR/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")
source("C:/Koma/Github/komazsofi/myPhD_escience_analysis/Paper1_inR_v2/Function_Classification.R")

# Set global variables
#setwd("D:/Koma/Paper1_v2/Run4_2019April/")
#setwd("D:/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_09April/")
setwd("C:/Koma/Sync/_Amsterdam/02_Paper1_ReedbedStructure_onlyALS/3_Dataprocessing/Results_09April/")

# Import

load("rfe_l1_rerank.RData")
load("rfe_l2_rerank.RData")
load("rfe_l3_rerank.RData")

# Conf matrix
load("conf_m_l1.RData")
load("conf_m_l2.RData")
load("conf_m_l3.RData")

# Fig3: RFE

rfe_l1_df=data.frame(rfe_l1$results$Variables, rfe_l1$results$Accuracy, rfe_l1$results$AccuracySD)
rfe_l2_df=data.frame(rfe_l2$results$Variables, rfe_l2$results$Accuracy, rfe_l2$results$AccuracySD)
rfe_l3_df=data.frame(rfe_l3$results$Variables, rfe_l3$results$Accuracy, rfe_l3$results$AccuracySD)

absoluteBest_l1 <- pickSizeBest(rfe_l1$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l1 <- pickSizeTolerance(rfe_l1$results, metric = "Accuracy", maximize = TRUE)

absoluteBest_l2 <- pickSizeBest(rfe_l2$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l2 <- pickSizeTolerance(rfe_l2$results, metric = "Accuracy", maximize = TRUE)

absoluteBest_l3 <- pickSizeBest(rfe_l3$results, metric = "Accuracy", maximize = TRUE)
within5Pct_l3 <- pickSizeTolerance(rfe_l3$results, metric = "Accuracy", maximize = TRUE)

p1=ggplot(rfe_l1_df,aes(x=rfe_l1$results$Variables,y=rfe_l1$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l1, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l1$results$Accuracy-rfe_l1$results$AccuracySD, ymax=rfe_l1$results$Accuracy+rfe_l1$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of LiDAR metrics") + ylab("Accuracy") + ylim(0, 1) + theme_bw(base_size = 17) + theme(plot.title = element_text(size=17))
p2=ggplot(rfe_l2_df,aes(x=rfe_l2$results$Variables,y=rfe_l2$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l2, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l2$results$Accuracy-rfe_l2$results$AccuracySD, ymax=rfe_l2$results$Accuracy+rfe_l2$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of LiDAR metrics") + ylab("Accuracy") + ylim(0, 1) + theme_bw(base_size = 17) + theme(plot.title = element_text(size=17))
p3=ggplot(rfe_l3_df,aes(x=rfe_l3$results$Variables,y=rfe_l3$results$Accuracy))+geom_point(color="black",size=3) + geom_line(color="black",size=2) + geom_vline(xintercept = within5Pct_l3, color="red", size=2) + geom_ribbon(aes(ymin=rfe_l3$results$Accuracy-rfe_l3$results$AccuracySD, ymax=rfe_l3$results$Accuracy+rfe_l3$results$AccuracySD), linetype=2, alpha=0.1) + xlab("Number of LiDAR metrics") + ylab("Accuracy") + ylim(0, 1) + theme_bw(base_size = 17) + theme(plot.title = element_text(size=17))

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)

# Fig: every results from RFE
#l1
feaimp_l1=rfe_l1[["variables"]]

feaimp_l1_all=feaimp_l1[feaimp_l1$Variables==26,]

feaimp_l1_all_pfea <- feaimp_l1_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l1_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l1_all_pfea_clas=add_varclass(feaimp_l1_all_pfea)

#l2
feaimp_l2=rfe_l2[["variables"]]

feaimp_l2_all=feaimp_l2[feaimp_l2$Variables==26,]

feaimp_l2_all_pfea <- feaimp_l2_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l2_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l2_all_pfea_clas=add_varclass(feaimp_l2_all_pfea)

#l3
feaimp_l3=rfe_l3[["variables"]]

feaimp_l3_all=feaimp_l3[feaimp_l3$Variables==26,]

feaimp_l3_all_pfea <- feaimp_l3_all %>%
  group_by(var) %>%
  summarise(mean_imp = mean(Overall),sd_imp = sd(Overall))

names(feaimp_l3_all_pfea) <- c("variable","mean_imp","sd_imp" )
feaimp_l3_all_pfea_clas=add_varclass(feaimp_l3_all_pfea)

p4=ggplot(feaimp_l1_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 17) +
  xlab("LiDAR metrics") + ylab("Feature importance") + ylim(-0.5,6.5) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",22), rep("red",4)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "chocolate4", "3" = "blueviolet","4"="darkolivegreen3", "5"="blue"),name="Feature class",labels=c("Coverage (C_*)","3D shape (3S_*)", "Vertical variability (VV_*)","Height (H_*)","Horizontal variability (HV_*)"))

p5=ggplot(feaimp_l2_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 17) +
  xlab("LiDAR metrics") + ylab("Feature importance") + ylim(-0.5,6.5) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",11), rep("red",15)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "chocolate4", "3" = "blueviolet","4"="darkolivegreen3", "5"="blue"),name="Feature class",labels=c("Coverage (C_*)","3D shape (3S_*)", "Vertical variability (VV_*)","Height (H_*)","Horizontal variability (HV_*)"))

p6=ggplot(feaimp_l3_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = FALSE) + coord_flip() + theme_bw(base_size = 17) +
  xlab("LiDAR metrics") + ylab("Feature importance") + ylim(-0.5,6.5) + theme(axis.text.y=element_text(angle=0,colour = c(rep("black",11), rep("red",15)))) +
  scale_color_manual(values = c("1" = "deeppink", "2" = "chocolate4", "3" = "blueviolet","4"="darkolivegreen3", "5"="blue"),name="Feature class",labels=c("Coverage (C_*)","3D shape (3S_*)", "Vertical variability (VV_*)","Height (H_*)","Horizontal variability (HV_*)"))

p0=ggplot(feaimp_l1_all_pfea_clas, aes(x=reorder(variable,mean_imp),y=mean_imp)) + geom_pointrange(aes(ymin=mean_imp-sd_imp, ymax=mean_imp+sd_imp,color=factor(varclass)),size=1,show.legend = TRUE) + coord_flip() + theme_bw(base_size = 17) +
  xlab("LiDAR metrics") + ylab("Feature importance") +
  scale_color_manual(values = c("1" = "deeppink", "2" = "chocolate4", "3" = "blueviolet","4"="darkolivegreen3", "5"="blue"),name="Feature class",labels=c("Coverage (C_*)","3D shape (S_*)", "Vertical variability (VV_*)","Height (H_*)","Horizontal variability (HV_*)")) +
  theme(axis.text.y=element_text(angle=0)) 

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig4=grid.arrange(
  p1,
  p4,
  p2,
  p5,
  p3,
  p6,
  legend,
  ncol=3,
  nrow=3,
  layout_matrix=rbind(c(1,2,8), c(3,4,8),c(5,6,8)),
  widths = c(1.25,2,0.8),
  heights = c(3,3,3)
)

ggsave("Fig4.png",plot = fig4,width = 16, height = 18)
