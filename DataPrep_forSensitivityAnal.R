"
@author: Zsofia Koma, UvA
Aim: Sensitivity analysis for resolution and number of selected training data
"

library(randomForest)
library(caret)
library(rfUtilities)

library(ggplot2)
library(gridExtra)
library(ggrepel)

library(reshape2)
library(corrplot)

library(plyr)
library(dplyr)

# Set global variables
setwd("D:/Koma/Paper1/Revision/Results/5m/")

# Import

featuretable_l1_10=read.csv(paste("featuretable_level1_3000_10",".csv",sep=""))
featuretable_l2_10=read.csv(paste("featuretable_level2_3000_10",".csv",sep=""))
featuretable_l3_10=read.csv(paste("featuretable_level3_3000_10",".csv",sep=""))

featuretable_l1_5=read.csv(paste("featuretable_level1_3000_5",".csv",sep=""))
featuretable_l2_5=read.csv(paste("featuretable_level2_3000_5",".csv",sep=""))
featuretable_l3_5=read.csv(paste("featuretable_level3_3000_5",".csv",sep=""))

featuretable_l1_2.5=read.csv(paste("featuretable_level1_3000_2.5",".csv",sep=""))
featuretable_l2_2.5=read.csv(paste("featuretable_level2_3000_2.5",".csv",sep=""))
featuretable_l3_2.5=read.csv(paste("featuretable_level3_3000_2.5",".csv",sep=""))

featuretable_l1_1=read.csv(paste("featuretable_level1_3000_1",".csv",sep=""))
featuretable_l2_1=read.csv(paste("featuretable_level2_3000_1",".csv",sep=""))
featuretable_l3_1=read.csv(paste("featuretable_level3_3000_1",".csv",sep=""))

# Prep. 100,500,1000 from 10m 

featuretable_l1_10_100=ddply(featuretable_l1_10,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l2_10_100=ddply(featuretable_l2_10,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l3_10_100=ddply(featuretable_l3_10,.(V3),function(x) x[sample(nrow(x),100),])

write.table(featuretable_l1_10_100,"featuretable_l1_10_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_10_100,"featuretable_l2_10_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_10_100,"featuretable_l3_10_100.csv",row.names=FALSE,sep=",")

featuretable_l1_10_500=ddply(featuretable_l1_10,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l2_10_500=ddply(featuretable_l2_10,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l3_10_500=ddply(featuretable_l3_10,.(V3),function(x) x[sample(nrow(x),500),])

write.table(featuretable_l1_10_500,"featuretable_l1_10_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_10_500,"featuretable_l2_10_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_10_500,"featuretable_l3_10_500.csv",row.names=FALSE,sep=",")

featuretable_l1_10_1000=ddply(featuretable_l1_10,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l2_10_1000=ddply(featuretable_l2_10,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l3_10_1000=ddply(featuretable_l3_10,.(V3),function(x) x[sample(nrow(x),1000),])

write.table(featuretable_l1_10_1000,"featuretable_l1_10_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_10_1000,"featuretable_l2_10_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_10_1000,"featuretable_l3_10_1000.csv",row.names=FALSE,sep=",")

# Prep. 100,500,1000 from 5m 

featuretable_l1_5_100=ddply(featuretable_l1_5,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l2_5_100=ddply(featuretable_l2_5,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l3_5_100=ddply(featuretable_l3_5,.(V3),function(x) x[sample(nrow(x),100),])

write.table(featuretable_l1_5_100,"featuretable_l1_5_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_5_100,"featuretable_l2_5_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_5_100,"featuretable_l3_5_100.csv",row.names=FALSE,sep=",")

featuretable_l1_5_500=ddply(featuretable_l1_5,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l2_5_500=ddply(featuretable_l2_5,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l3_5_500=ddply(featuretable_l3_5,.(V3),function(x) x[sample(nrow(x),500),])

write.table(featuretable_l1_5_500,"featuretable_l1_5_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_5_500,"featuretable_l2_5_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_5_500,"featuretable_l3_5_500.csv",row.names=FALSE,sep=",")

featuretable_l1_5_1000=ddply(featuretable_l1_5,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l2_5_1000=ddply(featuretable_l2_5,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l3_5_1000=ddply(featuretable_l3_5,.(V3),function(x) x[sample(nrow(x),1000),])

write.table(featuretable_l1_5_1000,"featuretable_l1_5_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_5_1000,"featuretable_l2_5_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_5_1000,"featuretable_l3_5_1000.csv",row.names=FALSE,sep=",")

# Prep. 100,500,1000 from 2.5m 

featuretable_l1_2.5_100=ddply(featuretable_l1_2.5,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l2_2.5_100=ddply(featuretable_l2_2.5,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l3_2.5_100=ddply(featuretable_l3_2.5,.(V3),function(x) x[sample(nrow(x),100),])

write.table(featuretable_l1_2.5_100,"featuretable_l1_2.5_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_2.5_100,"featuretable_l2_2.5_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_2.5_100,"featuretable_l3_2.5_100.csv",row.names=FALSE,sep=",")

featuretable_l1_2.5_500=ddply(featuretable_l1_2.5,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l2_2.5_500=ddply(featuretable_l2_2.5,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l3_2.5_500=ddply(featuretable_l3_2.5,.(V3),function(x) x[sample(nrow(x),500),])

write.table(featuretable_l1_2.5_500,"featuretable_l1_2.5_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_2.5_500,"featuretable_l2_2.5_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_2.5_500,"featuretable_l3_2.5_500.csv",row.names=FALSE,sep=",")

featuretable_l1_2.5_1000=ddply(featuretable_l1_2.5,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l2_2.5_1000=ddply(featuretable_l2_2.5,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l3_2.5_1000=ddply(featuretable_l3_2.5,.(V3),function(x) x[sample(nrow(x),1000),])

write.table(featuretable_l1_2.5_1000,"featuretable_l1_2.5_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_2.5_1000,"featuretable_l2_2.5_1000.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_2.5_1000,"featuretable_l3_2.5_1000.csv",row.names=FALSE,sep=",")

# Prep. 100,500,1000 from 1m 
featuretable_l1_1_100=ddply(featuretable_l1_1,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l2_1_100=ddply(featuretable_l2_1,.(V3),function(x) x[sample(nrow(x),100),])
featuretable_l3_1_100=ddply(featuretable_l3_1,.(V3),function(x) x[sample(nrow(x),100),])

write.table(featuretable_l1_1_100,"featuretable_l1_1_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_1_100,"featuretable_l2_1_100.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_1_100,"featuretable_l3_1_100.csv",row.names=FALSE,sep=",")

featuretable_l1_1_500=ddply(featuretable_l1_1,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l2_1_500=ddply(featuretable_l2_1,.(V3),function(x) x[sample(nrow(x),500),])
featuretable_l3_1_500=ddply(featuretable_l3_1,.(V3),function(x) x[sample(nrow(x),500),])

write.table(featuretable_l1_1_500,"featuretable_l1_1_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l2_1_500,"featuretable_l2_1_500.csv",row.names=FALSE,sep=",")
write.table(featuretable_l3_1_500,"featuretable_l3_1_500.csv",row.names=FALSE,sep=",")

featuretable_l1_1_1000=ddply(featuretable_l1_1,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l2_1_1000=ddply(featuretable_l2_1,.(V3),function(x) x[sample(nrow(x),1000),])
featuretable_l3_1_1000=ddply(featuretable_l3_1,.(V3),function(x) x[sample(nrow(x),1000),])