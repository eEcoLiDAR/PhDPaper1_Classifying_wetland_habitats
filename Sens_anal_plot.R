
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

x_10m <- data.frame("OA"=c(81,85,86,67,74,76,79,80,80),"NofAnn"=c(100,500,1000,100,500,1000,100,500,1000),"Level"=c('1','1','1','2','2','2','3','3','3'))
a=ggplot(data=x_10m)+geom_point(aes(x=NofAnn,y=OA,color=Level),size=3)+geom_line(aes(x=NofAnn,y=OA,color=Level),size=2)+theme_minimal(base_size = 17)+
  xlab("Number of annotation points")+ylab("Mean overall accuracy [%]")+ggtitle("a) Spatial resolution: 10 m")

x_5m <- data.frame("OA"=c(86,86,87,70,71,73,79,80,81),"NofAnn"=c(100,500,1000,100,500,1000,100,500,1000),"Level"=c('1','1','1','2','2','2','3','3','3'))
b=ggplot(data=x_5m)+geom_point(aes(x=NofAnn,y=OA,color=Level),size=3)+geom_line(aes(x=NofAnn,y=OA,color=Level),size=2)+theme_minimal(base_size = 17)+
  xlab("Number of annotation points")+ylab("Mean overall accuracy [%]")+ggtitle("b) Spatial resolution: 5 m")

x_2m <- data.frame("OA"=c(79,87,87,70,69,69,69,76,75),"NofAnn"=c(100,500,1000,100,500,1000,100,500,1000),"Level"=c('1','1','1','2','2','2','3','3','3'))
c=ggplot(data=x_2m)+geom_point(aes(x=NofAnn,y=OA,color=Level),size=3)+geom_line(aes(x=NofAnn,y=OA,color=Level),size=2)+theme_minimal(base_size = 17)+
  xlab("Number of annotation points")+ylab("Mean overall accuracy [%]")+ggtitle("c) Spatial resolution: 2.5 m")

x_1m <- data.frame("OA"=c(80,83,62,66,65,65),"NofAnn"=c(100,500,100,500,100,500),"Level"=c('1','1','2','2','3','3'))
d=ggplot(data=x_1m)+geom_point(aes(x=NofAnn,y=OA,color=Level),size=3)+geom_line(aes(x=NofAnn,y=OA,color=Level),size=2)+theme_minimal(base_size = 17)+
  xlab("Number of annotation points")+ylab("Mean overall accuracy [%]")+ggtitle("d) Spatial resolution: 1 m")


fig=grid.arrange(
  a,
  b,
  c,
  d,
  ncol=2,
  nrow=2
)

ggsave("C:/Koma/Sync/_Amsterdam/_PhD/Chapter1_habitat_type_classification/4_Figures/Fig_extra.png",plot = fig,width = 15, height = 10)
