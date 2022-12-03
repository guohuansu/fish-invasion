# find optimal combinations of BRT  invasibility
rm(list=ls());gc()
library(ggBRT)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT")
load("9 opest")
source("ggPD_su (re-rank).R")

brtest<-div_env[,-c(3,7)]
res<-NULL # R-squared value
con<-data.frame(var=NA) # contribution of each var
set.seed(2021)
for(i in 1:100)
{
  gbm <- gbm.step(data=brtest, gbm.x = 3:14, gbm.y = 2,
                  tree.complexity = opest[1,2], family = "poisson",#poisson
                  learning.rate = opest[1,3], bag.fraction = opest[1,1],plot.main = F)
  D2<-(gbm$self.statistics$mean.null-gbm$cv.statistics$deviance.mean)/gbm$self.statistics$mean.null
  res<-c(res,D2) # %
  con<-left_join(summary(gbm,plot=F),con,by="var")
}
invaInf<-rbind(con,c(NA,res))
colnames(invaInf)[-1]<-"inf"
invaInf$mean<-apply(invaInf[,-1],1,mean)
getwd()
save(invaInf, file="10 BRT invasibility 100 loop")

load("10 BRT invasibility 100 loop")
conM<-invaInf[,c(1,102)]
# plot figure
# for plot 
set.seed(2021)
gbm <- gbm.step(data=brtest, gbm.x = 3:14, gbm.y = 2,
                tree.complexity = opest[1,2], family = "poisson",#poisson
                learning.rate = opest[1,3], bag.fraction = opest[1,1],plot.main = F)
# change contri to mean value
cont<-left_join(gbm$contributions,conM,by="var")
gbm$contributions[,2]<-cont[,3]

D2<-(gbm$self.statistics$mean.null-gbm$cv.statistics$deviance.mean)/gbm$self.statistics$mean.null
D2# 0.676
conM[13,]# mean D2 0.679

#summary groups
sum(conM[c(2,9,10),2])# 22.1%  hunman-relaeted
sum(conM[c(3,5),2])# 21.3% environmental 
sum(conM[c(4,7,8,10,12),2])# 33%# functional diversty
conM[1,2];conM[6,2]; # 16.6%, 8.8%

setwd("../output")
png("10 BRT invasibility (Fig. 3).png",width = 5000,height = 3000,res=500) 
ggPD_su(gbm,rug = T,smooth = T,mod="loess",span=0.6,cex.smooth=1,y.label = "Community invasibility",)
dev.off()



