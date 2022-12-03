### log10-transformed FCt
###
### remove outliers
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(cowplot) 
library(fastglm)
library(glm2)
library(lme4)
library(MASS)
library(rcompanion) # pseudo r2
library(gbm)
library(dismo)
library(Rmisc)
library(boot)
library(ggcorrplot2)
library(psych)# corr.test
library(ggBRT)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("4 data for BRT invasiveness")
source("ggPD_su (re-rank).R")
source("ggMultiInf_su.R")
############################
brt$HUI<-factor(brt$HUI,levels = 0:4)# to factor
## BRT remove outliers
#1 size < 10
brt1<-brt[brt$Size_PCA1<8,]
#2 BEl >10
brt1<-brt1[brt1$BEl<10,]
#3 RMl >2
brt1<-brt1[brt1$RMl<2,]
# 7 removed
#log10-transformed FCt
brt2<-brt1
brt2$FCt<-log10(brt2$FCt)

## find optimal metrics
com<-NULL
for(lr in c(0.01,0.005,0.001,0.0005))#
{
  for(tc in seq(1,6,1))
  {   
    
    for(bf in seq(0.6,0.9,0.1))
    {
      set.seed(2021)
      gbm <- gbm.step(data=brt2, gbm.x = 3:20, gbm.y = 2,
                      family = "poisson", tree.complexity = tc,
                      learning.rate = lr, bag.fraction = bf,plot.main = F)
      ## total deviance explained by BRT CV-D2  
      D2<-(gbm$self.statistics$mean.null-gbm$cv.statistics$deviance.mean)/gbm$self.statistics$mean.null
      nt<-length(gbm$trees)
      com<-rbind(com,c(bf,tc,lr,round(D2,4),nt))
    }
  }
}
colnames(com)<-c("bf","tc","lr","CV-D2","nt")
op<-com[order(com[,4],decreasing = T),]
save(op,file="5 op")

#loop
#op1<-op
load("5 op")
res1<-NULL # R-squared value
con<-data.frame(var=NA) # contribution of each var
set.seed(2021)
for(i in 1:100)
{
  gbm <- gbm.step(data=brt2, gbm.x = 3:20, gbm.y = 2,
                  family = "poisson", tree.complexity = op[1,2],
                  learning.rate = op[1,3], bag.fraction = op[1,1],plot.main = F)
  
  D2<-(gbm$self.statistics$mean.null-gbm$cv.statistics$deviance.mean)/gbm$self.statistics$mean.null
  res1<-c(res1,D2) # %
  con<-left_join(summary(gbm,plot=F),con,by="var")
}
conR<-rbind(con,c(NA,res1))
colnames(conR)[-1]<-"inf"
conR$mean<-apply(conR[,-1],1,mean)
getwd()
save(conR, file="5 BRT 100 loop")

load("5 op")
load("5 BRT 100 loop")
conM<-conR[,c(1,102)]
# plot figure
# for plot 
set.seed(2021)
gbm <- gbm.step(data=brt2, gbm.x = 3:20, gbm.y = 2,
                family = "poisson", tree.complexity = op[1,2],
                learning.rate = op[1,3], bag.fraction = op[1,1],plot.main = F)

# change contri to mean value
cont<-left_join(gbm$contributions,conM,by="var")
gbm$contributions[,2]<-cont[,3]
#rank(-cont$mean)

D2<-(gbm$self.statistics$mean.null-gbm$cv.statistics$deviance.mean)/gbm$self.statistics$mean.null
D2# 0.435
conM[19,]# mean D2 0.422

#summary others 8:18
sum(conM[8:18,2]) # 11.3

setwd("../output")
#pdf("5 BRT invasiveness 3x4_rerank (Fig. 2).pdf",width = 10,height = 6)
#ggPD_su(gbm,rug = T,smooth = T,n.plots=12,ncol=4,mod="loess",span=0.3,cex.smooth=1,y.label = "Species invasiveness")
#dev.off()
#pdf("5 BRT invasiveness 5x4_rerank (Fig. S3).pdf",width = 10,height = 8)
#ggPD_su(gbm,rug = T,smooth = T,ncol=4,mod="loess",span=0.3,cex.smooth=1,y.label = "Species invasiveness")
#dev.off()

png("5 BRT invasiveness 3x4_rerank (Fig. 2).png",width = 5000,height = 3000,res=500)
ggPD_su(gbm,rug = T,smooth = T,n.plots=12,ncol=4,mod="loess",span=0.3,cex.smooth=1,y.label = "Species invasiveness")
dev.off()
png("5 BRT invasiveness 5x4_rerank (Fig. S3).png",width = 5000,height = 4000,res=500)
ggPD_su(gbm,rug = T,smooth = T,ncol=4,mod="loess",span=0.3,cex.smooth=1,y.label = "Species invasiveness")
dev.off()


## robustness to extreme values
#raw data 300 species
set.seed(2021)
gbma <- gbm.step(data=brt, gbm.x = 3:20, gbm.y = 2,
                 family = "poisson", tree.complexity = op[1,2],
                 learning.rate = op[1,3], bag.fraction = op[1,1],plot.main = F)
# remove 7 species with extreme values
set.seed(2021)
gbmb <- gbm.step(data=brt1, gbm.x = 3:20, gbm.y = 2,
                 family = "poisson", tree.complexity = op[1,2],
                 learning.rate = op[1,3], bag.fraction = op[1,1],plot.main = F)
# remove 7 species + FCt log10-transformed
set.seed(2021)
gbmc <- gbm.step(data=brt2, gbm.x = 3:20, gbm.y = 2,
                 family = "poisson", tree.complexity = op[1,2],
                 learning.rate = op[1,3], bag.fraction = op[1,1],plot.main = F)
# change gbmc contri to mean value
cont<-left_join(gbmc$contributions,conM,by="var")
gbmc$contributions[,2]<-cont[,3]

png("5 BRT invasiveness robustness (Fig. S9).png",width = 4000,height = 2500,res=500)
ggMultiInf_su(Model_1=gbma,Model_2=gbmb,Model_3=gbmc)
dev.off()

