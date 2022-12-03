# remove NSR,FDis
# find optimal combinations of BRT  invasibility
rm(list=ls());gc()
library(ggBRT)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT")
# remove NSR,FDis
brtest<-div_env[,-c(3,7)]
## find optimal metrics
com<-NULL
for(lr in c(0.01,0.005,0.001,0.0005))#
{
  for(tc in seq(1,6,1))
  {   
    
    for(bf in seq(0.6,0.8,0.1))
    {
      set.seed(2021)
      gbm <- gbm.step(data=brtest, gbm.x = 3:14, gbm.y = 2,
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
opest<-com[order(com[,4],decreasing = T),]
save(opest,file="9 opest") # optimal for established species
