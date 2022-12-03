
rm(list=ls());gc()
library(PVR)
library(dplyr)
library(ape)
library(ggplot2)
if(!require("missForest")) 
{install.packages("missForest")}
require(missForest) ##deal with missing value 'missForest"

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 phylo_859")
load("0 trait_miss_859")
load("0 occurrence 1868 watersheds+ 859 species")

#compute eigenvector of phylogenentic relatedness for filling missing values; use the first 10
tree<-phylo_859
pvr_tree <- PVRdecomp(tree, scale = TRUE)
eigenvec<-data.frame(Genus.species=tree$tip.label, pvr_tree@Eigen$vectors)

trait<-trait_miss_859
trait_miss<-full_join(trait,eigenvec[,1:11],by=("Genus.species"))
for(i in 1:7)
{trait_miss[,i]<-as.factor(trait_miss[,i])}

## check % of missing values
per_miss<-apply(trait[,8:27],2,function(x) round(length(na.omit(x))/859*100,2))     
all_miss<-apply(trait[,8:27],2,function(x) length(na.omit(x)))     
1-sum(all_miss)/859/20 # 18.1%

# randomforest fill missing value
set.seed(2021)
trait_miss_trans<-trait_miss
trait_miss_trans[8:22]<-log(trait_miss_trans[8:22]+1) # log-transform the data 
FO<-missForest::missForest(trait_miss_trans[,c(2:4,8:37)]) 
FOmiss<-data.frame(trait_miss_trans[,1:7],FO$ximp[,4:33])
FOmiss[8:22]<-exp(FOmiss[8:22])-1 # log-transform back the data 

df<-FOmiss
trait_fill<-df[,1:27]
getwd()
save(trait_fill,file="1 trait_fill_859")

## use 350 species to compare the methods
t<-na.omit(trait_miss)
set.seed(2021)
res<-vector("list",100)

for(ii in 1:100) ## iterations 100
{
  set350<-t[sample(1:dim(t)[1],350),]
  set350<-set350[order(set350$Genus.species),]
  tr<-set350
  na<-350*0.2 ### use average NA percent for each trait
  for(i in 1:20)
  {tr[sample(1:350,na),i+7]<-NA}
  
  # check  NA %|
  N1<-apply(tr,2,function(x) length(na.omit(x)))
  1-sum(N1[8:27])/350/20
  1-N1[8:27]/350
  
  # use misforest fill the NA
  rff<-tr
  rff[8:22]<-log(rff[8:22]+1) # log-transform the data 
  rf<-missForest::missForest(rff[,c(2:4,8:37)]) ### 
  rf_350<-data.frame(rff[,1:7],rf$ximp[,4:33])
  rf_350[8:22]<-exp(rf_350[8:22])-1 # log-transform back the data 
  
  # use mean
  Mean_350<-tr ## for using mean values
  M<-apply(tr[,8:24],2,function(x) mean(na.omit(x)))
  M<-c(M,0,4,1)
  
  Mean_350$EHl<-as.numeric(Mean_350$EHl)
  Mean_350$DBt<-as.numeric(Mean_350$DBt)
  Mean_350$PCr<-as.numeric(Mean_350$PCr)
  
  for(i in 8:27)
  {
    Mean_350[which(is.na(Mean_350[,i])),i]<-M[i-7]
  }
  
  ## tansfer data to numeric
  set350$EHl<-as.numeric(set350$EHl)
  rf_350$EHl<-as.numeric(rf_350$EHl)
  set350$DBt<-as.numeric(set350$DBt)
  rf_350$DBt<-as.numeric(rf_350$DBt)
  set350$PCr<-as.numeric(set350$PCr)
  rf_350$PCr<-as.numeric(rf_350$PCr)
  
  yy<-NULL
  for(i in 8:27)
  { 
    
    x<-cor.test(set350[,i],rf_350[,i],method="spearman")
    z<-cor.test(set350[,i],Mean_350[,i],method="spearman")
    y<-c(x$estimate,z$estimate)#y$estimate,
    yy<-rbind(yy,y)
  }
  
  
  p<-as.data.frame(yy)
  colnames(p)<-c("rf","M")
  p$name<-colnames(tr)[8:27]
  res[[ii]]<-p
}
getwd()

rf<-apply(sapply(res,function(x) x$rf),1,mean)
m<-apply(sapply(res,function(x) x$M),1,mean)
rf.sd<-apply(sapply(res,function(x) x$rf),1,function(y) sd(y)/sqrt(length(y)))
m.sd<-apply(sapply(res,function(x) x$M),1,function(y) sd(y)/sqrt(length(y)))

# data
dat<-data.frame(name=rep(res[[1]]$name,2),group=rep(c("rf","mean"),each=20),mean=c(rf,m),sd=c(rf.sd,m.sd))
dat$name<-factor(dat$name,levels = res[[1]]$name)
dat$group<-factor(dat$group,levels=c("rf","mean"))

## plot Fig. S7
## remove mean & EHl PCr DBt
dat<-dat%>%filter(!(name%in%c("EHl","PCr","DBt")&group=="mean"))
tapply(dat$mean,dat$group,range)

ff<-ggplot(data=dat,aes(x=name,y=mean,ymin = mean-sd, ymax = mean+sd,fill=group))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(position=position_dodge(width=0.9 ), width = 0.2)+
  coord_cartesian(ylim = c(0.7,1))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+xlab("Trait code")+ylab("Spearman rho")+
  theme(panel.grid = element_blank(), 
        legend.title = element_text(),
        axis.text.x = element_text(angle=45, hjust=1,vjust=1,color = "black"),
        plot.margin = unit(c(2,8,5,3),"mm")) +
  scale_fill_discrete(name="Methods",labels=c('RandomForest','Average'))+
  scale_x_discrete(labels=dat$name)
ff 
tapply(dat$mean,dat$group,range)
getwd()
setwd("../output")
ggsave("1 fill missing values RF&mean comparision (Fig. S7).png", ff, width = 8, height = 6,dpi=600)



