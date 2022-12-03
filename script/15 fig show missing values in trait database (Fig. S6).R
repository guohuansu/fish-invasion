# Fig. S6
rm(list=ls());gc()
library(dplyr)
library(viridis)
library(cowplot)
library(gridExtra)
library(ggpubr)
library(scales)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 occurrence 1868 watersheds+ 859 species")
load("0 trait_miss_859")
load("0 traitName")
trait<-trait_miss_859[,-(3:7)]# 

# exo: spcies have successful records 
# tra: spcies have successful records 
# nat: native had never been introduced 

spe<-tapply(occ$Genus.species,occ$Status,function(x) names(table(x)))
exo<-spe[[1]] #67
tra<-spe[[3]] #233
nat<-spe[[2]][-which(spe[[2]]%in%tra)]# 559
# trait
Texo<-trait[which(trait$Genus.species%in%exo),]
Ttra<-trait[which(trait$Genus.species%in%tra),]
Tnat<-trait[which(trait$Genus.species%in%nat),]

Texo$type<-"exotic"
Ttra$type<-"translocated"
Tnat$type<-"native"
dat<-rbind(Texo,Ttra,Tnat)
dat$type<-factor(dat$type,levels = c("native","translocated","exotic")[3:1])
dat$type%>%table # exotic 73, tra 227, native only =559

apply(dat[,3:22],2,function(x) length(na.omit(x)))
checkmiss<-apply(dat[,3:22],2,function(x) tapply(x,dat$type,function(y)length(na.omit(y))))
missnb<-apply(checkmiss,1,sum)

# miss per in 859 species
checkmiss859<-apply(dat[,3:22],2,function(x) length(na.omit(x)))
sum(checkmiss859)/859/20

# miss per in the 24 Orders
trait_miss_859$Order<-as.character(trait_miss_859$Order)
checkmissOrder<-apply(trait_miss_859[8:27],2,function(x) tapply(x,trait_miss_859$Order,function(y)length(na.omit(y))))
apply(checkmissOrder,1,sum)

cc<-data.frame(a=apply(checkmissOrder,1,sum),b=table(trait_miss_859$Order))
cc$per<- cc$a/cc$b.Freq/20
colnames(cc)<-c("count","order","nb.sp","per")
cc<-rbind(
          data.frame(count=sum(checkmiss859),order="All",nb.sp=859,per=sum(checkmiss859)/20/859),
          data.frame(count=missnb[3],order="Native",nb.sp=559,per=missnb[3]/20/559),
          data.frame(count=sum(missnb[1:2]),order="Non-native",nb.sp=67+233,per=sum(missnb[1:2])/20/(67+233)),cc)
cc<-cc[order(cc$nb.sp,decreasing = T),]
cc$order<-factor(cc$order,levels = cc$order[c(1:2,4,3,5:27)])
cc<-na.omit(cc)
missper<-ggplot(data=cc,aes(x=order,y=1-per))+geom_bar(stat="identity")+scale_y_continuous(breaks = c(0,0.2,0.5,0.8,1),labels = scales::percent,limits = c(0,1))+
  geom_text(aes(label = nb.sp), vjust = -0.2)+
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 0,size=10))+xlab('')+ylab("% of missing values")
getwd()
setwd("../output") ###  set the path 
ggsave(missper,file="15 miss percent (Fig. S6).PNG",height = 5,width = 8)
