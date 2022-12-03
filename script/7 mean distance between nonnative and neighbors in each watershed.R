rm(list=ls());gc()
library(dplyr)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 occurrence 1868 watersheds+ 859 species")
load("1 trait_fill_859")
load("0 traitName")
load("2 pcoa1-5 based on 859 species")
load("2 FD_native_pcoa1-5(783spex1868wsd)")

#length(unique(occ$Genus.species))
pc<-pc_859
trait<-data.frame(Genus.species=rownames(pc),pc)
trait<-trait[trait$Genus.species%in%occ$Genus.species,]## 861 all species in the 1871 watersheds
nat<-occ[occ$Status=="native",]
non<-occ[occ$Status!="native",]
tapply(occ$Genus.species,occ$Status,function(x)length(table(x)))

## 1868 watersheds: translocated 233; exotic 67.
non$HUC_8<-as.character(non$HUC_8)
speName_nat<-as.data.frame(tapply(nat$Genus.species,nat$HUC_8,function(x)x))## species names in each watershed
speName_non<-as.data.frame(tapply(non$Genus.species,non$HUC_8,function(x)x))## species names in each watershed
huc8<-names(table(occ$HUC_8))# watershed code huc8
non<-non[,c(1,2,4)]
distn<-function(y)
{
  d<-data.frame(trait[,2:(1+y)])
  rownames(d)<-trait$Genus.species
  Ud<-as.matrix(dist(d))
  diag(Ud)=NA
  dat2<-NULL
  for(j in 1:1873)
  {
    # per shed native species name
    shedSpeName<-unlist(speName_nat[rownames(speName_nat)==huc8[j],])
    # per shed nonnative species name
    shedNonName<-unlist(speName_non[rownames(speName_non)==huc8[j],])
    if(length(shedNonName)>0){
      for(i in 1:length(shedNonName))
      {
        id1<-which(colnames(Ud)%in%c(shedSpeName))
        id2<-which(colnames(Ud)%in%c(shedSpeName,shedNonName[i]))
        shedUd1<-Ud[id1,id1] # only native species distance matrix
        shedUd2<-Ud[id2,id2] # matrix include nonnative species
        m<-min(shedUd1,na.rm = T)
        x<-sort(shedUd2[which(colnames(shedUd2)%in%shedNonName[i]),])
        distFNSN<-shedUd1[names(x)[1],names(x)[2]]
        dat2<-rbind(dat2,data.frame(HUC8=huc8[j],Name=shedNonName[i],FNN=names(x)[1],SNN=names(x)[2],distFN=x[1],distSN=x[2],distFNSN=distFNSN,distmin=m))
      }
    }
  }
  # dat2$sec_first<-dat2$secNeighb/dat2$firstNeighb ## ratio of the second to the first distance
  dat2$probRep<-1-dat2$distFN/dat2$distSN ## ratio of the second to the first distance
  dat2$cos<-(dat2$distFN^2+dat2$distSN^2-dat2$distFNSN^2)/(2*dat2$distFN*dat2$distSN)
  dat<-left_join(non,dat2,by=c("Genus.species"="Name","HUC_8"="HUC8"))
  return(dat)
}

dist5d<-distn(5) #PCoa1-5
a<-tapply(dist5d$distFN,dist5d$HUC_8,mean)
a<-data.frame(huc8=rownames(a),mdistNEs=a)

mdist<-data.frame(huc8=rownames(FD_nat))
mdist<-left_join(mdist,a,by="huc8")
save(mdist,file="7 mean distance between nonnative and neighbors in each watershed")
