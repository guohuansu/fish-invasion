### distance between the native assemblage and the invasion assemblages 1873 basins
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 occurrence 1868 watersheds+ 859 species")
load("1 trait_fill_859")
load("2 pcoa1-5 based on 859 species")
load("2 FD_native_pcoa1-5(783spex1868wsd)")

table(occ$Status)
length(unique(occ$Genus.species))
# distance
huc<-sort(rownames(FD_nat))
ocnat<-occ[occ$Status=="native",]
ocest<-occ[occ$Status!="native",]

pc<-pc_859
distNN<-NULL
for(i in 1:1868)
{
  a<-apply(pc[rownames(pc)%in%ocnat[ocnat$HUC_8==huc[i],]$Genus.species,],2,mean)# native centroid
  
  x<-as.data.frame(pc[rownames(pc)%in%ocest[ocest$HUC_8==huc[i],]$Genus.species,])
  if(dim(x)[2]==1)
    b<-apply(t(x),2,mean) else b<-apply(x,2,mean) # established centroid

  distNN<-rbind(distNN,c(huc[i],dist(rbind(a,b))))#, dist(rbind(a,c)),dist(rbind(a,d)),dist(rbind(c,d))
}

colnames(distNN)<-c("huc8","nat_est") #
distNN<-as.data.frame(distNN)
save(distNN,file = "6 centroid distNN")
