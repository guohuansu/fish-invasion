rm(list=ls())
if(!require("geometry"))
{install.packages("geometry")}
library(geometry)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("1 trait_fill_859")
load("0 occurrence 1868 watersheds+ 859 species")
source("multidimFD.R")


## historical occur 
nat_oc<-occ[occ$Status=="native",]
spe_name<-names(nat_oc$Genus.species%>%table) ### 783 species
huc_name<-names(nat_oc$HUC_8%>%table)
mat_occ<-as.data.frame(unclass(table(nat_oc$HUC_8,nat_oc$Genus.species)))## change table to matrix
mat_occ<-mat_occ[,order(colnames(mat_occ),decreasing = F)]
mat_occ<-as.matrix(mat_occ)
## made trait data
trait<-trait_fill[trait_fill$Genus.species%in%colnames(mat_occ),]
trait<-trait[order(trait$Genus.species,decreasing = F),]
# pcoa for 783 native species
dat<-trait[,c(8:27)]
rownames(dat)<-trait$Genus.species
res.gower<-cluster::daisy(dat,type=list(asymm=c(18)))
res.pcoa<-ape::pcoa(res.gower)
pc<-res.pcoa$vectors[,1:5]
## compute alpha FD indices
FD_nat<-multidimFD(pc,mat_occ)
getwd()
save(FD_nat,file="2 FD_native_pcoa1-5(783spex1868wsd)")
save(mat_occ,file="2 occMat_bef_1868_783 species")
save(pc,file="2 pcoa1-5 based on 783 species")

## current occur with invasions
mat_occ_aft<-as.data.frame(unclass(table(occ$HUC_8,occ$Genus.species)))## change table to matrix
mat_occ_aft<-mat_occ_aft[,order(colnames(mat_occ_aft),decreasing = F)]
mat_occ_aft<-as.matrix(mat_occ_aft)
save(mat_occ_aft,file="2 occMat_aft_1868_859 species")

# pcoa for 859 native species
dat<-trait_fill[,c(8:27)]
rownames(dat)<-trait_fill$Genus.species
res.gower<-cluster::daisy(dat,type=list(asymm=c(18)))
res.pcoa<-ape::pcoa(res.gower)
pc_859<-res.pcoa$vectors[,1:5]
save(pc_859,file="2 pcoa1-5 based on 859 species")

