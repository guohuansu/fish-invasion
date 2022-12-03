### cleaned BRT invasibility ~ div env
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(ggcorrplot2)
library(psych)# corr.test
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 occurrence 1868 watersheds+ 859 species")
load("0 environmental varibles")
load("2 FD_native_pcoa1-5(783spex1868wsd)")
load("6 centroid distNN")
load("7 mean distance between nonnative and neighbors in each watershed")

distNN$huc8<-as.numeric(distNN$huc8)
distNN$nat_est<-as.numeric(distNN$nat_est)
mdist$huc8<-as.numeric(mdist$huc8)

#find the number of invaisve spcecies in each watersheds
nb<-data.frame(unclass(table(occ$HUC_8,occ$Status)))
nb$inva<-nb$exotic+nb$translocated
inva<-data.frame(huc8=as.numeric(rownames(nb)),inva.nb=nb$inva)

#div
fd<-FD_nat[,c(1,23:28)]
fd<-data.frame(huc8=as.numeric(rownames(fd)),fd)
div_env<-left_join(fd,env,by=c("huc8"))
div_env<-left_join(div_env,distNN,by="huc8")
div_env<-left_join(div_env,mdist,by="huc8")
colnames(div_env)[c(2,14,15)]<-c("NSR","cd","md")

co<-div_env
cor2 <- corr.test(co[,-1],method ="pearson")
corr <- cor2$r
p.mat <- cor2$p
x<-ggcorrplot.mixed(corr, upper = "ellipse", lower = "number",p.mat = p.mat, 
                    insig = "label_sig", sig.lvl = c(0.05, 0.01, 0.001))
x
setwd("../output") ###  set the path 
ggsave("8 predictor correlation (pearson) (Fig. S10).png", x, width = 7, height = 7,dpi=400)

setwd("../input") ###  set the path 
div_env<-inner_join(inva,co,by="huc8")
save(div_env,file="8 var for BRT")




