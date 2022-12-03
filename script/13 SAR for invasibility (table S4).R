rm(list=ls());gc()
library(spdep)
library(ggplot2)
library(RColorBrewer)
library(cowplot) # plot_grid # get_legend()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT")
go=rgdal::readOGR('./fixed huc8/HUC8 fixed.shp') ## use for Moran's I 

brtest<-div_env[,-c(3,7)]
data<-brtest
apply(data,2,function(x) length(na.omit(x)))

# for 
sarest<-data[data$inva.nb>0,]
sarest$NPP2<-sarest$NPP^2
sarest$FSpe2<-sarest$FSpe^2
sarest$cd2<-sarest$cd^2
sarest$md2<-sarest$md^2
sarest$Area2<-sarest$Area^2
sarest$GDP2<-sarest$GDP^2

morI<-merge(go,sarest,by.x="HUC",by.y="huc8",all.x=F)
W_cont_el <-poly2nb(morI, queen=T,row.names = morI$HUC)
W_cont_el_mat<-nb2listw(W_cont_el,
                        style="W", zero.policy=T)
y<-as.data.frame(morI)
y<-data.frame(HUC=y$HUC)###name order of morI
sarest<-merge(y,sarest,by.x="HUC",by.y="huc8",sort=F)

r<-colnames(sarest)[-(1:2)]# all varibales
f<-colnames(sarest)[2]
h<-combn(1:18,18)# build formula

sarest[,3:20]<-scale(sarest[,3:20])

## sem model
imp1<-NULL## coefficient
r2moranI1<-NULL

i=1
x<-as.formula(paste(f[i],"~",paste(r[h[,1]],collapse = " + ")))
mod.sem <-spatialreg::errorsarlm(x, data=sarest, listw=W_cont_el_mat,zero.policy=T)  
sem<-summary(mod.sem,Nagelkerke=T)
sem.coef<-round(sem$Coef,4);sem.coef
sem.r2<-sem$NK;sem.r2
sem.aic<-AIC(mod.sem);sem.aic
res <- mod.sem$residuals
I<-moran.test(res,listw=W_cont_el_mat,zero.policy=T);I
print(sem.coef);print(sem.r2);print(sem.aic)
print(paste("Moran's I = ",round(I$estimate[1],4)))
print(paste("p =",round(I$p.value,3))) 
coef<-data.frame(var= rownames(sem.coef)[-1],var=f[i],sem.coef[-1,]) #for plot
imp1<-rbind(imp1,coef) # for plot
r2moranI1<-c(r2=round(sem.r2,3),moranI=round(I$estimate[1],4),pvalue=round(I$p.value,3))
In<-c("coef","SE","z_value","pvalue")
colnames(imp1)[3:6]<-In

setwd("../output") ###  set the path 
writexl::write_xlsx(list(imp=imp1,r2moranI=as.data.frame(r2moranI1)),"13 SAR 12+6 variables (Table S4).xlsx")

