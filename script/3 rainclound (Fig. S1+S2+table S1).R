#### rain cloud for the 20 traits of the 3 types species
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
load("0 traitName")
load("1 trait_fill_859")
source("R_rainclouds.R") 

tapply(occ$Genus.species,occ$Status,function(x)length(table(x)))

# exo: spcies have successful records 67
# tra: spcies have successful records 233
# nat: native had never been introduced 783 (559 pure)

trait<-trait_fill[,-(3:7)]# 
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
dat$type%>%table # exotic 67, tra 233, native only =559
colnames(dat)[3:22]<-traitName[,3]

apply(dat[,3:19],2,function(x) tapply(x,dat$type,mean))
apply(dat[,3:19],2,function(x) tapply(x,dat$type,median))

col<-c("black","blue","red")[3:1]

rain1 <- function(dat,yy,ylab="",axis.text.y = element_blank(),leg = "none") 
{
  ggplot(dat, aes(Class, yy)) + theme_cowplot()+
    geom_flat_violin(aes(col = type),position = position_nudge(x = .15, y = 0),width=1.2, adjust = 1.5, trim = FALSE,fill=NA, alpha = .8)+ 
    #geom_point(aes(x = as.numeric(realm)-.15, y = yy, colour = type
    #),position = position_jitter(width = .05), size = 1, shape = 20)+ 
    geom_boxplot(aes(x = Class, y = yy, fill = type),outlier.shape 
                 = NA, alpha = 0.8, width = .2, colour = "black",show.legend =F)+ 
    #scale_colour_manual(values=c("black","purple"))+ 
    #scale_fill_manual(guide = guide_legend())+ 
    xlab("")+ylab(ylab)+guides(fill=guide_legend(reverse=T))+
    scale_x_discrete(expand=c(0,0.2))+
    scale_color_manual(values=col)+
    scale_fill_manual(values=col)+
    theme( legend.position = leg,legend.title=element_blank(),panel.grid =element_blank(), 
           axis.text.y = axis.text.y, axis.title.y = element_text( size=15),
           axis.ticks.y=element_blank(),
           plot.margin=margin(0.2, 0, 0.4, 0, "cm"),legend.spacing.x = unit(5,"mm"),legend.text = element_text(size=20))+coord_flip()
}

### each morhpoTrait
a1=rain1(dat,log(dat$`log(MBl)`),ylab="log(MBl)")# maximum body length
a2=rain1(dat,log(dat$`log(BEl)`),ylab="log(BEl)") # body elongation
a3=rain1(dat,dat$REs,ylab="REs")# relative eye size
a4=rain1(dat,dat$OGp,ylab="OGp")# oral gape position
a5=rain1(dat[dat$RMl<2,],dat[dat$RMl<2,]$RMl,ylab="RMl") # maxillary length
a6=rain1(dat,dat$EVp,ylab="VEp") # eye vertical position
a7=rain1(dat,dat$BLs,ylab="BLs")# body leteral shape
a8=rain1(dat,dat$PFv,ylab="PFv") #pectoral fin vertical position
a9=rain1(dat,dat$PFs,ylab="PFs")# pectoral fin size
a10=rain1(dat,dat$CPt,ylab="CPt")# caudal peduncle  throttling
a0=rain1(dat,dat$CPt,leg="top")
mylegend<-get_legend(a0+ theme(legend.position="bottom",legend.justification = "center"))
fig<-plot_grid(a1,a2,a3,a4,a5,
               a6,a7,a8,a9,a10,nrow = 5)

#p3 <- grid.arrange(fig,mylegend, nrow=2,heights=c(10, 1))
#ggarrange(a1,a2,a3,a4,a5, a6,a7,a8,a9,a10,nrow = 5, ncol=2,common.legend = TRUE, legend="bottom")
p4=plot_grid( mylegend,fig,  ncol = 1, rel_heights = c(.1,1))

setwd("../output")
ggsave(p4,file="3 raincloud morTrait10_67exo_233tra_559nat (Fig. S1).PNG",width = 8, height = 10)

b1<-rain1(dat,log(dat$`log(LGt)`),ylab="log(LGt)")# longevity
b2<-rain1(dat,log(dat$`log(FCt)`),ylab="log(FCt)")# fecundity
b3<-rain1(dat,log(dat$`log(MTg)`),ylab="log(MTg)")# mature age
b4<-rain1(dat,dat$TLl,ylab="TLl")# trophic level
b5<-rain1(dat,dat$TRg,ylab="TRg") # temperature range
b6<-rain1(dat,dat$TMx,ylab="TMx") # temp maximal
b7<-rain1(dat,dat$TMn,ylab="TMn") # temp minimal

# funciton to make percentage stacked bar chart
perplot<-function(x,ecoT=NULL)
{
  per<-rbind(data.frame(type=rownames(x),value=x[,2]/(x[,1]+x[,2]),Status="yes"),
             data.frame(type=rownames(x),value=x[,1]/(x[,1]+x[,2]),Status="no"))
  per$type<-factor(per$type,levels=c("native","translocated","exotic"))
  per$Status<-factor(per$Status,levels=c("no","yes"))
  p<-ggplot(per)+geom_bar(aes(x=type,y=value,group=Status,fill=type,alpha=Status),
                          position="stack", stat="identity",color="black")+
    guides( fill = FALSE)+ # remove fill legend
    scale_alpha_manual(values=c(0.4,0.8),name=ecoT,labels=c("No","Yes"))+
    scale_fill_manual(values=col[3:1],name="Group")+
    scale_y_continuous(labels=percent)+xlab("")+ylab("")+theme_bw()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.x=element_text(size=12,face="bold"),
      axis.text.y=element_text(size=14))
  return(p)
}

# b8 ## Euryaline
eury<-table(dat$type,dat$EHl)
b8<-perplot(eury,ecoT="Euryhaline")
b8

# b9 ## parents care
x<-table(dat$type,dat$PCr)
x<-data.frame(x[,1]+x[,2],x[,3]+x[,4],x[,5])
I<-c("Non-guarders","Guarders","Bearers")
colnames(x)<-I
x<-round(x/apply(x,1,sum),3)
per<-rbind(data.frame(type=rownames(x),value=x[,1],Status=I[1]),
           data.frame(type=rownames(x),value=x[,2],Status=I[2]),
           data.frame(type=rownames(x),value=x[,3],Status=I[3]))
per$type<-factor(per$type,levels=c("native","translocated","exotic"))
per$Status<-factor(per$Status,ordered = T,levels=I)
b9<-ggplot(per)+geom_bar(aes(x=type,y=value,group=Status,fill=type,alpha=Status),
                         position="stack", stat="identity",color="black")+
  scale_alpha_manual(values=c(0.3,0.6,0.9),name="ParentalCare")+
  scale_fill_manual(values=col[3:1],name="Group")+
  guides( fill = FALSE)+ # remove fill legend
  scale_y_continuous(labels=percent)+xlab("")+ylab("")+theme_bw()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.x=element_text(size=12,face="bold"),
    axis.text.y=element_text(size=14))
b9
#b10
# interval 1:3, 4:6, 7:9 
x<-table(dat$type,dat$DBt)
x<-data.frame(x[,1]+x[,2]+x[,3],x[,4]+x[,5]+x[,6],x[,7]+x[,8]+x[,9])
I<-c("1-3","4-6","7-9")
colnames(x)<-I
x<-round(x/apply(x,1,sum),3)
per<-rbind(data.frame(type=rownames(x),value=x[,1],Status=I[1]),
           data.frame(type=rownames(x),value=x[,2],Status=I[2]),
           data.frame(type=rownames(x),value=x[,3],Status=I[3]))
per$type<-factor(per$type,levels=c("native","translocated","exotic"))
per$Status<-factor(per$Status,ordered = T,levels=I)
b10<-ggplot(per)+geom_bar(aes(x=type,y=value,group=Status,fill=type,alpha=Status),
                          position="stack", stat="identity",color="black")+
  scale_alpha_manual(values=c(0.3,0.6,0.9),name="DietBreadth")+
  scale_fill_manual(values=col[3:1],name="Group")+
  guides( fill = FALSE)+ # remove fill legend
  scale_y_continuous(labels=percent)+xlab("")+ylab("")+theme_bw()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.x=element_text(size=12,face="bold"),
    axis.text.y=element_text(size=14))
b10

mylegend<-get_legend(b1+ theme(legend.position="bottom",legend.justification = "center"))
fig<-plot_grid(b1,b2,b3,b4,b5,
               b6,b7,b8,b9,b10,nrow = 5)

p5=plot_grid( mylegend,fig,  ncol = 1, rel_heights = c(.1,1))
ggsave(p5,file="3 raincloud ecoTrait10_67exo_233tra_559nat (Fig. S2).PNG",width = 8, height = 10)

################# ks test for the trait between 3 groups
library(PMCMRplus)###nemenyi.test
library(pgirmess)###kruscalmc
dat$`log(MBl)`<-log(dat$`log(MBl)`)
dat$`log(BEl)`<-log(dat$`log(BEl)`)
dat$`log(LGt)`<-log(dat$`log(LGt)`)
dat$`log(FCt)`<-log(dat$`log(FCt)`)
dat$`log(MTg)`<-log(dat$`log(MTg)`)
dat$type<-factor(dat$type,levels=c("native","translocated","exotic"))

name<-c("na-tr KS p","na-ex KS p","tr-ex KS p","na-tr KW p","na-ex KW p","tr-ex KW p")# potential native(PN) and potential threatened(PT)
test.res<-matrix(NA,nrow=20,ncol=6,dimnames = list(colnames(dat)[3:22],name)) ###matrix for the species number of each type
for(i in 3:19)
{
  x<-ks.test(dat[,i][dat$type=="native"],dat[,i][dat$type=="translocated"])
  y<-ks.test(dat[,i][dat$type=="native"],dat[,i][dat$type=="exotic"])
  z<-ks.test(dat[,i][dat$type=="translocated"],dat[,i][dat$type=="exotic"])
  j=i-2
  test.res[j,1]<-x$p.value
  test.res[j,2]<-y$p.value
  test.res[j,3]<-z$p.value
  
  x<-kwAllPairsNemenyiTest(dat[,i]~type,data=dat,dist="Tukey")###
  test.res[j,4:6]<-c(x$p.value[,1],x$p.value[2,2])
  
}
test.res<-data.frame(traitName=rownames(test.res),round(test.res,4))

## chisuqred test for the 3 categorical traits
library(rcompanion)
## Euryhaline and parentalCare
eur<-table(dat$type,dat$EHl)
par<-table(dat$type,dat$PCr)
eurT<-pairwiseNominalIndependence(eur,fisher=T,gtest = F,chisq = T,method="fdr")
parT<-pairwiseNominalIndependence(par,fisher=T,gtest = F,chisq = T,method="fdr")
## dietBreadth
x<-table(dat$type,dat$DBt)
x<-data.frame(x[,1]+x[,2]+x[,3],x[,4]+x[,5]+x[,6],x[,7]+x[,8]+x[,9])
I<-c("1-3","4-6","7-9")
colnames(x)<-I
x<-as.matrix(x)
dieT<-pairwiseNominalIndependence(x,fisher=T,gtest = F,chisq = T,method="fdr")

test.res[18:20,-1]<- round(rbind(c(eurT[,2],eurT[,4]),
                                 c(parT[,2],parT[,4]),
                                 c(dieT[,2],dieT[,4])),4)
test.res$traitName<-traitName$newName
colnames(test.res)[2:4]<-c("Native-translocated","Native-exotic","Translocated-exotic")
writexl::write_xlsx(test.res[,1:4],"3 tests of 20traits between the 3 groups (Table S1).xlsx")

