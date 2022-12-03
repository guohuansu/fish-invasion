## 
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(cowplot)#plot_grid
#remotes::install_github("caijun/ggcorrplot2")
library(ggcorrplot2)
library(psych)# corr.test
library(factoextra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("0 occurrence 1868 watersheds+ 859 species")
load("1 trait_fill_859")
load("0 traitName")

trait<-trait_fill
nat<-occ[occ$Status=="native",]
non<-occ[occ$Status!="native",]
## frequent
## 1868 watersheds: translocated 233; exotic 67.
x<-tapply(occ$Genus.species,occ$Status,table)
exo<-as.data.frame(x$exotic);colnames(exo)<-c("Genus.species","freq")
tra<-as.data.frame(x$translocated);colnames(tra)<-c("Genus.species","freq")
exo<-left_join(exo,trait,by="Genus.species")
tra<-left_join(tra,trait,by="Genus.species")
exo$Type<-"exotic"
tra$Type<-"translocated"
tfreq<-rbind(tra,exo)
tfreq$Type<-factor(tfreq$Type,levels=c("translocated","exotic"))

trait<-tfreq[,9:28]
trait<-trait[,c("MBl" ,"LGt" ,"MTg" ,"FCt" ,"BEl", "REs", "OGp" ,"RMl" ,
                "EVp" ,"BLs" ,"PFv" ,"PFs" ,"CPt" ,"TLl" ,"EHl","PCr","DBt","TRg" ,"TMx" ,"TMn")]
trait$EHl<-as.numeric(trait$EHl)
trait$PCr<-as.numeric(trait$PCr)
trait$DBt<-as.numeric(trait$DBt)

cor2 <- corr.test(trait,method ="pearson")
corr <- cor2$r
p.mat <- cor2$p
x<-ggcorrplot.mixed(corr, upper = "ellipse", lower = "number",p.mat = p.mat, 
                    insig = "label_sig", sig.lvl = c(0.05, 0.01, 0.001))
x

setwd("../output") ###  set the path 
ggsave("4 trait correlation (pearson) (Fig. S8A).png", x, width = 9, height = 9,dpi=400)

### pca for high correlation traits
# group 1 TRg, TMn, TMx

res1<-FactoMineR::PCA(tfreq[,c("TRg", "TMn", "TMx")], scale.unit = T,graph = F)
con<-t(round(res1$eig[1:3,],3)) # 1st axis 75.0%
dat<-round(res1$var$contrib,3) #traits contribute to the first 4 axes
p1<-fviz_pca_var(res1,title="Temperature-related PCA",
                 col.var = "contrib", # Color by contributions to the PC
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE)     # Avoid text overlapping)

pc1<-res1$ind$coord # use the first axis

#group2 "LGt", "MTg", "MBl"
res2<-FactoMineR::PCA(tfreq[,c("LGt", "MTg", "MBl")], scale.unit = T,graph = F)
con<-t(round(res2$eig[1:3,],4)) # 1st axis 90.2%
dat<-round(res2$var$contrib,3) #traits contribute to the first 3 axes

p2<-fviz_pca_var(res2,title="Size-related PCA",
                 col.var = "contrib", # Color by contributions to the PC
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE)     # Avoid text overlapping)
pc2<-res2$ind$coord

y<-plot_grid(p2,p1, align = 'h', nrow = 1)
ggsave("4 PCA of patial traits (Fig. S8 B-C).png", y, width = 9, height = 4.5,dpi=400)

# merge
newdat<-data.frame(tfreq[,c(2,9:29)],Temp_PCA1=pc1[,1],Size_PCA1=pc2[,1])
brt<-data.frame(Genus.species=tfreq$Genus.species,newdat[,-c(2,12,14,16,17,18)])
levels(brt$PCr)<-1:5
getwd()
setwd("../input")
huse<-readxl::read_xlsx("3 human index of 300 nonnative species checked.xlsx",sheet = 1)
colnames(huse)[3]<-"HUI"
brt<-left_join(brt,huse[-2],by="Genus.species")
save(brt,file="4 data for BRT invasiveness")
#writexl::write_xlsx(tfreq,"4 invasiveness-traits (pca).xlsx")


