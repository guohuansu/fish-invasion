## make a summary table of abiotic variables Table S3
rm(list=ls());gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT" )

abio<-div_env[,c("Area","NPP","DOF","GDP","HFI")]
summary(abio)
ind<-function(x){c(mean(x),sd(x),min(x),median(x),max(x))}

su<-apply(abio,2,ind)
rownames(su)<-c("Mean","SD","Minimum","Median","Maximum")
su<-data.frame(Variables=colnames(su), t(round(su,2)))
setwd("../output")
writexl::write_xlsx(su,"14 summary table of abiotic variables (Table S2).xlsx")
