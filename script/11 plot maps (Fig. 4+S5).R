rm(list=ls())
if(!require("maps")) 
{install.packages("maps")} 
library(maps)

if(!require("ggplot2")) 
{install.packages("ggplot2")} 
library(ggplot2)

if(!require("RColorBrewer")) 
{install.packages("RColorBrewer")} 
library(RColorBrewer)

if(!require("cowplot"))  # get_legend(); plot_grid()
{install.packages("cowplot")} 
library(cowplot)
library(rgdal)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT")
load("0 coord_huc8")

coord<-coord_huc8
coord<-coord[coord$X1>(-150),]

bef<-div_env
bef$lev_sp<-cut(bef$NSR,quantile(bef$NSR,seq(0,1,0.1)),right=F,include.lowest = T)
bef$lev_est<-cut(bef$inva.nb,c(0:4,quantile(bef$inva.nb[which(bef$inva.nb>4)],seq(0,1,0.2))),right=F,include.lowest = T)

B<-inner_join(coord,bef,by=c("Bains"="huc8"))

USA<-map_data("usa") 
G<-ggplot(USA,aes(long,lat,group=group))+geom_polygon(fill="grey",colour="grey")+
  theme(panel.grid = element_blank(),axis.line  = element_blank(),
        panel.background = element_blank(),
        
        legend.position = "right",
        legend.text  = element_text(size=18),
        
        axis.text = element_blank(),
        
        plot.title = element_text(size=20,hjust =0.5),
        
        axis.ticks = element_blank(),
        
        axis.title = element_blank())+coord_map("bonne", lat0 = 50)
G
mycol1<-c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')[10:1]
library("viridis")  
library(RColorBrewer)
mycol2<-c("white",brewer.pal(n = 9, name = "YlOrRd"))
nat_sp<-G+geom_polygon(data=B,aes(X1,X2,group=Bains2,fill=lev_sp))+
  scale_fill_manual(name= "",values = mycol1,
                    labels=c("[6,8]","[9,11]","[12,15]","[16,21]","[22,28]","[29,39]",
                             "[40,52]","[53,64]","[65,881]","[82,156]"))+
  ggtitle("Native species number")
nat_sp
non_sp<-G+geom_polygon(data=B,aes(X1,X2,group=Bains2,fill=lev_est))+
  scale_fill_manual(name= "",values = mycol1,
                    labels=c("[0]","[1]","[2]","[3]","[4]","[5]",
                             "[6]","[7,9]","[10,12]","[13,35]"))+
  ggtitle("Non-native species number")
non_sp

setwd("../output")
ggsave("11 USA maps non-native species number (Fig. 4).png", non_sp, width = 10, height = 4,dpi=600)

Fig<-plot_grid(nat_sp,non_sp, align = 'v', nrow = 2)+
  draw_plot_label(label = LETTERS[1:2], size = 18,x = 0.1, y = c(1,0.5))

ggsave("11 USA maps species number  native+invasive (Fig.S5).png", Fig, width =10, height = 8,dpi=600)

getwd()

