rm(list=ls())
if(!require("ggplot2")) 
{install.packages("ggplot2")} 
library(ggplot2)
if(!require("RColorBrewer")) 
{install.packages("RColorBrewer")} 
library(RColorBrewer)
if(!require("cowplot"))  # get_legend(); plot_grid()
{install.packages("cowplot")} 
library(cowplot)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")

load("0 occurrence 1868 watersheds+ 859 species")
load("0 traitName")
load("1 trait_fill_859")
load("2 occMat_bef_1868_783 species")
load("2 occMat_aft_1868_859 species")
load("2 pcoa1-5 based on 859 species")
load("8 var for BRT")
shp=rgdal::readOGR('./fixed huc8/HUC8 fixed.shp') 

trait<-trait_fill
#colnames(trait)[8:27]<-traitName$newName
trait<-trait[,c(1,8:27)]
pc<-data.frame(Genus.species=rownames(pc_859),pc_859)
tpc<-inner_join(pc,trait,by="Genus.species")
## find ideal example
# high invasibility, low cd, high md; vice versa
#colnames(div_env)
data<-div_env[,c("huc8","NSR","inva.nb","cd","md",
                 "FRic","FDiv","FEve","FDis","FSpe","FOri")]
data<-na.omit(data)

range(data$cd)
hist(data$cd)
summary(data$cd)
range(data$md)
hist(data$md)
summary(data$md)
cor.test(data$inva.nb,data$cd)

# plot

huc_name<-as.data.frame(shp)
us_sf<-fortify(shp,region = "HUC")
us_sf<-us_sf[us_sf$long >(-150),]
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

##select  
f<-c("2020004","18050002", "10130102", "17020010", "4020203",  "3090103", "11140306", "12100202")
mycol1<-c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')[10:1]
G+geom_polygon(data=us_sf[us_sf$id%in%f,],aes(long,lat,group=group),colour="black")#+

mapplot<-function(mp)
{
  if(length(mp)==1)
  {est<-div_env[div_env$huc8==mp,"inva.nb"]
  if(est<2) col=mycol1[2] else 
    if(est<3) col=mycol1[3] else
      if(est<4) col=mycol1[4] else
        if(est<5) col=mycol1[5] else 
          if(est<6) col=mycol1[6] else
            if(est<7) col=mycol1[7] else
              if(est<10) col=mycol1[8] else
                if(est<13) col=mycol1[9] else
                  col=mycol1[10]
                xy<-us_sf[us_sf$id%in%mp,]
                ro<-(range(xy$long)[2]- range(xy$long)[1])/(range(xy$lat)[2]-range(xy$lat)[1])
                
                ggplot()+geom_polygon(data=us_sf[us_sf$id%in%mp,],aes(long,lat,group=group),colour="black",fill=col)+
                  #theme_bw()+
                  theme(panel.grid = element_blank(),axis.text = element_blank(),axis.line  = element_blank(),
                        axis.ticks = element_blank(),axis.title = element_blank(),
                        plot.background = element_rect(fill = "transparent", color = NA), 
                        panel.background = element_rect(fill = "transparent"),
                        plot.margin = unit(c(0,0,0,0),"mm"),
                        plot.title = element_text(hjust =0.5))#+coord_map()
  }else
    
    if(length(mp)>1)
    {
      est0<-div_env[div_env$huc8%in%mp,c("huc8","inva.nb")]
      est0<-merge(data.frame(huc8=as.numeric(mp)),est0,by.x="huc8",by.y = "huc8",sort=F)
      coln<-NULL
      for(est in est0$inva.nb)
      {
        if(est<2) col=mycol1[2] else 
          if(est<3) col=mycol1[3] else
            if(est<4) col=mycol1[4] else
              if(est<5) col=mycol1[5] else 
                if(est<6) col=mycol1[6] else
                  if(est<7) col=mycol1[7] else
                    if(est<10) col=mycol1[8] else
                      if(est<13) col=mycol1[9] else
                        col=mycol1[10]
                      coln<-c(coln,col)
      }  
      
      data=us_sf[us_sf$id%in%mp,]
      data$id<-factor(data$id,levels = mp)
      G+geom_polygon(data=data,aes(long,lat,group=group,fill=id))+
        scale_fill_manual(values=coln)+
        theme(panel.grid = element_blank(),axis.line  = element_blank(),axis.text = element_blank(),
              axis.ticks = element_blank(),axis.title = element_blank(),
              legend.position = "none",
              panel.background = element_blank(),plot.title = element_text(hjust =0.5))#+coord_map()
    }
}
m1<-mapplot(f[1])
m2<-mapplot(f[2])
m3<-mapplot(f[3])
m4<-mapplot(f[4])
m5<-mapplot(f[5])
m6<-mapplot(f[6])
m7<-mapplot(f[7])
m8<-mapplot(f[8])
m0<-mapplot(f)


labels=c("[0]","[1]","[2]","[3]","[4]","[5]",
         "[6]","[7,9]","[10,12]","[13,35]")
empplot<-function(emp,name=cat_name)
{
  sw<-data.frame(Genus.species=colnames(mat_occ_aft)[which(mat_occ_aft[rownames(mat_occ_aft)==emp,]>0)],type="nonnative")
  nat_sw<-colnames(mat_occ)[which(mat_occ[rownames(mat_occ)==emp,]>0)]# native species name
  sw$type[sw$Genus.species%in%nat_sw]<-"native"
  # find species in the pcoa data
  sw_trait<-tpc[tpc$Genus.species%in%sw$Genus.species,]
  sw_trait<-inner_join(sw,sw_trait,by="Genus.species")
  ### add native and non-native number,cd md
  nat<-div_env[div_env$huc8==emp,"NSR"];est<-div_env[div_env$huc8==emp,"inva.nb"]
  cd<-round(div_env[div_env$huc8==emp,"cd"],3);md<-round(div_env[div_env$huc8==emp,"md"],3)
  ### catchment name
  cat_name<-huc_name[huc_name$HUC==emp,"CAT_NAME"]
  x=log(sw_trait$MBl);y=log(sw_trait$BEl)
  xmax=range(x)[2];ymin=range(y)[1]
  ggplot(data=sw_trait,aes(x,y,group=type,fill=type))+geom_point(size=2,shape=21,col="darkblue",alpha=0.7)+theme_bw()+
    scale_fill_manual(values=c("#4472C4","#FF0000"))+#   "#2F528F"
    labs(title=paste(name,"\nnative:",nat,"; nonnative:",est,"\ncd:",cd,"; md:",md))+
    xlab("")+ylab("")+
    theme(legend.position ="none",axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5,size=16))#+#+coord_equal()
    #patchwork::inset_element(m1,left = 0.88, right = 1, bottom =0.8 ,top = 1)
}

f1<-empplot(f[1])+patchwork::inset_element(m1,left = 0.8, right = 1, bottom =0 ,top = 0.18)
f1
f2<-empplot(f[2])+ patchwork::inset_element(m2,left = 0.9, right = 1, bottom =0 ,top = 0.22)
f2
f3<-empplot(f[3],name="Upper Lake Oahe. Dakota.")+ patchwork::inset_element(m3,left = 0.85, right = 1, bottom =0 ,top = 0.23)
f3
f4<-empplot(f[4])+ patchwork::inset_element(m4,left = 0.88, right = 1, bottom =0 ,top = 0.28)
f4
f5<-empplot(f[5])+ patchwork::inset_element(m5,left = 0.8, right = 1, bottom =0 ,top = 0.15)
f5
f6<-empplot(f[6])+ patchwork::inset_element(m6,left = 0.85, right = 1, bottom =0 ,top = 0.16)
f6
f7<-empplot(f[7])+ patchwork::inset_element(m7,left = 0.85, right = 1, bottom =0 ,top = 0.21)
f7
f8<-empplot(f[8])+ patchwork::inset_element(m8,left = 0.83, right = 1, bottom =0 ,top = 0.18)
f8
#rgb(47,82,143, maxColorValue = 255)

p<-plot_grid(f1,f2,f3,f4,f5,f6,f7,f8,align=("hv"),nrow = 4)
getwd()
setwd("../output")
ggsave("16 site example (Fig. 4).png",p, width = 8, height = 15,dpi=300)
