rm(list=ls());gc()
library(ggBRT)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ###  set the path 
setwd("../input")
load("8 var for BRT")
load("9 opest")

brtest<-div_env[,-c(3,7)]
set.seed(2021)
gbm <- gbm.step(data=brtest, gbm.x = 3:14, gbm.y = 2,
                tree.complexity = opest[1,2], family = "poisson",#poisson
                learning.rate = opest[1,3], bag.fraction = opest[1,1],plot.main = F)

# interaction
library(circlize)
find.int <- gbm.interactions(gbm)
x<-find.int$interactions
find.int$rank.list
setwd("../output")
png("12 invasibility interaction (Fig. S4 A).png",width=3000,height=3000,res=600)
set.seed(2022)
chordDiagram(t(x), annotationTrack = "grid", preAllocateTracks = 1, link.visible = t(x) >= 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = T, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
dev.off()

#plot 3D fig
png("12 invasibility interaction 3D (Fig. S4 B-D).png",width=6000,height=2000,res=600)
par(mar = c(2, 2, 2, 0), mfrow = c(1, 3))
gbm.perspec(gbm, 11, 4, x.range = c(0, 0.21), y.range = c(0.18,0.55),z.range = c(0,20))
gbm.perspec(gbm, 11, 6, x.range = c(0, 0.21), y.range = c(0,15000),z.range = c(0,20))
gbm.perspec(gbm, 11, 5, x.range = c(0, 0.21), y.range = c(0.1,0.4),z.range = c(0,20))
dev.off()

