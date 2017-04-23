
setwd("/Users/eansar2/Desktop/Geostatistics/TakeHome")
pdf("Estimation.pdf")
vio.month=c(seq(10,1000,length.out=7),50,30)
xlim=range(vio.month)
ylim=range(simulation.res[vio.month,])
plot(1,1,xlim=xlim,ylim=ylim,axes=T,cex=0.1,xlab="month",
     ylab="million $",
     main=paste("revenue vs. month for the field (",sim.number," sim.)"))
for (plot.cont in vio.month){
  
  vioplot(simulation.res[plot.cont,],add=T,
          col=adjustcolor("orange",alpha=1/5),border="orange",
          rectCol="lightblue1",colMed="orange",at=plot.cont,wex=140)
  
}
lines(1:nrow(simulation.res),rowMeans(simulation.res),col='red')


dev.off()

save(list=ls(),file="workspace2")