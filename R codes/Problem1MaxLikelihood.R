rm(list=ls())
if (!is.null(dev.list())) dev.off()

#Reading Kapur Data
chap.data.dir = '/Users/eansar2/Desktop/Geostatistics/Chapter5/Data'
chap.code.dir = '/Users/eansar2/Desktop/Geostatistics/Chapter5/Code'
misc.dir      = '/Users/eansar2/Desktop/Geostatistics/Misc'
load(file.path(chap.data.dir,'kapur2000.Rdata'))

#Counting
facies     = kapur["facies"]
tab.facies = table(facies)
N.facies   = length(tab.facies)
tot.obs    = sum(tab.facies)
p.facies   = tab.facies/tot.obs


#Plotting Prioros for each facies using Beta distribution
require(lattice)
require(boot)
require(nnet)
require(RColorBrewer)
require(ggplot2)
line.colors <- brewer.pal(N.facies,"Dark2")

#Bootstrapping 

source(file.path(chap.code.dir,'fit-kapur-glm-modified.R'))
N.Boot = 100

# Bootstrapping using boot function

ML.facies <- function(formula, data, indices){
kapur.glm       =  multinom(formula,data=data[indices,])
pred.glm        =  fitted(kapur.glm)
pred.glm        = sortPredByLevels(pred.glm)
most.likely.glm = apply(pred.glm,1,which.max)
return(most.likely.glm)
}


form <- facies ~ caliper + ind.deep + gamma + r.deep + density
#sample.idx = sample(1:tot.obs,tot.obs,replace=FALSE) #change tot.obs
boot.result <- boot(formula=form, statistic=ML.facies,
                   data=scaleKapur(kapur),R=N.Boot)

for (i in 1:N.facies){
       d.facies <-  density(boot.result$t[,i])
       x        <-  d.facies$x
       y        <-  d.facies$y
}


setwd("/Users/eansar2/Desktop/Geostatistics/TakeHome")

pdf("MLvsDepth.pdf")


dev.off()



save(list=ls(),file="workspaceML")