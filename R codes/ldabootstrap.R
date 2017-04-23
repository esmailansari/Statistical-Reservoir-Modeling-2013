rm(list=ls())
#if (!is.null(dev.list())) dev.off()

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
facies.name = names(tab.facies)
#Note:
# a (as vector for all) is equal to tab.facies+1 
# b (as vector for all) is equal to tot.obs-tab.facies+1
#fun.beta = function(x,tab.facies,tot.obs){
#  dbeta(x,tab.facies+1,tot.obs-tab.facies+1)}

#Plotting Prioros for each facies using Beta distribution
require(lattice)
require(boot)
require(nnet)
require(RColorBrewer)
require(ggplot2)
line.colors <- brewer.pal(N.facies,"Dark2")

#for (i in 1:N.facies){

#    curve(fun.beta(x,tab.facies[i],tot.obs)/
#          sum(fun.beta(x,tab.facies[i],tot.obs)),
#          lty=1,lwd=4,xlab=paste('Proportion of Facies ',
#                                 names(tab.facies[i])),
#          ylab='Probability',col=line.colors[i],xlim=c(0,0.3))
#}
for (i in 1:N.facies) {
  x     <-
    alpha <- tab.facies+1
  beta  <-  tot.obs-tab.facies+1
  p.df  <- dbeta(x,alpha[i],beta[i])
}


#Bootstrapping 

source(file.path(chap.code.dir,'fit-kapur-glm-modified.R'))
N.Boot =300

# Bootstrapping using boot function

prob.facies <- function(formula, data, indices){
  
  selected.data    =  data[indices,]
  kapur.glm        =  lda(formula,data=selected.data)
  pred.glm         =  fitted(kapur.glm)
  pred.glm         =  sortPredByLevels(pred.glm)
  most.likely.glm =  apply(pred.glm,1,which.max)
  tab.pred.facies =  table(most.likely.glm)
  tot.obs         =   sum(tab.pred.facies)
  all.fac.name    =  names(table(data$facies))
  N.facies        =   length(all.fac.name)
  idxToFac        =   all.fac.name
  names(idxToFac) =   as.character(1:N.facies)
  sel.fac.name    =   names(tab.pred.facies)
  names(tab.pred.facies) = idxToFac[sel.fac.name]
  no.occur.facies =   setdiff(all.fac.name,sel.fac.name)
  prob            =   rep(NA,N.facies)
  names(prob)     =  all.fac.name
  if (any(as.integer(no.occur.facies))) {
    prob[no.occur.facies] = 0
  }
  prob[sel.fac.name]  =  as.vector(tab.pred.facies)/tot.obs
  return(prob)
}

# depth.vars <- function(formula, data, indices){
#   kapur.glm       =  multinom(formula,data=data[indices,])
#   pred.glm        =  fitted(kapur.glm)
#   pred.glm        = sortPredByLevels(pred.glm)
#   most.likely.glm = apply(pred.glm,1,which.max)
#   tab.pred.facies =  table(most.likely.glm)
#   N.facies        =  length(tab.pred.facies)
#   tot.obs         =  sum(tab.pred.facies)
#   print(N.facies)
#   prob            =  as.vector(tab.pred.facies)/tot.obs
#   return(most.likely.glm)
# }

form <- facies ~ caliper + ind.deep + gamma + r.deep + density
#sample.idx = sample(1:tot.obs,tot.obs,replace=FALSE) #change tot.obs
boot.result <- boot(formula=form, statistic=prob.facies,
                    data=scaleKapur(kapur),R=N.Boot)
#boot.depth.vars <- boot(formula=form, statistic=prob.facies,
#                    data=scaleKapur(kapur[sample.idx,]),R=N.Boot)

all.likelihood=as.data.frame(matrix(0,nrow=100000,ncol=3))
colnames(all.likelihood)=c("Proportion","Probability","Facies.type")
all.prior=as.data.frame(matrix(0,nrow=100000,ncol=3))
colnames(all.prior)=c("Proportion","Probability","Facies.type")
sum.size.lik=1
sum.size.pri=1
for (i in 1:N.facies){
  d.facies <-  density(boot.result$t[,i])
  x        <-  d.facies$x
  y        <-  d.facies$y
  #plot(d.facies$x,d.facies$y)
  current.size=length(x)
  sum.size.lik=current.size+sum.size.lik
  all.likelihood[(sum.size.lik-current.size):(sum.size.lik-1),1]=x
  all.likelihood[(sum.size.lik-current.size):(sum.size.lik-1),2]=
    y/sum(y)
  all.likelihood[(sum.size.lik-current.size):(sum.size.lik-1),3]=
    rep(paste("facies ",i),length(x))
  x=seq(0,1,0.001)
  alpha <- tab.facies+1
  beta <-  tot.obs-tab.facies+1
  p.df <- dbeta(x,alpha[i],beta[i])
  #plot(x,p.df)
  current.size=length(x)
  sum.size.pri=current.size+sum.size.pri
  all.prior[(sum.size.pri-current.size):(sum.size.pri-1),1]=x
  all.prior[(sum.size.pri-current.size):(sum.size.pri-1),2]=
    p.df/sum(p.df)
  all.prior[(sum.size.pri-current.size):(sum.size.pri-1),3]=
    rep(names(tab.facies)[i],length(x))
  
}
myStrip <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = line.colors[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             font=2,
             lab = factor.levels[which.panel])
}


setwd("/Users/eansar2/Desktop/Geostatistics/TakeHome")

pdf("prior-likelihood.pdf")

print(xyplot(Probability~Proportion|Facies.type,
             data=all.prior[1:sum.size.pri-1,],type="l",lwd=3,
             strip=myStrip))
print(xyplot(Probability~Proportion|Facies.type,
             data=all.likelihood[1:sum.size.lik-1,],type="l",lwd=3,
             strip=myStrip))
dev.off()

# = rep(NA,n.Boot)
# Bootstrapping by sampling
#  for (i in 1:N.Boot){
#    idx = sample(1:tot.obs,tot.obs,replace=TRUE)
#    kapur.scaled = sacleKapur(kapur[idx,])
#    prob.facies(formula=form,data=kapur.scaled,indices=idx)    
#  }

# Finding the Posterior
pdf("posterior.pdf")
opar=par()
par(mfrow=c(2,2))
g=vector('list',8)
for (i in 1:N.facies){
  d.facies <-  density(boot.result$t[,i])
  x        <-  d.facies$x
  y        <-  d.facies$y
  
  alpha      <- tab.facies+1
  beta       <-  tot.obs-tab.facies+1
  p.df       <- dbeta(x,alpha[i],beta[i])
  posterior  = p.df*y/sum(p.df*y)
  prior      = p.df/sum(p.df)
  likelihood = y/sum(y)
  
  df <- data.frame(x,prior,likelihood,posterior)
  g[[i]] <- ggplot(df, aes(x)) +                   
    geom_line(aes(y=prior,colour="prior")) +  
    geom_line(aes(y=likelihood,colour="likelihood")) +  
    geom_line(aes(y=posterior,colour="posterior"))
  g[[i]] <- g[[i]] + xlab(paste("proportion of facies ", 
                                names(tab.facies[i]))) + ylab("Probability")+
    scale_colour_manual("",values=c("darkgreen","blue","red"))
}
require(gridExtra)
for (i in seq(1,8,2)){
  sidebysideplot <- grid.arrange(g[[i]], g[[i+1]], nrow=2)
  print(sidebysideplot)
}
dev.off()

save(list=ls(),file="workspace1")