rm(list=ls())
source(file.path('/Users/eansar2/Desktop/Geostatistics/TakeHome',
                 'SetState.R'))

states <- c("Queue","Drill","FinishedDrill","Complete","Produce",
            "Shut_in","WaitForRig","WorkOver","Abandon")
costs <- list(Queue=0,Drill=-25,FinishedDrill=0,Complete=-25,Produce=-0.1,
              Shut_in=0,WaitForRig=0,WorkOver=-25,Abandon=-10)

N.states <- length(states)

N.wells <- 10
wells <- vector("list",N.wells)

for (i in 1:N.wells){
  wells[[i]] = list(current.state="Queue",next.state="Queue",
                    t.states=matrix(0,10000,N.states),
                    t.drill=sample(2:6,1,p=c(0,0.25,0.5,0.75,1)),
                    t.this=0,N.believe=runif(1,500,1000),
                    N.true=runif(1,500,1000),q=runif(1,0.5,3),value=0,
                    drilling.flag=FALSE)
  colnames(wells[[i]]$t.states) <- states
}


trans.prob=matrix(0,nrow=N.states,ncol=N.states,
                  dimnames=list(states,states))

#Initial Condition
trans.prob = initialize(trans.prob)
time=0
wells.abandoned=rep(FALSE,N.wells)
pos.workover.wells=rep(FALSE,N.wells)
waiting.time = rep(0,N.wells)
workover.well=0
def.N.true.wells=rep(FALSE,N.wells)
drilling.flag=rep(FALSE,N.wells)

while (!all(wells.abandoned)){
  
  for (i in 1:N.wells){
     
     for (j in 1:N.wells)
       drilling.flag[j] =wells[[j]]$drilling.flag
          
      if (any(drilling.flag))
        trans.prob <- drill.close(trans.prob) else
        trans.prob <- drill.open(trans.prob)

     
     if (sum(wells[[i]]$t.states[,"Drill"])==wells[[i]]$t.drill)
       trans.prob<-Finished.drill.open(trans.prob) else
         trans.prob<-Finished.drill.close(trans.prob)
     
     if (i==workover.well)
        trans.prob <- WorkOver.open(trans.prob) else 
        trans.prob <- WorkOver.close(trans.prob)
      
     if (def.N.true.wells[i])
        trans.prob <- well.abandon(trans.prob)  else 
        trans.prob <- well.open(trans.prob)
    
    
    current.state = wells[[i]]$current.state
    next.state = wells[[i]]$next.state
    
    pos.next.states  <- names(which(  trans.prob[current.state,]!=0 )) 
    prob.next.states <- trans.prob[current.state,pos.next.states]
    #browser()
    selected.next.state<- sample(pos.next.states,1,p=prob.next.states)

    wells[[i]]$next.state <- selected.next.state
     
     if (wells[[i]]$next.state=="Drill") 
         wells[[i]]$drilling.flag=TRUE else
         wells[[i]]$drilling.flag=FALSE

   }
  
  for (i in 1:N.wells) {
    
    if (wells[[i]]$next.state==wells[[i]]$current.state)
      wells[[i]]$t.this=wells[[i]]$t.this+1  else 
      wells[[i]]$t.this=0
    

    wells[[i]]$t.states[time,wells[[i]]$current.state]=1    
    wells[[i]]$current.state = wells[[i]]$next.state
    
    
    wells.abandoned[[i]] = (wells[[i]]$current.state=="Abandon")
  
    
  }
  
  for (i in 1:N.wells){
    
    profit=as.integer(wells[[i]]$current.state=="Produce")*wells[[i]]$q
    
    if (!(wells[[i]]$current.state=="Abandon"))
      wells[[i]]$value = (wells[[i]]$value+
                            costs[[wells[[i]]$current.state]]+profit)
    if ((wells[[i]]$current.state=="Abandon")&&(wells[[i]]$t.this==1))
      wells[[i]]$value = (wells[[i]]$value+
                            costs[[wells[[i]]$current.state]]+profit)    
    
    wells[[i]]$N.true   = wells[[i]]$N.true - profit
    
    def.N.true.wells[i] = (wells[[i]]$N.true <= 0)   
    
    pos.workover.wells[i]=(wells[[i]]$current.state=="WaitForRig")
    waiting.time[i] = as.integer(pos.workover.wells[i])*
                      wells[[i]]$t.this
    
  }
  
  
  if (any(pos.workover.wells))
  {
   max.waiting.wells=which(waiting.time==
                           max(waiting.time[pos.workover.wells]))
   workover.well= min(max.waiting.wells)
  } else {
   workover.well=0
  }

  
  time=time+1
  
  
}

setwd("/Users/eansar2/Desktop/Geostatistics/TakeHome")
pdf("simulaionPlots.pdf")
dev.val=0
for (i in 1:N.wells)
  dev.val = dev.val + wells[[i]]$value
print(dev.val)

t.plot=200
sim.log=rep(NA,t.plot)
opar=par(mfrow=c(2,2))
for (j in 1:N.wells){
  for (i in 1:t.plot)
  {
    sim.log[i]= which(wells[[j]]$t.states[i,] != 0)
  }

  plot(1:t.plot,sim.log,type="s",axes=F,xlab="",ylab="",lwd=0.25)
  axis(2,at=c(1,2,3,4,5,6,7,8,9),label=states,las=1,cex.axis=0.65)
  axis(1)
  title(paste("well",j,"log"), xlab="month",cex.main=1)
  #par=opar
}
dev.off()
