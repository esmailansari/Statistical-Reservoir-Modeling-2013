rm(list=ls())
require(vioplot)
run.month=2000
sim.number=1000
simulation.res=matrix(0,nrow=run.month,ncol=sim.number)

source(file.path('/Users/eansar2/Desktop/Geostatistics/TakeHome',
                 'SetState.R'))

states <- c("Queue","Drill","FinishedDrill","Complete","Produce",
            "Shut_in","WaitForRig","WorkOver","Abandon")
costs <- list(Queue=0,Drill=-25,FinishedDrill=0,Complete=-25,Produce=-0.1,
              Shut_in=0,WaitForRig=0,WorkOver=-25,Abandon=-10)

N.states <- length(states)

N.wells <- 10
wells <- vector("list",N.wells)

N.True=rep(0,N.wells)
N.Believe=rep(0,N.wells)

for (kk in 1:N.wells)
{
  N.True[kk]=runif(1,500,1000)
  N.Believe[kk]=runif(1,500,1000)
}


for (sim.counter in 1:sim.number){

  
    for (i in 1:N.wells){
      wells[[i]] = list(current.state="Queue",next.state="Queue",
                        t.states=matrix(0,run.month,N.states),
                        t.drill=sample(2:6,1,p=c(0,0.25,0.5,0.75,1)),
                        t.this=0,N.believe=N.Believe[i],
                        N.true=N.True[i],q=runif(1,0.5,3),value=0,
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
    monthly.dev.val=rep(0,run.month)

    
    
    
    while (!all(wells.abandoned)){
      
      for (i in 1:N.wells){
        
        for (j in 1:N.wells)
          drilling.flag[j] =wells[[j]]$drilling.flag
        
        if (any(drilling.flag))
          trans.prob <- drill.close(trans.prob) else
            trans.prob <- drill.open(trans.prob)
        
        
        if (sum(wells[[i]]$t.states[1:time,"Drill"])==wells[[i]]$t.drill)
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
      #browser()
      
      time=time+1
      
      #browser()
      for (jjj in 1:N.wells)
        monthly.dev.val[time]=monthly.dev.val[time]+wells[[jjj]]$value
      
      if (time>(run.month-10)) break
    }

simulation.res[,sim.counter]=c(    monthly.dev.val[1:time], 
                                   rep(monthly.dev.val[time],
                                        run.month-time)        )

}


