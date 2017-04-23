rm(list=ls())
states <- c("Queue","Drill","Produce","Abandon")

N.states <- length(states)

N.wells <- 10
wells <- vector("list",N.wells)

for (i in 1:N.wells){
  wells[[i]] = list(current.state="Queue",next.state="Queue",
                    t.states=matrix(0,10000,N.states),
                    t.drill=sample(2:6,1,p=c(0,0.25,0.5,0.75,1)),
                    t.this=0,N.believe=runif(1,500,1000),
                    N.true=runif(1,500,1000),q=runif(1,0.5,3),value=0)
  colnames(wells[[i]]$t.states) <- states
}


trans.prob=matrix(0,nrow=N.states,ncol=N.states,
                  dimnames=list(states,states))

trans.prob["Drill","Drill"]=0
trans.prob["Drill","Abandon"]=0.1
trans.prob["Drill","Produce"]=0.6
trans.prob["Produce","Produce"]=0.9
trans.prob["Produce","Abandon"]=0.1
trans.prob["Abandon","Abandon"]=1

     #Initial Condition
time=0
wells.abandoned=rep(FALSE,N.wells)
wells[[1]]$current.state="Queue"
trans.prob["Queue","Drill"]=1
trans.prob["Queue","Queue"]=0


while (!all(wells.abandoned)){
  
  for (i in 1:N.wells){
    
    if  (wells[[i]]$current.state=="Drill" )
         if(wells[[i]]$t.this+1 < wells[[i]]$t.drill){
          #trans.prop <- drill.close(trans.prop)
          trans.prob["Queue","Drill"]=0
          trans.prob["Queue","Queue"]=1
          trans.prob["Drill",]=0
          trans.prob["Drill","Drill"]=1
          }else{
            #trans.prop <- drill.open(trans.prop)
            trans.prob["Queue","Drill"]=1
            trans.prob["Queue","Queue"]=0
            trans.prob["Drill","Drill"]=0
            trans.prob["Drill","Abandon"]=0.1
            trans.prob["Drill","Produce"]=0.6
          }
    
    current.state = wells[[i]]$current.state
    next.state = wells[[i]]$next.state
    
    
    #if(current.state=="Abandon") next
    
    pos.next.states  <- names(which(  trans.prob[current.state,]!=0 )) 
    prob.next.states <- trans.prob[current.state,pos.next.states]
    selected.next.state<- sample(pos.next.states,1,p=prob.next.states)
    #browser()
    #if (selected.next.state=="Drill")
    #    if  ((wells[[i]]$current.state=="Drill" && 
    #          wells[[i]]$t.this < wells[[i]]$t.drill)) {
    #      wells[[i]]$next.state="Drill"
    #     break
    #    } 
    #if  ((wells[[i]]$current.state=="Drill" && 
    #        wells[[i]]$t.this < wells[[i]]$t.drill)) {
    #  wells[[i]]$next.state="Drill"
    #  break
    #}
    
    wells[[i]]$next.state <- selected.next.state
  }
  
  for (i in 1:N.wells) {
    
    if (wells[[i]]$next.state==wells[[i]]$current.state)
        wells[[i]]$t.this=wells[[i]]$t.this+1 
    else 
      wells[[i]]$t.this=0
    
    time=time+1
    wells[[i]]$t.states[time,wells[[i]]$current.state]=1    
    wells[[i]]$current.state = wells[[i]]$next.state

    
    
    wells.abandoned[[i]] = (wells[[i]]$current.state=="Abandon")
  }
  
  
  
}
