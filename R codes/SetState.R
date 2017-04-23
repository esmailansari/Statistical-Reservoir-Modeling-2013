initialize <- function(trans.prob){
  trans.prob["Queue","Queue"]=0   #Queue open at the begining
  trans.prob["Queue","Drill"]=1
  trans.prob["Drill","Drill"]=0   #Need to be reset inside code
  trans.prob["Drill","FinishedDrill"]=1 #This valve open at start.
  trans.prob["FinishedDrill","Complete"]=0.8
  trans.prob["FinishedDrill","Abandon"]=0.2
  trans.prob["Complete","Complete"]=0
  trans.prob["Complete","Produce"]=0.8
  trans.prob["Complete","Abandon"]=0.2
  trans.prob["Produce","Produce"]=0.9
  trans.prob["Produce","Shut_in"]=0.1
  trans.prob["Produce","Abandon"]=0    #!!!!!!!!!!!!!
  trans.prob["Shut_in","Produce"]=0.9
  trans.prob["Shut_in","WaitForRig"]=0.1
  trans.prob["WaitForRig","WorkOver"]=1  #This valve open. 
  #trans.prob["WaitForRig","WaitForRig"]=0
  trans.prob["WorkOver","WorkOver"]=0
  trans.prob["WorkOver","Produce"]=0.8
  trans.prob["WorkOver","Abandon"]=0.2
  trans.prob["Abandon","Abandon"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------

drill.close <- function(trans.prob){
  trans.prob["Queue","Queue"]=1
  trans.prob["Queue","Drill"]=0
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------

drill.open <-function(trans.prob){
  trans.prob["Queue","Queue"]=0
  trans.prob["Queue","Drill"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------

Finished.drill.close <- function(trans.prob){
   trans.prob["Drill",]=0
   trans.prob["Drill","Drill"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------

Finished.drill.open <- function(trans.prob){
    trans.prob["Drill",]=0
    trans.prob["Drill","FinishedDrill"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------
WorkOver.open <-function(trans.prob){
  trans.prob["WaitForRig","WorkOver"]=1
  trans.prob["WaitForRig","WaitForRig"]=0
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------
WorkOver.close <-function(trans.prob){
  trans.prob["WaitForRig","WorkOver"]=0
  trans.prob["WaitForRig","WaitForRig"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------
well.abandon <-function(trans.prob){
  trans.prob["Produce",]=0
  trans.prob["Produce","Abandon"]=1
  return(trans.prob)
}

#---------------------------------------
#---------------------------------------
well.open <-function(trans.prob){
  trans.prob["Produce",]=0
  trans.prob["Produce","Produce"]=0.9
  trans.prob["Produce","Shut_in"]=0.1
  return(trans.prob)
}
