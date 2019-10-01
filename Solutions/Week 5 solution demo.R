##########################################################################
# Data 710: Introduction to R
# Week 5 - Programming Structures
##########################################################################
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files

##################################################################################################
# Create a function to prove the Central Limit Theorem using a given equation. This function will 
# be used in future week's assignments.
##################################################################################################
#
#  Central Limit Theorem - If you take repeated samples from a population with a finite 
#  variance and calculate their averages, then the averages will be normally distributed
#

centralLimitTheorem <- function(diceValues, nbrDice, aSampleNbr, xlab, showCurve=FALSE) {
  
  means <- numeric(aSampleNbr)
  for (i in 1:aSampleNbr) {
    samples <- sample(diceValues,nbrDice,replace=TRUE)
    aSum <- sum(samples)
    means[i] <- mean(aSum)
  }
  lowerBreak = (diceValues[1]*nbrDice) - .5
  upperBreak = (diceValues[length(diceValues)]*nbrDice) + .5
  hist(means,xlab=xlab,breaks=lowerBreak:upperBreak, main="")

  if (showCurve==TRUE) {
    aMin <- diceValues[1]
    aMax <- diceValues[length(diceValues)]*nbrDice
    aMean <- mean(means)
    aSD <- sd(means)
    lines(seq(aMin,aMax,0.1),dnorm(seq(aMin,aMax,0.1),aMean,aSD)*aSampleNbr)
  }
}

par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
aResult <- centralLimitTheorem(c(1:6),1,10000,"One Dice")
aResult <- centralLimitTheorem(c(1:6),2,20000,"Two Dice")
aResult <- centralLimitTheorem(c(1:6),3,30000,"Three Dice")
aResult <- centralLimitTheorem(c(1:6),5,50000,"Five Dice",showCurve = TRUE)

##################################################################################################
# Create a function to perform a quick sort.
# Adapted from The Art of Programming by Rorman Matloff (page 176)
##################################################################################################
# The following function implements a more "traditional" quicksort algorithm using array indexes.
# Partitioning is done "manually" without using R easy filtering features.  The use of arrays would
# be less efficient than vectorization, but this function is focused on flow control.
quickSort <- function(aVector, lowIndex, highIndex) {
  if (lowIndex < highIndex)
  {
    pivot <- aVector[highIndex];  
    
    i <- (lowIndex - 1) 
    
    for (j in lowIndex:(highIndex-1)) {
      if (aVector[j] <= pivot) {
        i<-i+1
        
        #Exchange values
        aTemp<-aVector[i]
        aVector[i]<-aVector[j]
        aVector[j]<-aTemp
      }
    }
    # Exchange values
    aTemp <-aVector[(i+1)]
    aVector[i+1] <- aVector[highIndex]
    aVector[highIndex] <- aTemp
    
    anIndex <- i + 1
        
    aVector <- quickSort(aVector, lowIndex, anIndex - 1);  
    aVector <- quickSort(aVector, anIndex + 1, highIndex); 
  }
  return(aVector)
}

#Test QuickSort
myNum <- c(56,12,23,87,10,45,27,72,23,20,100)
mySortedNum <- quickSort(myNum,1,length(myNum))
mySortedNum

myNum <- c(10,9,8,7,6,5,4,3)
mySortedNum <- quickSort(myNum,1,length(myNum))
mySortedNum

myNum <- c(1,2,1,2)
mySortedNum <- quickSort(myNum,1,length(myNum))
mySortedNum

#  How can it be rewritten to be more efficient? 
#
#  Second parameter is always one, so you could use as a constant.
#  Third parameter is the length of the first, so it could be calculated inside function



##################################################################################################
# Create a binary search tree function.
# Adapted core funtions from printtree, newtree, and ins from The Art of Programming by Rorman Matloff (pages 181 - 182)
##################################################################################################
createTree <- function (aValue) {
  
  aTree <- matrix(data=c(NA,NA,NA),nrow=3,ncol=3)
  aTree[1,2] <- aValue
  colnames(aTree)<-c("Left","Value","Right")
  aSize<-3
  anIndex<-2
  
  aResult <- list(nextIndex=anIndex, matrixSize=aSize, treeMatrix=aTree)
  return(aResult)
}

addNode <-function(aTree,aValue) {
  
  nextIndex <- aTree["nextIndex"][[1]]
  matrixSize <- aTree["matrixSize"][[1]]
  treeMatrix <- aTree["treeMatrix"][[1]]
  
  # Add more space to the matrix if needed
  if (nextIndex > matrixSize) {
    treeMatrix <- rbind(treeMatrix, matrix(c(NA,NA,NA),nrow=3,ncol=3))
    matrixSize <- matrixSize + 3
  }
  
  # Set starting location  
  anIndex <- 1
  while (anIndex > 0){
    if (aValue <= treeMatrix[anIndex,"Value"]) {
      # Add Value
      if (is.na(treeMatrix[anIndex,"Left"])) {
        treeMatrix[anIndex,"Left"]<-nextIndex
        treeMatrix[nextIndex,"Value"]<-aValue
        anIndex<-0
      } else {
        # Traverse down a level to the Left
        anIndex <- treeMatrix[anIndex,"Left"][[1]]
      }
    } else {
      if (is.na(treeMatrix[anIndex,"Right"])) {
        treeMatrix[anIndex,"Right"]<-nextIndex
        treeMatrix[nextIndex,"Value"]<-aValue
        anIndex<-0
      } else {
        # Traverse down a level to the Right
        anIndex <- treeMatrix[anIndex,"Right"][[1]]
      }
    }
  }
  nextIndex<-nextIndex+1
  aResult <- list(nextIndex=nextIndex, matrixSize=matrixSize, treeMatrix=treeMatrix)
  return(aResult)
}    

printTree <- function(aTree, aSpace=1, anIndex=1){
  
  treeMatrix <- aTree["treeMatrix"][[1]]
  aCount <- 5
  
  if(is.na(treeMatrix[anIndex,"Value"][[1]]) == TRUE) {
    return()
  }
  
  aSpace <- aSpace+aCount
  
  printTree(aTree, aSpace, treeMatrix[anIndex,"Right"][[1]])
  if (exists("printString") != TRUE) {
    printString<-""
  }
  cat(rep(".",(aSpace-aCount-1)), sep="")
  cat(treeMatrix[anIndex,"Value"][[1]],"\n")
  printTree(aTree, aSpace, treeMatrix[anIndex, "Left"][[1]])
  
  return()
}

# Test Binary Tree
aTree <- createTree(8)
p<-printTree(aTree)
aTree <- addNode(aTree,5)
p<-printTree(aTree)
aTree <- addNode(aTree,20)
p<-printTree(aTree)
aTree <- addNode(aTree,6)
p<-printTree(aTree)
aTree <- addNode(aTree,2)
p<-printTree(aTree)
aTree <- addNode(aTree,21)
p<-printTree(aTree)
aTree <- addNode(aTree,18)
p<-printTree(aTree)

##################################################################################################
# Create a discrete event simulation for a queue system involving a bus terminal. It should include 
# an event list, along with components to add and subtract from that list as buses arrive and depart 
# from the terminal. The following functions should be present:
#      - Core simulation function
#      - Variable initialization
#      - Add event
#      - Event response
#      - Get next event
#      - Function to print the results of the simulation
##################################################################################################

# DES.R:  R routines for discrete-event simulation (DES)

# Adapted from The Art of Programming by Rorman Matloff (pages 166 - 169)

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype, a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm,evntty,appin=NULL) {
  rw <- c(list(evnttime=evnttm,evnttype=evntty),appin)
  return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm,evntty,appin=NULL) {
  newevnt <- evntrow(evnttm,evntty,appin)
  # if the event list is empty, set it to consist of evnt and return
  if (is.null(sim$evnts)) {
    sim$evnts <<- newevnt
    return()
  }
  # otherwise, find insertion point
  inspt <- binsearch((sim$evnts)$evnttime,evnttm) 
  # now "insert," by reconstructing the data frame; we find what
  # portion of the current matrix should come before the new event and
  # what portion should come after it, then string everything together
  before <- 
    if (inspt == 1) NULL else sim$evnts[1:(inspt-1),]
  nr <- nrow(sim$evnts)
  after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
  sim$evnts <<- rbind(before,newevnt,after)
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x,y) {
  n <- length(x)
  lo <- 1
  hi <- n
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (y == x[mid]) return(mid)
    if (y < x[mid]) hi <- mid else lo <- mid
  }
  if (y <= x[lo]) return(lo)
  if (y < x[hi]) return(hi)
  return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
  head <- sim$evnts[1,]
  # delete head
  if (nrow(sim$evnts) == 1) {
    sim$evnts <<- NULL
  } else sim$evnts <<- sim$evnts[-1,]
  return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,
                  dbg=FALSE) {
  sim <<- list()
  sim$currtime <<- 0.0  # current simulated time
  sim$evnts <<- NULL  # events data frame
  sim$dbg <<- dbg
  initglbls(apppars)
  while(sim$currtime < maxsimtime) {  
    head <- getnextevnt()
    sim$currtime <<- head$evnttime  # update current simulated time
    reactevnt(head)  # process this event 
    if (dbg) print(sim)
  }
  prntrslts()
}

# Bus terminal application specific functions 
# This simulation assumes a normal distribution for the servicing and arrival of a bus at a terminal.  
# The key to the simulation will largely be the identification of the right distribution.  A normal
# distribution would not be a proper choice if there are going to be values near zero, which would result 
# in an error trap.  By adjusting the values in the error trap, the normal distribution would be lost.
terminalInitGlbls <- function(apppars) {
  terminal.Glbls <<- list()
  # simulation parameters
  terminal.Glbls$arrvrate <<- apppars$arrvrate
  terminal.Glbls$arrvsd   <<- apppars$arrvsd
  terminal.Glbls$srvrate <<- apppars$srvrate
  terminal.Glbls$srvsd <<- apppars$srvsd
  # server queue, consisting of arrival times of queued jobs
  terminal.Glbls$srvq <<- vector(length=0) 
  # statistics
  terminal.Glbls$busesDeparted <<- 0  # jobs done so far
  terminal.Glbls$totwait <<- 0.0  # total wait time so far

  aTime <- rnorm(1,terminal.Glbls$arrvrate,terminal.Glbls$arrvsd)
  # Prevent buses from arriving "in the past" or at the same time
  ifelse (aTime <= 0,1,aTime)                     
  arrvtime <- sim$currtime + aTime
  schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
}

# application-specific event processing function called by dosim()
# in the general DES library 
terminalEvent <- function(head) {
  if (head$evnttype == "arrv") {  # arrival

    if (length(terminal.Glbls$srvq) == 0) {
      terminal.Glbls$srvq <<- head$arrvtime

      aTime <- rnorm(1,terminal.Glbls$srvrate,terminal.Glbls$srvsd)
      
      # Error Trap - Bus must stop for at least 1 minute to handle empty buses
      ifelse (aTime <= 0,1,aTime)
      
      srvdonetime <- sim$currtime + aTime                     
      
      schedevnt(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
    } else {
      terminal.Glbls$srvq <<- c(terminal.Glbls$srvq,head$arrvtime)
    }
    
    # generate next arrival using a normal distribution
    aTime <- rnorm(1,terminal.Glbls$arrvrate,terminal.Glbls$arrvsd)
    # Error Trap - Prevent buses from arriving "in the past" or at the same time
    ifelse (aTime <= 0,1,aTime)                     
    arrvtime <- sim$currtime + aTime
    schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
  } else {  

    terminal.Glbls$busesDeparted <<- terminal.Glbls$busesDeparted + 1
    terminal.Glbls$totwait <<- terminal.Glbls$totwait + sim$currtime - head$arrvtime
    # remove from queue
    terminal.Glbls$srvq <<- terminal.Glbls$srvq[-1]
    # more still in the queue?
    if (length(terminal.Glbls$srvq) > 0) {
      # schedule new service
      
      aTime <- rnorm(1,terminal.Glbls$srvrate,terminal.Glbls$srvsd)
      # Bus must stop for at least 1 minute to handle empty situations
      ifelse (aTime <= 0,1,aTime)
      
      srvdonetime <- sim$currtime + aTime                     
      schedevnt(srvdonetime,"srvdone",list(arrvtime=terminal.Glbls$srvq[1]))
    } 
  }
}

terminalSimResultsPrint <- function() {
  print("Total Wait Time:")
  print(terminal.Glbls$totwait)
  print("Total Buses Serviced:")
  print(terminal.Glbls$busesDeparted)
  print("Bus mean times (minutes):")
  print(terminal.Glbls$totwait/terminal.Glbls$busesDeparted)
}

# Bus termininal is open for 12 hours a day (i.e. 720 minutes)
# A simulation with typical parameters
dosim(terminalInitGlbls,terminalEvent,terminalSimResultsPrint,720,list(arrvrate=45,arrvsd=10,srvrate=45,srvsd=10))

# A simulation with quick service rates
dosim(terminalInitGlbls,terminalEvent,terminalSimResultsPrint,720,list(arrvrate=45,arrvsd=10,srvrate=5,srvsd=1))

# A simulation with quick arrivals
dosim(terminalInitGlbls,terminalEvent,terminalSimResultsPrint,720,list(arrvrate=5,arrvsd=1,srvrate=45,srvsd=10))

