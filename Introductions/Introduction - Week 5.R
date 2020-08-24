#####################################################################################
# 
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 5 - Programming Structures
#
#   GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
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