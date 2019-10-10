##########################################################################
# Data 710: Introduction to R
# Week 6: Assignment - Introduction to Simulations
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files


##################################################################################################
# Given a standard deck of 52 playing cards, create a simulation to calculate the probability of 
# getting and ace.
##################################################################################################
drawAces <- function(attempts) {
  
  drawAce <- function(){
  
    foundAce = FALSE
    
    #Create deck and pick a card
    aCard <- sample(1:52,1)
    # Cards 13,26,39,52 are aces
    if ((aCard %% 13) == 0) {
      foundAce <- TRUE
    } 
    return (foundAce)
  }
  
  totalAces <- 0
  for (i in 1:attempts){
    aResult <- drawAce()
    if(aResult == TRUE) {
      totalAces <- totalAces+1
    } 
  }
  return(totalAces/attempts)
}

# The greater number of attempts, the closer the simulation results calculate the probability
drawAces(10)
drawAces(100)
drawAces(1000)
drawAces(10000)
drawAces(100000)

##################################################################################################
# Create a two simulations of function to perform coin toss results (binary). Use the sample() 
# function for one and the rbinom() for the other.
# The Art of Programming by Rorman Matloff (page 204)
##################################################################################################

# Coin toss function, using a sample() function, resulting in a binary result
# 0 is heads and 1 is tails
flipCoinS <- function() {
  aResult <- sample(0:1,1)
  
  return(aResult)
}
flipCoinS()

# Coin toss function, using a sample function, resulting in a binary result
# 0 is heads and 1 is tails
flipCoinR <- function() {
  aResult <- rbinom(1,size=1,prob=0.5)
  
  return(aResult)
}
flipCoinR()

##################################################################################################
# Create a function to calculate the probability for a discrete distribution.
##################################################################################################
myBinomialPD <- function(x,n,p) {
  
  aResultSet <- vector(mode='numeric',length=length(x))
  
  for (i in 1:length(x)) {
     aResultSet[i] <- (factorial(n)/(factorial(x[i])*factorial(n-x[i])))*(p^x[i])*(1-p)^(n-x[i])
  }
  
  return(aResultSet)
}

myBinomialPD(1,10,0.6)

# Display Binomial distribution table as a test
for (i in 0:10) {
  aResult <- myBinomialPD(i,10,0.6)
  print(aResult)
}

# Test myBinomialPD by comparing it to the standard R dbinom function results 
myBinomialPD(c(0:10),10,0.6)
dbinom(c(0:10),10,0.6)

##################################################################################################
# Create a probability and a distribution. Build a function that will determine the corresponding 
# quantile for that probability.
##################################################################################################
myQuantile <- function(x,y){
  
  x <- sort(x)
  
  aResultSet <- vector(mode='numeric',length = length(y))
  
  for(i in 1:length(y)){
    aStartPos <- (y[i] * length(x))
    if(aStartPos == 0) {
      aStartPos = 1
    }
    aResultSet[i] <- x[floor(aStartPos)]
  }
  resultSetNames <- y*100
  names(aResultSet) <- resultSetNames
  return(aResultSet)
}

# Test myQuantile by comparing it to the standard R quantile function results
aPD <- dbinom(c(0:5),5,0.5)
myQuantile(aPD,c(0,0.25,0.5,0.75,1.0))
quantile(aPD,c(0,0.25,0.5,0.75,1.0),type=3)

aPD <- dbinom(c(0:15),15,0.5)
myQuantile(aPD,c(0,0.25,0.5,0.75,1.0))
quantile(aPD,c(0,0.25,0.5,0.75,1.0),type=3)

##################################################################################################
# Build a function that will calculate a z-score and form a confidence interval for a mean of a 
# population.
##################################################################################################

set.seed(1234)
sampleData = rnorm(1000,mean = 50,sd = 10)

myZTest <- function(x,y,ci){
  aMean <- mean(x)
  aSD <- sd(x)
  anAlpha <-(1-ci)/2
  aCV <- qnorm(1-anAlpha)

  lowerCI <- aMean - (aCV*(aSD/sqrt(length(x))))
  upperCI <- aMean + (aCV*(aSD/sqrt(length(x))))
  print(c("Confidence Interval: ",lowerCI,upperCI))
  
  aZScore <- (y-aMean)/aSD
  print(c("z-score: ",aZScore))

  return(aZScore)
}

tmp <- myZTest(sampleData,39,0.95)
tmp <- myZTest(sampleData,40,0.90)

##################################################################################################
# Using the simulation you suggested in the discussions, create an R program for that simulation. 
# Include graphs and statistical information.
##################################################################################################

# This simulation seeks to show the impact of not having enough inventory in the buffer system to
# fulfill orders from a warehouse.  A normal distribution is used to represent a standard order
# fulfillment time while an exponential distribution is used for buffer replenishment on missing
# items.  Based on this information, warehouse managers can adjust inventory strategies and 
# fulfillment times.
#
# Two parameters are used to generate the simulation results.
#      numberofOrders - number of orders to simulate
#      inventoryAvailable - % of inventory that is planned to be in the product buffer
#
inventorySimulation <- function(numberOfOrders,inventoryAvailable){

    printStats <- function (aSimResult, inventoryAvailable) {
      
      result<- paste("Fulfillment Time (without replenishment):\t",mean(aSimResult$fulfillTime),"\n",sep="")
      aReplenishment <- aSimResult$replenishTime[aSimResult$replenishTime >0]
      
      result<- paste(result,"Items missing from the buffer:\t\t\t",length(aReplenishment),"\n",sep="")
      result<- paste(result,"Average buffer replenishment time:\t\t",mean(aReplenishment),"\n",sep="")
      
  
      result<- paste(result,"\nOverall Fulfillment Time Summary\n",sep="")
      result<- paste(result,"Average:\t",mean(aSimResult$totalTime),"\n",sep="")
      result<- paste(result,"Median:\t\t",median(aSimResult$totalTime),"\n\n\t\tQuantile\n",sep="")
      
      cat(result)
      print(quantile(aSimResult$totalTime))
     
      
      hist(aSimResult$totalTime, 
           main=paste("Histogram of Order Fulfillment Times\n(",inventoryAvailable*100,"% of Inventory in the Buffer)", sep=""),
           xlab="Fulfillment Times", 
           breaks=100)
      
    } 
  

  
  # Randomly simulate an order availability mix 
  inventoryDistribution <- runif(numberOfOrders,0,1)
  
  aResultSet <- data.frame(fulfillTime = numeric(numberOfOrders),replenishTime = numeric(numberOfOrders))
  
  for (i in 1:numberOfOrders) {
    
    # All orders need to go through the buffer, so a base amount of time is added.  This is modeled as a normal distribution since
    # the automated tote retrieval times are not a constant value.
    aResultSet$fulfillTime[i] <- rnorm(1,mean=60,sd=8)
    
    
    if (inventoryDistribution[i] >= inventoryAvailable) {
      # Inventory not in buffer, so time needs to be added 
      aResultSet$replenishTime[i] <- rexp(1,.005)
    } else {
      #Inventory is in the buffer
      aResultSet$replenishTime[i] <- 0
    }
  }
  
  aResultSet$totalTime <- aResultSet$fulfillTime + aResultSet$replenishTime 
  
  printStats(aResultSet, inventoryAvailable)
  
  return(aResultSet)
}

temp <- inventorySimulation(10000,0.99)
temp <- inventorySimulation(10000,0.95)
temp <- inventorySimulation(10000,0.90)
temp <- inventorySimulation(10000,0.50)

