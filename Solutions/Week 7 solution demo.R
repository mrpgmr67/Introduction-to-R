##########################################################################
# Data 710: Introduction to R
# Week 7: Assignment - Object Oriented Programming
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")        # Don't forget to set to your working directory to find the files

##################################################################################################
# Create a S3 class named crops that will track soybeans, wheat, and corn harvests.
#     - Include a function to evaluate the mean of all three crops, and include a simple error 
#       handling routine.
##################################################################################################

# A crops constructor
crops <- function(aCropType,aCropYield,aYear,aFieldNumber) {
  
  aLen <- length(aCropType)
  aDataFrame <- NULL
  if ((aLen != length(aCropYield) | (aLen != length(aYear)) | (aLen != length(aFieldNumber)))) {
    cat("Error: Parameters need to be of the same length!")
  } else {
    aDataFrame <- data.frame(cropType=aCropType,cropYield=aCropYield,year=aYear,fieldNumber=aFieldNumber)
    class(aDataFrame) <- "crops"
  }
  
  return(aDataFrame)
}

# Implement a nice print function for the class
print.crops <- function(obj) {
  aDF <- data.frame(unclass(obj))
  print(aDF)
}

# Create new class function
cropMean <- function(anObj) {
  UseMethod("cropMean")
}
# The default could have handled the mean, but built this to show a generic class and an extension
cropMean.default <- function(anObj) {
  cat("This is a generic function\n")
}
cropMean.crops <- function(anObj) {

  anObj <- data.frame(unclass(anObj))
  aResult<-mean(anObj$cropYield)
 
  return(aResult)   
}

cropYearlyMean <- function(anObj) {
  UseMethod("cropYearlyMean")
}
# The default could have handled the mean, but built this to show a generic class and an extension
cropYearlyMean.default <- function(anObj) {
  cat("This is a generic function\n")
}
cropYearlyMean.crops <- function(anObj) {
  
  anObj <- data.frame(unclass(anObj))
  
  aResult <- aggregate(anObj[,"cropYield"],list(year=anObj$year),mean)
  colnames(aResult) <- c("Year","Mean")
  
  return(aResult)   
}

# Create new addCrops class function.  This will add merge crops into a single data structure.
addCrops <- function(anObj,aNewObj) {
  UseMethod("addCrops")
}
addCrops.default <- function(anObj,aNewObj) {
  cat("This is a generic function\n")
}
addCrops.crops <- function(anObj,aNewObj) {

   anObj <- data.frame(unclass(anObj))
   aNewObj <- data.frame(unclass(aNewObj))
   
   aResult <- rbind(anObj,aNewObj)

   class(aResult) <- "crops"
   return(aResult)
}


# Test new class
aTestCrop <- crops(rep("soybeans",5),aCropYield = 6:10, aYear=c(1,1,1,1,1), aFieldNumber = 1:5)
class(aTestCrop)
cropMean(aTestCrop)
aTestCrop


aNewTestCrop <- crops(rep("wheat",5),6:10,1:5,1:5)
aTestCrop <- addCrops(aTestCrop, aNewTestCrop)

aNewTestCrop <- crops(rep("corn",10),1:10,1:10,1:10)
aTestCrop <- addCrops(aTestCrop, aNewTestCrop)
cropMean(aTestCrop)
cropYearlyMean(aTestCrop)
aTestCrop

# Test Error Handling
aNewTestCrop <- crops(rep("soybeans",5),5:10,1:5,1:5)


##################################################################################################
# Create a S4 class named workers that will track farm workers harvesting those same crops.  
#      - Assume that each worker has a skillset that is unique to each crop, plus another set of 
#        workers that can harvest multiple types.
#      - Include a function that will provide summary information about those workers.
##################################################################################################

setClass("Farm_Worker",               
         representation(
           ID = "numeric",                
           Crop_Skillset = "character",
           Multiple_Types = "logical")
#         validity=function(object) {
#         }
         
)
Farm_Worker <- function(anId, aCrop_Skillset, aMultiple_Types) {
  aResult <- new("Farm_Worker", ID=anId, Crop_Skillset=aCrop_Skillset, Multiple_Types=aMultiple_Types)
  return(aResult)
}

# Create new generic function
setGeneric(
  "Worker_Summary",
  function(anObj) {
    standardGeneric("Worker_Summary")
  }
)

setMethod("Worker_Summary","Farm_Worker",
  function(anObj) {
    ids <- slot(anObj,"ID")
#    cat("Total number of workers: ",length(ids),"\n")
    multiSkilled <- sum(anObj@Multiple_Types)
    
    skillsetTable <- table(crop=anObj@Crop_Skillset,multiSkill=anObj@Multiple_Types)
#    print(skillsetTable)
    aResult <- data.frame(skillsetTable)
    return(aResult)
  }
)
setGeneric(
  "Add_Worker",
  function(anObj,aNewObj) {
    standardGeneric("Add_Worker")
  }
)

setMethod("Add_Worker","Farm_Worker",
          function(anObj,aNewObj) {
            anIndex <- length(anObj@ID)
            anObj@ID <- append(anObj@ID,aNewObj@ID,anIndex)
            anObj@Crop_Skillset <- append(anObj@Crop_Skillset,aNewObj@Crop_Skillset,anIndex)
            anObj@Multiple_Types <- append(anObj@Multiple_Types,aNewObj@Multiple_Types,anIndex)
            return(anObj)
          }
)

# Test Farm_Worker Class
cornWorkers <- Farm_Worker(1:10,rep("corn",10),rep(FALSE,10))
wheatWorkers <- Farm_Worker(11:15,rep("wheat",5),rep(FALSE,5))
soybeanWorkers <- Farm_Worker(16:20,rep("soybeans",5),rep(FALSE,5))
superWorkers <- Farm_Worker(21:25,rep("corn",5), rep(TRUE,5))
workers <- cornWorkers
workers <- Add_Worker(workers,wheatWorkers)
workers <- Add_Worker(workers,soybeanWorkers)
workers <- Add_Worker(workers, superWorkers)
aSummary <- Worker_Summary(workers)
aSummary

##################################################################################################
# Create an inherited class based on one of the classes you have created.
##################################################################################################

setClass("FT Employee", contains = "Farm_Worker", representation(name="character"))
aWorker2 <- new("FT Employee", ID=1, Crop_Skillset="corn", Multiple_Types=FALSE, name="Joe") 

# Test
aWorker2
class(aWorker2)
Worker_Summary(aWorker2)
str(aWorker2)

# Clean Up test objects before the simulation
rm(cornWorkers)
rm(wheatWorkers)
rm(soybeanWorkers)
rm(superWorkers)
rm(workers) 
rm(aSummary)
rm(aTestCrop)
rm(aNewTestCrop)
rm(aWorker2)
##################################################################################################
# Build a farming simulation based on the combination of both workers and crops.  
#      - Use the classes that were created in the previous steps and have it set to calculate yearly 
#        projections.
#      - Create input variables for each of the components, and run the simulation for a five year 
#        cycle.
##################################################################################################

# farmSim assumes it will take one worker per field to product 1 unit of yield.
# A value can be entered into either workerSD and/or fieldSD to introduce standard deviation into the simulation
farmSim <- function(cornWorkers=0
                    ,wheatWorkers=0
                    ,soybeanWorkers=0
                    ,multiSkilledWorkers=0
                    ,workerSD=0
                    ,cornFields=0
                    ,wheatFields=0
                    ,soybeanFields=0
                    ,fieldSD=0
                    ,yearsToSim=1) {
  
  # Internal function to generate a work pool, with a normal distribution, for a year.  The work pool
  # changes are meant to simulate work force fluctuations.  Any negatives work force distributions are
  # voided out and set to zero.
  workPool <- function(cornWorkers=0,wheatWorkers=0,soybeanWorkers=0,multiSkilledWorkers=0,workerSD=0) {
    nbrWorkers <- round(rnorm(1,cornWorkers,workerSD))
    if(nbrWorkers < 0) nbrWorkers = 0
    workers <- Farm_Worker(1:nbrWorkers,rep("corn",nbrWorkers),rep(FALSE,nbrWorkers))
    
    anIndex <- nbrWorkers + 1
    nbrWorkers <- round(rnorm(1,wheatWorkers,workerSD))
    if(nbrWorkers < 0) nbrWorkers = 0
    anEndIndex <- anIndex+nbrWorkers-1
    newWorkers <- Farm_Worker(anIndex:anEndIndex,rep("wheat",nbrWorkers),rep(FALSE,nbrWorkers))
    workers <- Add_Worker(workers,newWorkers)

    anIndex <- anIndex + nbrWorkers +1
    nbrWorkers <- round(rnorm(1,soybeanWorkers,workerSD))
    if(nbrWorkers < 0) nbrWorkers = 0
    anEndIndex <- anIndex+nbrWorkers-1
    newWorkers <- Farm_Worker(anIndex:anEndIndex,rep("soybeans",nbrWorkers),rep(FALSE,nbrWorkers))
    workers <- Add_Worker(workers,newWorkers)
    
    anIndex <- anIndex + nbrWorkers +1
    nbrWorkers <- round(rnorm(1,multiSkilledWorkers,workerSD))
    if(nbrWorkers < 0) nbrWorkers = 0
    anEndIndex <- anIndex+nbrWorkers-1
    newWorkers <- Farm_Worker(anIndex:anEndIndex,rep("multi",nbrWorkers),rep(TRUE,nbrWorkers))
    workers <- Add_Worker(workers,newWorkers)
    
    return(workers)
  }
  
  # This function is intended to dispense the multi-skilled workers when needed
  requestMultiSkill <- function(nbrRequested,setInitialNumber=0,resetNumber=FALSE) {

    if (resetNumber == TRUE) {
      aMSWC <<- setInitialNumber
    }
    
    aResult <- 0
    if (nbrRequested <= aMSWC) {
      aResult <- nbrRequested
      aMSWC <<- aMSWC - nbrRequested
    } else {
      aResult <- aMSWC
      aMSWC <<- 0
    }
    
    return(aResult)
  }
  
  # This is the core loop for the simulation
  for (i in 1:yearsToSim) {
    aWorkPool <- workPool(cornWorkers,wheatWorkers,soybeanWorkers,multiSkilledWorkers,workerSD)
    aSummary <- Worker_Summary(aWorkPool)

    # Number of multi-skilled crop workers.  These workers will be used after the crop specific workers
    # have been depleted.  
    nbrMultSkilledWorkers <- sum(aSummary[which(aSummary$multiSkill == TRUE),"Freq"]) 
    mSW <- requestMultiSkill(0,nbrMultSkilledWorkers,TRUE)


    # Start with Corn Sim
    nbrFields <- round(rnorm(1,cornFields,fieldSD))
    nbrWorkers <- aSummary[which((aSummary$crop=="corn") & (aSummary$multiSkill==FALSE)),"Freq"]
    if (nbrFields < nbrWorkers) {
      if(exists("aCrop") == TRUE) {
        aNewCrop <- crops("corn",aCropYield = nbrFields, aYear=i, aFieldNumber = nbrFields)
        aCrop <- addCrops(aCrop, aNewCrop)
      } else {
        aCrop <- crops("corn",aCropYield = nbrFields, aYear=i, aFieldNumber = nbrFields)
      }
    } else {
      if(exists("aCrop") == TRUE) {
        mSW <- requestMultiSkill(nbrFields-nbrWorkers)
        aNewCrop <- crops("corn",aCropYield = nbrWorkers+mSW, aYear=i, aFieldNumber = nbrFields)
        aCrop <- addCrops(aCrop, aNewCrop)
      } else {
        mSW <- requestMultiSkill(nbrFields-nbrWorkers)
        aCrop <- crops("corn",aCropYield = nbrWorkers+mSW, aYear=i, aFieldNumber = nbrFields)
      }
    }

    # Soybean Sim
    nbrFields <- round(rnorm(1,soybeanFields,fieldSD))
    nbrWorkers <- aSummary[which((aSummary$crop=="soybeans") & (aSummary$multiSkill==FALSE)),"Freq"]
    if (nbrFields < nbrWorkers) {
      aNewCrop <- crops("soybeans",aCropYield = nbrFields, aYear=i, aFieldNumber = nbrFields)
      aCrop <- addCrops(aCrop, aNewCrop)
    } else {
      mSW <- requestMultiSkill(nbrFields-nbrWorkers)
      aNewCrop <- crops("soybeans",aCropYield = nbrWorkers+mSW, aYear=i, aFieldNumber = nbrFields)
      aCrop <- addCrops(aCrop, aNewCrop)
    }                
    
    # Wheat Sim
    nbrFields <- round(rnorm(1,wheatFields,fieldSD))
    nbrWorkers <- aSummary[which((aSummary$crop=="wheat") & (aSummary$multiSkill==FALSE)),"Freq"]
    if (nbrFields < nbrWorkers) {
      aNewCrop <- crops("wheat",aCropYield = nbrFields, aYear=i, aFieldNumber = nbrFields)
      aCrop <- addCrops(aCrop, aNewCrop)
    } else {
      mSW <- requestMultiSkill(nbrFields-nbrWorkers)
      aNewCrop <- crops("wheat",aCropYield = nbrWorkers+mSW, aYear=i, aFieldNumber = nbrFields)
      aCrop <- addCrops(aCrop, aNewCrop)
    }        

  }
  
  cat("\nCrop Yield Mean: ",cropMean(aCrop),"\n\n")
  
  cat("Yearly Projected Simulation Means: \n")
  print(cropYearlyMean(aCrop))
  
  cat("\n\nYearly Projected Simulation Details\n") 
  print(aCrop)
  
  return(aCrop)
}

a <- farmSim(cornWorkers=100
             ,wheatWorkers=100
             ,soybeanWorkers=100
             ,multiSkilledWorkers=20
             ,workerSD=5
             ,cornFields=100
             ,wheatFields=120
             ,soybeanFields=125
             ,fieldSD=5
             ,yearsToSim=5)

