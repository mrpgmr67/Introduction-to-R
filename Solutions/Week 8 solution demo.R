##########################################################################
# Data 710: Introduction to R
# Week 8: Assignment - Input and Output
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")        # Don't forget to set to your working directory to find the files

aDataFrame <- read.table("sample.csv", header=TRUE, sep=",")

#Test
class(aDataFrame)
head(aDataFrame)

##################################################################################################
# Build a function to read data from a web-URL.
# Use the public archive data on Relative CPU performance: 
#   - URL: http://archive.ics.uci.edu/ml/machine-learning-databases/
#   - Dataset: machine.data
#   - Name File: machine.names
##################################################################################################

# No machine data was located at http://archive.ics.uci.edu/ml/machine-learning-databases/; however,
# the data files appear to be in the cpu-performance subdirectory.
aTempURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/"
aTempFileName <- "machine.data"
aTempHeader <- c("Vendor_Name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")

aUrlFile <- function (aURL, aFileName, aHeader) {
  aDataFile <- paste(aURL,aFileName, sep = "")
  
  data <- read.csv(aDataFile, header = FALSE, stringsAsFactors = FALSE)
  colnames(data) <- aHeader
  return(data)
}

aFile <- aUrlFile(aTempURL,aTempFileName,aTempHeader) 
head(aFile)

# Clean Up
rm(aFile)
rm(aDataFrame)

##################################################################################################
# Build a simple lookup function for CPU performance. Assume the data will be pulled from the same 
# URL, and that data is updated on a frequent basis. Given a vendor name turn the following results:
#   - A message if the vendor is not found (based on previous experience in the data)
#   - Vendor published relative performance (PRP) and estimated relative performance (ERP)
#   - If the vendor is the best in category, return a flag that they are the best. For example, "Best 
#     in Published Relative Performance".
#   - If they are not the best in category, return the vendor is the best including the PRP and ERP 
#     numbers.
##################################################################################################
lookupVendor <- function (aURL, aFileName, aHeader, aVendor) {
  
  # Fresh data is pulled everytime due to the potential of frequent updates  
  aFile <- aUrlFile(aURL,aFileName,aHeader)
  aLocation <- which(aFile$Vendor_Name == aVendor, arr.ind=TRUE)
  
  if (length(aLocation) == 0) {
    aResult <- paste(aVendor," not found!\n",sep="")
  } else {
    
    # Get the highest scores for the requested Vendor
    aVendorSubset <- aSubSet <- aFile[aLocation,]
    aVendorPRP <- aVendorSubset$PRP[which.max(aVendorSubset$PRP)]
    aVendorERP <- aVendorSubset$ERP[which.max(aVendorSubset$ERP)]
    
    bestPRP <- which.max(aFile$PRP)
    
    aResult <- paste("Highest ", aVendor," scores\n",sep="")    
    aResult <- paste(aResult,"Published Relative Performance: ",aVendorPRP,sep="")
    if (any(aLocation == bestPRP)) {
      aResult <- paste(aResult,"\t(Best Vendor!)\n",sep="")
    } else {
      aResult <- paste(aResult,
                       "\t(",
                       aFile$Vendor_Name[bestPRP],
                       " is the Best with a PRP of ",
                       aFile$PRP[bestPRP],
                       ")\n",sep="")
    }
    
    aResult <- paste(aResult,"Estimated Relative Performance: ",aVendorERP,sep="")
    bestERP <- which.max(aFile$ERP)
    if (any(aLocation == bestERP)) {
      aResult <- paste(aResult,"\t(Best Vendor!)\n",sep="")
    } else {
      aFile$Vendor_Name[bestERP]
      aResult <- paste(aResult,
                       "\t(",
                       aFile$Vendor_Name[bestERP],
                       " is the Best with an ERP of ",
                       aFile$ERP[bestERP],
                       ")\n",sep="")    
    }
  }
  return(aResult)
}

# Test
aLookup <- lookupVendor(aTempURL,aTempFileName,aTempHeader,"ibm")
cat(aLookup)
aLookup <- lookupVendor(aTempURL,aTempFileName,aTempHeader,"sperry")
cat(aLookup)
aLookup <- lookupVendor(aTempURL,aTempFileName,aTempHeader,"amdahl")
cat(aLookup)
aLookup <- lookupVendor(aTempURL,aTempFileName,aTempHeader,"dell")
cat(aLookup)
aLookup <- lookupVendor(aTempURL,aTempFileName,aTempHeader,"hp")
cat(aLookup)
