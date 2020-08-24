#####################################################################################
# 
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 6 - Introduction to Simulations
#
#   GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################
setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files

##################################################################################################
# Probably the most challenging parts is the discrete distribution probability and the quantile
# splits.  Here are examples of each
##################################################################################################

# Long way to do it....
myBinomialPD <- function(x,n,p) {
  
  aResultSet <- vector(mode='numeric',length=length(x))
  
  for (i in 1:length(x)) {
     aResultSet[i] <- (factorial(n)/(factorial(x[i])*factorial(n-x[i])))*(p^x[i])*(1-p)^(n-x[i])
  }
  
  return(aResultSet)
}
# HINT:  There is a much easier way using one of the standard R libraries

# Long way to do it....
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
# HINT:  There is a much easier way using one of the standard R libraries