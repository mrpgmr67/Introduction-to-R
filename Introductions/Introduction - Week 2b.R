# Week 2 Introduction - Data 710 Introduction to R Programming
# Matrices, and Arrays
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################


#  ASSIGNMENT - Binary time predictor
#     Binary - one of two possible outcomes (0,1), (TRUE, FALSE), (HEADS, TAILS)
#     Discrete value - Individually separate and distinct.  Opposite of continuous
#     Time series

a <-c(0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,1,1,0,0)  # This is the series
b <- 4

#  One possible solution.... pseudo code

#  Let's say I want to predict the Fourth element
#  1.  Determine the length of the vector
#  2.  Figure out what half of the length is for that vector
#  3.  Create a placeholder.  Set initial position to (total vector length - element you want to predict.  In this case 4)
#  4.  Store the sum of values from element 1 to element you want to predict.  In this case 4 so (0,1,0,0) = 1
#  5.  If that sum is greater than the half mark, set predictor to 1.  Otherwise set predictor to 0
#  6.  Create a for loop to iterate through the series.  Again setting the predictor to 1 or 0 as appropriate
#  7.  Ensure you include safety checks to make sure you don't get negative numbers or numbers greater than 1
#  8.  If using this technique, you return the mean of the absolute value of values observed.  ABS will make sure you don't have a negative

#Create and execute an R function named time_predictor(a,b) that can be used to predict a
#binary, discrete-valued time series.
a<-c(0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,1,1,0,0)  # This is the series
b<-4                                          
time_predictor <- function(a,b) {
  n <- length(a)                       # N will store the length of the string                    
  b2 <- b/2                                   
  pred <- vector(length=n-b)                  
  sm <- sum(a[1:b])                           
  if (sm>=b2) pred[1] <-1 else pred[1] <-0    
  if (n-b>=2) {                               
    for (i in 2:(n-b)) {                      
      sm <- sm + a[i+b-1] - a[i-1]              
      if (sm>=b2) pred[i] <- 1 else pred[i] <- 0  
    }
  }
  return(mean(abs(pred-a[(b+1):n])))              
}
time_predictor(a,b)
#[1] 0.6129032


# Create and execute an R function to generate a covariance matrix. It should be symmetric.
# Use rho=0.5 and n=5.
#
# Simply put, a 5x5 matrix where the lowest value is 0.5 and the top value is 1.0 

# A covariance matrix is a matrix whose element in the i, j position is the covariance between the i-th and j-th elements 
# of a random vector. A random vector is a random variable with multiple dimensions. Each element of the vector is a scalar 
# random variable. Each element has either a finite number of observed empirical values or a finite or infinite number of 
# potential values. The potential values are specified by a theoretical joint probability distribution.
# The covariance matrix generalizes the notion of variance to multiple dimensions. 

#[,1] [,2] [,3] [,4] [,5]
#[1,]  1.0  0.5  0.5  0.5  0.5
#[2,]  0.5  1.0  0.5  0.5  0.5
#[3,]  0.5  0.5  1.0  0.5  0.5
#[4,]  0.5  0.5  0.5  1.0  0.5
#[5,]  0.5  0.5  0.5  0.5  1.0


#Create and execute an R function to identify outliers. Use the observation that is furthest
#from the median to identify an outlier. Use the matrix created in the previous step.
findols <- function(m) {           
  findol <- function(mrow) {       
    mdn <- median(mrow)            
    devs <- abs(mrow-mdn)          
    return(which.max(devs))        
  }
  return(apply(m,1,findol))        
}











