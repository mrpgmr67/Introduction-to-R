##########################################################################
# Data 710: Introduction to R
# Week 2 - Create an R Script that features vectors, matrices and arrays
##########################################################################
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

#Compute basic statistics for two vectors (x and y). Include the mean, median, standard
#deviation, variance, correlation (pearson's or spearman), and covariance.
x<-c(0,2,2,4,6,10,16,26,42,68)
y<-c(1,3,6,10,15,21,28,36,45,55)
mean(x)
#[1] 17.6
mean(y)
#[1] 22
median(x)
#[1] 8
median(y)
#[1] 18
sd(x)
#[1] 22.06657
sd(y)
#[1] 18.5652
var(x)
#[1] 486.9333
var(y)
#[1] 344.6667
cor(x,y)
#[1] 0.949273
cov(x,y)
#[1] 388.8889


#Create and execute an R function named time_predictor(a,b) that can be used to predict a
#binary, discrete-valued time series.
a<-c(0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,1,1,0,0)  # This is the series
b<-4                                           # let's predict the fourth element
time_predictor <- function(a,b) {
  n <- length(a)                               # Figure the length of (a) and store it in variable n
  b2 <- b/2                                    # knowing that you are looking at b elements, find half of that 
  pred <- vector(length=n-b)                   # create your placeholder by subtracting predictor from length and storing in vector
  sm <- sum(a[1:b])                            # hold the sum of the series from 1 to b
  if (sm>=b2) pred[1] <-1 else pred[1] <-0     # safety catch if placeholder is less than one
  if (n-b>=2) {                                # another safety catch to ensure values are in appropriate range
    for (i in 2:(n-b)) {                              # loop to iterate through the series
      sm <- sm + a[i+b-1] - a[i-1]                    # recalculate the sum
      if (sm>=b2) pred[i] <- 1 else pred[i] <- 0      # set predictor to either 1 or 0
    }
  }
  return(mean(abs(pred-a[(b+1):n])))                  # return the mean(abs(placeholder - series position))
}
time_predictor(a,b)
#[1] 0.6129032


#Create and execute an R function to do a simple recode of data within a vector.
v<-c("D","A","T","A","D","A","T","A")
v
#[1] "D" "A" "T" "A" "D" "A" "T" "A"
ifelse(v == "D",1,ifelse(v == "A",2,3))
#[1] 1 2 3 2 1 2 3 2


# Create and execute an R function to generate a covariance matrix. It should be symmetric.
# Use rho=0.5 and n=5.
#
# Simply put, a 5x5 matrix where the lowest value is 0.5 and the top value is 1.0 

# A covariance matrix is a matrix whose element in the i, j position is the covariance between the i-th and j-th elements 
# of a random vector. A random vector is a random variable with multiple dimensions. Each element of the vector is a scalar 
# random variable. Each element has either a finite number of observed empirical values or a finite or infinite number of 
# potential values. The potential values are specified by a theoretical joint probability distribution.
# The covariance matrix generalizes the notion of variance to multiple dimensions. 
rho<-0.5
n<-5
makecov <- function(rho,n) {
  m <-matrix(nrow=n,ncol=n)            # Create a 5x5 matrix
  m <-ifelse(row(m) == col(m),1,rho)   # if the row and column are equal set to 1.0, otherwise set to 0.5
  return(m)                           # return the matrix
}
m=makecov(rho, n)
m
#[,1] [,2] [,3] [,4] [,5]
#[1,]  1.0  0.5  0.5  0.5  0.5
#[2,]  0.5  1.0  0.5  0.5  0.5
#[3,]  0.5  0.5  1.0  0.5  0.5
#[4,]  0.5  0.5  0.5  1.0  0.5
#[5,]  0.5  0.5  0.5  0.5  1.0


#Create and execute an R function to identify outliers. Use the observation that is furthest
#from the median to identify an outlier. Use the matrix created in the previous step.
findols <- function(m) {           # wrapper function (so we can excute findol multiple times).  Takes matrix as parameter
  findol <- function(mrow) {       # the actual function.  Takes the row as a parameter
    mdn <- median(mrow)            # calculate the median of the row
    devs <- abs(mrow-mdn)          # calculate the absolute value of the difference between the row and median
    return(which.max(devs))        # return the max of the deviations
  }
  return(apply(m,1,findol))        # This is where the magic happens. In the return call the function
                                   # m is the matrix
                                   # 1 means the dimension code is a row (remember 2 is a column)
                                   # use the findol function that you just created above
}
findols(m)

# Create another matrix.  Look for outliers that are the furthest from the median
m <- matrix(c(1,2,3,4,5,6,4,3,2,4,1,3,1,5,6,4), nrow=2, byrow=T)
median(m)
findols(m)
