# Week 2 Introduction - Data 710 Introduction to R Programming
# Vectors, Matrices, and Arrays
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

###############################################################################################
# Vector - All elements must be the same type.  Typically can just do a simple assign x <- 1 but can also give it parameters
#          such as specifying the length
#

# Simple Vector explicit declare (ten items)
x <- vector(length=10)

# Simple vector impled declare (sets value to 1)
y <- 1

# Simple vector using concat function 
z <- c(1,3,5,7,9,11)

# Determine the type of vector
typeof(z)

# Determine length of all three vectors and store in vector a
a <- length(c(x, y, z))

# Determine the length of each vector and store in vector b
b <- c(length(x), length(y), length(z))

# Add 10 to each element in the vector z
z <- z + 10 

# Add 1, 2, and 1 to the first three elements of z
z + c(1,2,1)

# Select 3 to 5 elements of z
z[3:5]

# Print the cumulative sum of vector z
cumsum(z)

# First create two vectors c and d using sequences
c <- seq(from=1, to=100, by=1)  # 1 to 100 
d <- seq(from=1, to=200, by=2)  # 1 to 200 by jumps of 2

c
d

# Repeat function - repeat the number 1 (10 times)
e <- rep(1,10)

e

# Check to see if any and all elements of vector z is > 17
any(z > 17)
all(z > 17)


# ASSIGNMENT - basic statistics.  You can complete by either doing the individual calls or by using one of the many library options
#              to display more advanced metrics
# mean(x)
# median(x)
# sd(x)
# var(x)
# cor(x,y)
# cov(x,y)

#  Recoding of vectors
v<-c("D","A","T","A","D","A","T","A")
v2 <- ifelse(v == "D",1,ifelse(v == "A",2,3))
v
v2

###############################################################################################
#  Matrix - vector with two additional atrributes (number of rows and columns)
#  nrow = number of rows
#  byrow = True/False   determines whether or not you want to assign by column or row
#

# Create a simple matrix
m <- matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE)   # Six elements, two rows three columns
m2 <- matrix(c(1,2,3,4,5,6), nrow=3, byrow=FALSE) # Six elements, three rows two columns

m
m2

# Use the structure function to describe it
str(m)


# Use the apply function to generate a mean
# m = vector
# 2 = dimcode (1 for row and 2 for column)
# mean = function
# fargs = arguments (not used in this example)
apply(m,2,mean)

# Create a new matrix by binding together c and d  
# This is used to combine objects by rows or columns
# (cbind for column / rbind for row)
f <- cbind(c,d)   # Bind columns
f1 <- rbind(c,d)  # Bind rows

# Bind names to the columns for f of Start and End
# This will add column names to the top of your data
colnames(f) <-c("Start", "End")

head(f)

# Return the attribute names of f
attributes(f)

#   ASSIGNMENT - Make Covariance matrix.  The output should look something like this:

#[,1] [,2] [,3] [,4] [,5]
#[1,]  1.0  0.5  0.5  0.5  0.5
#[2,]  0.5  1.0  0.5  0.5  0.5
#[3,]  0.5  0.5  1.0  0.5  0.5
#[4,]  0.5  0.5  0.5  1.0  0.5
#[5,]  0.5  0.5  0.5  0.5  1.0

#  Essentially it is an exercise to create a simple matrix.  The text has an example of doing it in a function
#  It can also be done using other methods for you to discover.



###############################################################################################
#  Arrays are similar to matrices but can have more than two dimensions
# Create an array  
dim(as.array(letters))
g <- array(1:4, c(2,4))
head(g)

attributes(g) # two rows, four columns

# Print off second column of matrix
g[,2]


###############################################################################################
#  Lists - Collections / can contain elements of different types
#
my_list <- list(name = c("Mike", "Kim"), gender = c("M", "F"), organization = "NAVAIR")
my_list

my_list[1]   # Return all names in the list


###############################################################################################
#  Data Frames - Used for creating a data table.  A list of vectors all of equal length

name <- c("Mike", "Jim", "Steve")      # Name - Person's first name
age <- c(52, 55, 50)                   # Age - Person's age
teacher <- c(TRUE, FALSE, FALSE)       # Teacher - Boolean (T/F) on whether or not they teach
df = data.frame(name, age, teacher)

df


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

time_predictor(a,b)


#  ASSIGNMENT - Remove Outliers
#
#  Possible Solutions:
#  1. Use plots
#  2. Write a function


# Produce sample quantiles
# Interquantile range = Upper - Lower 
# Set the output variable to the input starting position
# Cut the low side
# Cut the high side
# Return the output

# Do a setup like this
x <- rnorm(100)         # Generate a multivariate normal distribution 
x <- c(-10, x, 10)      # Add in two oddball outliers
y <- remove_outliers(x)

# and box plot it to show the outcome
par(mfrow = c(1, 2))
boxplot(x)
boxplot(y)

