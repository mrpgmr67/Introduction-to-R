# Week 2 Introduction - Data 710 Introduction to R Programming
# Vectors, Matrices, and Arrays
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

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

