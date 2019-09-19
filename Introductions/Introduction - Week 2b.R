# Week 2 Introduction - Data 710 Introduction to R Programming
# Matrices, and Arrays
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

# Create a simple matrix
m <- matrix(c(1,2,3,4,5,6), nrow=2, byrow=T)   # Six elements, two rows

m

# Use the structure function to describe it
str(m)

# Use the apply function to generate a mean
# m = vector
# 2 = dimcode (1 for row and 2 for column)
# mean = function
# fargs = arguments (not used in this example)
apply(m,2,mean)

# Create a new matrix by binding together c and d  (cbind for column / rbind for row)
f <- cbind(c,d)   # Bind columns
f1 <- rbind(c,d)  # Bind rows

# Bind names to the columns for f of Start and End
colnames(f) <-c("Start", "End")

# Return the attribute names of f
attributes(f)

# Create an array of both matrices 
g <- array(data=c(m,f),dim=c(3,2,2))

attributes(g)

# Print off second column of matrix
g[,2,]