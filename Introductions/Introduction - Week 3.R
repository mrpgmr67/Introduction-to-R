# Week 3 Introduction - Data 710 Introduction to R Programming
# Lists 
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

# Create a simple list using the vector command
a <-vector(mode="list")
a[["abc"]] <- 3
a

# Create a simple list of Name, ID, and whether or not they are an employee
a <- list(name="Sam", ID=123, employee=TRUE)
a

# Print off the ID of list a using all three method
a$ID
a[["ID"]]
a[[2]]

# Add another item in the list for salary and print off to verify
a$salary <- 50000
a

# Return the names of the lists
names(a)

# Return the values
unlist(a)

# Populate list with five records
a[[2]] <-c(seq(1:5))
a[[1]] <- c("Dave", "Pete", "Al", "Steve", "Lou")
a[[3]] <- c(TRUE,TRUE,FALSE,TRUE,FALSE)
a$salary<-c(50000,10000,20000,50000,90000)

a

# Use lapply function (similar to apply function earlier)
lapply(a$salary,mean)

# Create a new vector using the mean of list a$salary
b<- sapply(a$salary,mean)
b

###################################################################################################################
#  Here is a good rundown of the various apply functions
#
#  https://www.guru99.com/r-apply-sapply-tapply.html
#
#  apply(x, MARGIN, function_name)
#    x = array or matrix
#    MARGIN = 1 for rows or 2 for columns.  If you use c(1,2) it will apply to both
#    function_name = name of function
#
#  example:   m1 <- matrix(c<-(1:10), nrow=5, ncol=6)
#             apply(m1, 2, sum)     performs a sum of the column
#
#  lapply(x, function_name)         Think l means list!
#    x = vector or object
#    function_name = name of function
#
#  example:  lapply(c("ONE","TWO","THREE"), tolower)  would result in "one", "two", "three"
#
#  you can also use unlist() to turn a list into a vector!
#
#  example:  unlist(lapply(c("ONE","TWO","THREE"), tolower))   results in a vector of "one" "two" "three"
#
#  sapply(x,function_name)   does the same job as lapply but returns a vector automatically
#
#  example:  sapply(cars, min)     use the cars dataset and perform a min on each element.  Return a vector