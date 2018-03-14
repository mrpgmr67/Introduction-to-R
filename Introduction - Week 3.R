# Week 3 Introduction - Data 710 Introduction to R Programming
# Lists 

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