##########################################################################
# Data 710: Introduction to R
# Week 11: Assignment - Optimization, Performance Enhancement, and Debugging
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")        # Don't forget to set to your working directory to find the files

# Create a function with error handling.

# This function will report if there is an error in the form of having a number in a vector
# that is below a certain threshold.

summary(rivers)
?rivers

error_test <- function(vector, threshold) {
  if (min(vector) < threshold) {
    cat("Error: At least one number is below the established threshold")
  } else {
    cat("No Error")
  }
}

# I will use it to test if any of the major North American rivers are less than 100 miles.

error_test(rivers,100)
# No Error

# Now I will test with 1,000 miles.

error_test(rivers,1000)
# Error: At least one number is below the established threshold



### COMPONENT B ###

# Create a function that uses the validate and magrittr package to validate data.

library(validate)
library(magrittr)

summary(trees)
?trees

# Using the Magrittr package. 

# This will test that the data points are greater than 0 and should return no fails:

trees %>% check_that(Girth > 0, Height > 0, Volume > 0) %>% summary()

#   name items passes fails nNA error warning expression
# 1   V1    31     31     0   0 FALSE   FALSE  Girth > 0
# 2   V2    31     31     0   0 FALSE   FALSE Height > 0
# 3   V3    31     31     0   0 FALSE   FALSE Volume > 0

# This will test that the data is greater than 15 and should show some fails in the categories of 
# Girth and Volume:

trees %>% check_that(Girth > 15, Height > 15, Volume > 15) %>% summary()

#   name items passes fails nNA error warning  expression
# 1   V1    31      8    23   0 FALSE   FALSE  Girth > 15
# 2   V2    31     31     0   0 FALSE   FALSE Height > 15
# 3   V3    31     28     3   0 FALSE   FALSE Volume > 15


# Using the validate package to set rules.

tree_validation <- validator(Girth > 0, Height > 0, Volume > 0)
tree_validation

# Object of class 'validator' with 3 elements:
#  V1: Girth > 0
#  V2: Height > 0
#  V3: Volume > 0

# Testing the rules.
confront_trees <- confront(trees,tree_validation)
confront_trees

# Object of class 'validation'
# Call:
#   confront(dat = trees, x = tree_validation)

# Confrontations: 3
# With fails    : 0
# Warnings      : 0
# Errors        : 0


### COMPONENT C ###

# Using a previous simulation built in class, perform an optimization on it.


# I will use my simulation of the roulette wheel once again.

Roulette<-c(0:36)
sample_size<-100
repeat_samples<-10000          # I increased the repeat samples from 1,000 to 10,000 to better display 
# the time difference in the optimization.

# I have tweaked the for loop to generate one vector combining all of the simulations.

for(i in 1:repeat_samples){
  clt_y<-sample(Roulette,size=sample_size,replace=TRUE)
  if (exists("loop_wheel")==TRUE){
    clt_z <- c(clt_y)
    loop_wheel <- rbind(loop_wheel,clt_z)
    rm(clt_z)
  }else{
    loop_wheel <- c(clt_y)
  }
}
length(loop_wheel)             # Test to make sure length is 1,000,000
mean(loop_wheel)               # Mean should be close to 18


# This simulation could be run as a vector:

vector_wheel <- c(sample(Roulette,size=sample_size*repeat_samples,replace=TRUE))
length(vector_wheel)           # Test to make sure length is 1,000,000
mean(vector_wheel)             # Mean should be close to 18


# Now to show the time it takes to run each of those functions:

system.time(for(j in 1:repeat_samples){
  clt_y<-sample(Roulette,size=sample_size,replace=TRUE)
  if (exists("sample_x")==TRUE){
    clt_z <- c(clt_y)
    sample_x <- rbind(sample_x,clt_z)
    rm(clt_z)
  }else{
    sample_x <- c(clt_y)
  }
})

#    user  system elapsed 
#   10.72    0.15   10.90         <--- Hope you enjoyed the nap

system.time(vector_wheel <- c(sample(Roulette,size=sample_size*repeat_samples,replace=TRUE)))

#    user  system elapsed 
#    0.01    0.00    0.02         <--- Much faster