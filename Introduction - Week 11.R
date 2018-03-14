###################################################################################
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 11 - Optimization, Performance Tuning, and Debugging
###################################################################################


# Setting Validation Rules using Validate and Magrittr
# Validate is the core library
# Magrittr is a support library that helps if you want to use pipes

library(validate)
library(magrittr)

# Let's check to see if all values are > 0
datacheck_01 <- check_that(iris, Sepal.Length > 0, Sepal.Width > 0, Petal.Length >0, Petal.Width > 0 )
summary(datacheck_01)
barplot(datacheck_01, main="Data Check - All Values > 0")

# Let's check for all values > 3
datacheck_02 <- check_that(iris, Sepal.Length > 3, Sepal.Width > 3, Petal.Length > 3, Petal.Width > 3 )
summary(datacheck_02)
aggregate(datacheck_02)
barplot(datacheck_02, main="Data Check - All Values > 3")

# Use the Magrittr package
iris %>% check_that(Sepal.Width > 3, Sepal.Length > 3, Petal.Width > 3, Petal.Length > 3) %>% summary()

# Create Rule Sets with a Validator object
valid_iris_rules <- validator(Sepal.Width >0, Sepal.Length > 0, Petal.Length > 0, Petal.Width > 0)
valid_iris_rules

# Confront is how you check against validation rules
sample_confront <- confront(iris,valid_iris_rules)
sample_confront

summary(sample_confront)

# System time example
x <- runif(1000000)
y <- runif(1000000)
z <- vector(length=100000)

system.time(z <- x + y)
system.time(for (i in 1:length(x)) z[i] <- x[i] + y[i])

# Create a warning function
error_warning <- function(){
  #  Do Something if you get a warning
  return( )
}

# Create an error function
error_TrueError <- function(){
  # Do something if you truly get an error
  return()
}

# Example of calling an error handling routine
result = tryCatch(
{
  system.time(z <- x + y)}       # This is the expression we will test
  , warning = error_warning()    # If it is a warning, use the warning function
  , error = error_TrueError()    # If it is an error, use the error function
  , finally = {                  # Else, use this to clean it up or ignore it
  # cleanup code 
})

result   # Execute the code including the Try/Catch error handling routine
  

