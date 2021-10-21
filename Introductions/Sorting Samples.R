attach(mtcars)

# Method 1 - Using Order on a dataset
order_test <- mtcars[order(mpg),]

# Method 2 - Using Sort
sort_test <- sort(mpg)

# Method 3 - Using dplyr
library(dplyr)

dplyr_sort <- arrange(mtcars, mpg)

# Method 4 - Using Bubble Sort Manually created
#
#    Note:  This was taken from StackOverflow: https://stackoverflow.com/questions/36051165/bubble-sort-using-r-language  
x<-sample(1:100,10)         # Create a sample
example <- function(x){    
  n<-length(x)              # Determine total length of sample
  for(j in 1:(n-1)){        # For-Next loop to iterate through the sample (external loop)
    for(i in 1:(n-j)){      # For-Next loop to iterate through the sample (internal loop)
      if(x[i]>x[i+1]){      # This statement is determining if the value is greater than the next one
        temp<-x[i]          # if it is, then store that in a temporary variable
        x[i]<-x[i+1]        # Make the first element equal to the second element
        x[i+1]<-temp        # Make the second element equal to the temp.  This essentially swaps the two values
      }
    }
  }
  return(x)
}
res<-example(x)             # Function call to test it out
#input
x                           # Original Value
#output
res                         # Sorted value.  It passes the test

detach(mtcars)