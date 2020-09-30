#####################################################################################
# 
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 4 - Factors and Tables
#
#   GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

#
#    For apply functions
#    https://www.guru99.com/r-apply-sapply-tapply.html
#

any_vector <- c(5,12,13,12, 10, 15)

# Create a factor  - Question 1
sample_factor <- factor(any_vector)

#  Let's look at the structure
str(sample_factor)
unclass(sample_factor)

any_vector_categories <- c("K", "M", "I", "I", "I", "M")

#  Note lengths must be the same
length(sample_factor)
length(any_vector_categories)

#  Use tapply() on the factor - Question 1

# tapply() - compute a measure or a function for each factor variable in a vector
#            Create a subset of a vector and then apply some function to each of the subset
#
#    tapply(x, index, fun=NULL)
#         x:  an object / usually a vector
#         index:  a list containing a factor
#         fun:  the function to apply to each element in X
#    x = any_vector
#    index = any_vector_categories
#    fun = mean  (could be mean, median, min, max, etc.)
tapply(any_vector, any_vector_categories, mean)

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)  # calculate the median of the sepal.width for each species in Iris


# Use Split Function - Question 1
#
#  split(x, f)     list the data in vector x into groups defined by f
#
# Break the air quality dataset by month
split(airquality, airquality$Month)

#  Create a list and populate it with sample data.  Then create a table from the list - Question 2
new_vector <- c(2,3,4,5)
new_list <- list(c(4,2,4,5), c("A", "B", "B", "C"))
tapply(new_vector, new_list, length)

new_table <-table(new_list)
addmargins((new_table))

#
#  Create a sub table function - Question 3
#
# Must create the sub table function
# - You are given the function
# - Explain what is happening inside of this function
# - Leave comments after each statement
subtable <- function(tbl, subnames){
  tblarray <-unclass(tbl)
  dcargs <-list(tblarray)
  ndims <-length(subnames)
  for (i in 1:ndims){
    dcargs[[i+1]]<-subnames[[i]]
  }
  subarray <-do.call("[", dcargs)
  dims <-lapply(subnames, length)
  subtbl <-array(subarray,dims,dimnames=subnames)
  class(subtbl) <- "table"
  return(subtbl)
}
subtable(new_table, list(new_list == "A"))

# Aggregate function - Question 4
#  Split the data into a subset and compute summary statistics 
#  Aggregate (x, by, fun)
#     - x:  Object
#     - by:  List of grouping elements
#     - Fun:  Function to apply to it
aggregate(new_vector,new_list,mean)

# Using the state.x77 dataset
aggregate(state.x77, list(Region = state.region), mean)

# Cut function (cut the table into two breaks without a label) simulating a coin toss - Question 4
#  Great for turning a continuous variable (such as age) into a categorical variable
#
iris_table <-table(cut(iris$Sepal.Width, breaks = 5))
addmargins(iris_table)


# Create a new table based on Experiments on Plant Growth and perform basic statistics using an aggregate() - Question 5
# - create an aggregate() and use a function 
#  
# - Mean, SD, Var, Length, Summary

aWidth <- iris$Sepal.Length
aGroup <- iris$Sepal.Width
myPlantGrowth <- table(flower_width=aWidth,Group=aGroup)
addmargins((myPlantGrowth))

aggregate(aWidth,list(aGroup),summary)

aggregate(. ~ Species, data = iris, mean)   # Uses the dot notation

