##########################################################################
# Data 710: Introduction to R
# Week 4 - Factors and Tables
##########################################################################
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files

##################################################################################################
# Create a factor named 'x' and populate it with some sample data. Use the tapply() and split() 
# functions against the factor.
##################################################################################################

# simplest form of split:
# split(x, f)
# ARGUMENTS:  
#     x = vector or data frame to divide into groups
#     f = factor that defines the grouping
x <- factor(c("MI","IL","MN","IN","IL","OH","MI","OH","MN","MI","MI","MI","OH","IN","OH"))
after_split <- split(1:15,x)   # using the factor of states, split by the position of the state (example IL is in the 2 and 5 place)
unsplit(after_split, x)

###################################################################################################################
#  Here is a good rundown of the various apply functions
#
#  https://www.guru99.com/r-apply-sapply-tapply.html
#
#  tapply(X, INDEX, FUN = NULL)
#  Arguments:
#    -X: An object, usually a vector
#    -INDEX: A list containing factor
#    -FUN: Function applied to each element of x
#  tapply(iris$Sepal.Width, iris$Species, median)

computed_measure <- tapply(iris$Sepal.Width, iris$Species, median)


##################################################################################################
# Create a list() and populate it with sample data. From that list, create a new table.
##################################################################################################

students <- rep(c("Joe","Kat","Sam","Tim","Kelly","Frank","Sally","Tom"),3)
grades <- c(4,3,3,2,4,1,3,3,4,3,3,2,4,4,3,1,3,2,4,4,4,3,2,3)
aList <- list(Student=students,Grades=grades)
gradeBook <- table(aList)
gradeBook

##################################################################################################
# Create a sub-table.
# Adapted from The Art of Programming by Rorman Matloff (page 132-133)
##################################################################################################
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
# Pseudo Code 
# use unclass on the table to bust it apart
# create a list on the result
# keep the length of the substitute names
# update your list with the substitute names
# 
# read up on do.call:   https://www.stat.berkeley.edu/~s133/Docall.html
#
# list apply the subarray you just created
# populate a hold variable (subtbl) with an array of values.  Essentially assembling the table
# class() will turn the subtable back into a table


# Frank was removed from the gradebook.  Grades with a "1" were also dropped.
aRevisedGradeBook <- subtable(gradeBook,                                                    # Remember Gradebook is a table
                              list(Grades=c("Joe","Kat","Sam","Tim","Kelly","Sally","Tom"), # First half of list is names 
                              c(2,3,4)))                                                    # Second half of list is grade
aRevisedGradeBook
##################################################################################################
# Use the aggregate() and cut() functions on the table.
##################################################################################################
# aggregate(object, group_by, function)

# Frequency of grades handed out
aggregate(students,list(grades),length)

# Count of grades per student
aggregate(grades,list(students),length)

###########################
# cut(numeric_vector, break_points, labels)
# divides numeric_vector into intervals and codes values according to the interval they fall

# Put Grades into Bad (0-1), Okay(2), and Good(3+) categories
myBinLabels <- c("Bad","Okay","Good")
myBins <- c(0,1,2,Inf)

binMyTable <- cut(grades,breaks = myBins,labels=myBinLabels)
binMyTable

##################################################################################################
# Using the sample dataset on "results from an Experiment on Plant Growth" included with R, create a 
# new table and perform basic analysis on it. Include some standard measures of statistics as well as 
# an aggregate that could be presented showing the total count for each category.
##################################################################################################

aAge <- Orange$age
aGroup <- Orange$circumference
myPlantGrowth <- table(Age=aAge,Group=aGroup)
myPlantGrowth

aggregate(aAge,list(aGroup),mean)
aggregate(aAge,list(aGroup),sd)
aggregate(aAge,list(aGroup),var)
aggregate(aAge,list(aGroup),summary)
aggregate(aAge,list(aGroup),length)

