##########################################################################
# Data 710: Introduction to R
# Week 1 - Installing R, R-Studio, and Basic Language Introduction
##########################################################################
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

# Return the working directory
getwd()

# Display the search path
search()

# Using the library() command, list out the sample datasets that are included with the R and R-Studio installation
library(help = "datasets")

# Run a help command on one of the sample datasets
help("airquality")

# Use the summary() command to generate descriptive statistics
summary(airquality)

# Use the R built-in function to return the first few records of your sample dataset.
head(airquality)

# Use a hist() command to create a histogram of one of the sample datasets
hist(airquality$Temp)

# Use library() command to use the pastecs library, and then use the stat.desc(DATASET) to generate more advanced descriptives using the library
library(pastecs)
stat.desc(airquality)

# Using the read.csv() function, read the sample text file listed below into a variable
setwd("D:/Davenport/Data 710 Scripts")
mySampleText <- read.csv("sample.txt", header=TRUE, sep="\t")
