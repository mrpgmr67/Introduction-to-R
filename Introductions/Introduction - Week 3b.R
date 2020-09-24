# Week 3 Introduction - Data 710 Introduction to R Programming
# Data Frames
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts")                      # Don't forget to set to your working directory to find the files

#  Creating a simple data frame
x <- c(seq(1:10))
y <- seq(from=1, to=20, by=2)
z <- seq(from=1, to=100, by=10)

c <- data.frame(x,y)
d <- data.frame(y,z)

# Return the structure and the value of the x variable from data frame c
str(c)
c$x

# Merge two data frames
e <- merge(c,d)
e

# Use of the read.csv, barplot, and summary functions
sample_data <-read.csv("sample.csv")
barplot(sample_data$Amount)
summary(sample_data$Amount)



aList$V_Input[i] <- ifelse((aList$ID[i] %% 2) == 0, FALSE, TRUE)


#  THE ASSIGNMENT
#
#
#
#  1.  Create a list
#  ID - Numeric 1 to 100
#  Category - Character
#  V_Input - Boolean
#  Sample 100 records    hint:  use seq() function


#  2.  Create a function to populate V_Input with TRUE if the ID is odd and FALSE if the id is even
#
#   Use ifelse() and the modulus operator %%
#                        if modulus 2 == 0 then you know it is even

#  3.  Word frequencies
#      several possible solutions:  
#          quanteda library    http://quanteda.io/
#          tm library
#          create your own function.  
#      
#       bar plot     
#       barplot(wordCnt$wordFreq,xlab="Words", ylab="Frequency (count)" )

#  4.  read.csv()        We have done this multiple times so far

#  5.  Linear regression


library(lattice)

# Example of a good linear relationship
xyplot(iris$Petal.Length ~ iris$Petal.Width)                            # Simple scatter plot
xyplot(iris$Petal.Length ~ iris$Petal.Width | iris$Species)             # Scatter broken down by species

simpleIrisRegression <- lm(Petal.Length ~ Petal.Width, data=iris)       # Create the linear regression model

plot(iris$Petal.Length ~ iris$Petal.Width)                              # Create the initial plot
abline(simpleIrisRegression)                                            # Overlay the regression line
summary(simpleIrisRegression)                                           # Statistical Summary

# Example of a poor linear relationship
xyplot(iris$Sepal.Length ~ iris$Sepal.Width)
xyplot(iris$Sepal.Length ~ iris$Sepal.Width | iris$Species)

simpleIrisRegression <- lm(Sepal.Length ~ Sepal.Width, data=iris)

plot(iris$Sepal.Length ~ iris$Sepal.Width)
abline(simpleIrisRegression)
summary(simpleIrisRegression)

