# Week 2 Introduction - Data 710 Introduction to R Programming
# Matrices, and Arrays
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################


#  ASSIGNMENT - Binary time predictor
#     Binary - one of two possible outcomes (0,1), (TRUE, FALSE), (HEADS, TAILS)
#     Discrete value - Individually separate and distinct.  Opposite of continuous
#     Time series

a <-c(0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,1,1,0,0)  # This is the series
b <- 4

#  One possible solution.... pseudo code

#  Let's say I want to predict the Fourth element
#  1.  Determine the length of the vector
#  2.  Figure out what half of the length is for that vector
#  3.  Create a placeholder.  Set initial position to (total vector length - element you want to predict.  In this case 4)
#  4.  Store the sum of values from element 1 to element you want to predict.  In this case 4 so (0,1,0,0) = 1
#  5.  If that sum is greater than the half mark, set predictor to 1.  Otherwise set predictor to 0
#  6.  Create a for loop to iterate through the series.  Again setting the predictor to 1 or 0 as appropriate
#  7.  Ensure you include safety checks to make sure you don't get negative numbers or numbers greater than 1
#  8.  If using this technique, you return the mean of the absolute value of values observed.  ABS will make sure you don't have a negative

time_predictor(a,b)








