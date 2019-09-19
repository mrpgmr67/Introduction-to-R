##########################################################################
# Data 710: Introduction to R
# Week 3 - Lists and Data Frames
##########################################################################
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files

# Create an R script with the following components:
#   Create a list (test_list) that has three fields:
#      ID - numeric 1 to 100
#      Category - Character
#      V_Input - Boolean (True/False)
test_list <- list(ID=1,
                  Category="My Category",
                  V_Input=TRUE)

# Populate it with sample data for 100 records
test_list[["ID"]] <- c(seq(1:100))                                               # Using a sequence to generate 100 IDs in order
test_list[["Category"]] <- rep(c("Cat - A","Cat - B","Cat - C","Cat - D"), 25)   # Using the rep() function to replicate     
test_list[["V_Input"]] <- rep(c(TRUE,FALSE,FALSE,TRUE,TRUE), 20)

test_list

# Create and execute an R function to populate the V_Input with a TRUE value if the ID is odd and 
# a FALSE value if the ID is even
popInput <- function(aList) {
  x<-length(aList$ID)                                                     # Store the length of the list
  for (i in 1:x) {                                                        # Use a for loop to iterate
    aList$V_Input[i] <- ifelse((aList$ID[i] %% 2) == 0, FALSE, TRUE)      # Replace the V_Input with True or False
  }
  return(aList)                                                           # Return the list
}

# Before V_Input change               
test_list$V_Input                        # Test Statements
test_list <- popInput(test_list)         # Test Statements
test_list$V_Input                        # Test Statements

# Create and execute an R function that will scan through a text file to determine word frequencies. 
# Use a barplot() to display the results
countWords <- function(textFile) {
  
  aFile <- scan(textFile,"")                   # using "" will let it use the standard input, so you don't need to give file name
                                               # scan() reads the textFile in from the parameter on the function
  aFile <- sapply(aFile, tolower)              # convert to lower case for case-insensitivity
                                               # sapply(file_name, function_name)
                                               #   takes a list, vector, or data frame in file_name
                                               #   applies function function_name to it
                                               #   returns a vector or matrix
  
  # A simplistic clean up the data to remove non-words (i.e. numbers, special characters, etc)
  aFile <- subset(aFile,aFile >= "a")          # subset() returns a subset of the data.  There are a bunch of parameters that can
                                               #    be used; however, we only want the input name and the logical expression for
                                               #    the subset.  In this case aFile = stream and aFile >= "a" is the subset
  aFile <- gsub("[^a-z]", "", aFile)           # gsub(old_string, new_string, file_name)   replaces all instances of the 
                                               #    old_string with new_string in file_name
                                               #    also useful is the sub() function which only replaces the first instance
                                               # Using a wildcard (similar to what you would in GREP) to pick up non characters
  
  aFile <- sort(aFile)                         # Sort the output so like words are together alphabetically
  
  word <- unique(aFile)                        # Unique removes duplicates
  wordFreq <- rep(0,length(word))              # Set word frequency counter to zero
  aResult <- data.frame(word,wordFreq)         # Populate word list and pre-allocate data structure
  
  # Initialize variables for the loop
  aCount <- 1
  wordIndex <- 1
  
  for(i in 2:length(aFile)) {                  # iterate through the file
    if (aFile[i] != aFile[i-1]) {              # Make sure you are on a word and not at the end of the file
      aResult$wordFreq[wordIndex] <- aCount    # Build your result file
      wordIndex <- wordIndex + 1               # Increment your index position
      aCount <- 1                              # Reset your counter
    } else {
      aCount <- aCount+1                       # Increment your counter
    }  
    aResult$wordFreq[wordIndex] <- aCount      # Set your word count for that word to the counter
  }
 
  return(aResult)                              # Return the result
}

# Test statements
wordCnt <- countWords("text_file.txt")                              # Load test_file.txt into the function
barplot(wordCnt$wordFreq,xlab="Words", ylab="Frequency (count)" )   # Simple bar blot of word frequencies
str(wordCnt)                                                        # Check the structure of the word count
head(wordCnt, 20)                                                   # Check the first 20 records using a head()

# Create a simple linear regression on the sample dataset included with R dealing with 
# the "growth of Orange Trees". Include an xyplot() of the age - circumference, and the summary() of 
# the output from the regression.
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
