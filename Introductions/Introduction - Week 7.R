##########################################################################
# Data 710: Introduction to R
# Week 7: Assignment - Object Oriented Programming
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files

# Create a S3 class
demo_names <- list(name="Mike", ID=1, Instructor = TRUE)         # Create a sample list
class(demo_names) <- "Data_710"               # Create a sample class based on that list

attributes(demo_names)     # Notice it is now of the class Data_710

print.Data_710 <- function(davenport_person){         # Create a print function
  cat(davenport_person$name,"\n")
  cat(davenport_person$ID,"\n")
  cat(davenport_person$Instructor,"\n")
}

#You will get a warning with this statement
methods(,"Data_710")      # Check to make sure the class now has a print function attached to it

# UseMethod is one way to make the warning go away
x <- structure(1, class = letters)
Data_710 <- function(x) UseMethod("Data_710", x)
Data_710.z <- function(x) "z"
Data_710(x)
methods("Data_710")


# Inheritance
demo_two <- list(name="Kimberly", ID=2, Instructor=FALSE, GPA=4.0)
class(demo_two) <- c("students", "Data_710")

attributes(demo_two)  # Check to make sure it belongs to both classes now (students and Data 710)


# Define the S4 class
setClass("Davenport_Alumni",               # Name of the class
         representation(
           name = "character",             # First element and type
           ID = "numeric",                 # Second element and type
           Instructor = "logical")         # Final element and type
         )

demo_three <- new("Davenport_Alumni", name="Steve", ID=3, Instructor=FALSE)    # Create an instance of the S4 class


demo_two$name     # Reference an element within a S3 class
demo_three@name   # Notice the @ sign for reference to an element within a S4 class is different
