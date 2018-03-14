#####################################################################################
# 
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 9: String Manipulation
#
#####################################################################################

#  This example will use the PDF Tools library (pdftools)
#  You can install it with the command install.packages("pdftools")
#  and then using a libary(pdftools)
#
library(pdftools)
sample_data <- pdf_text("sample.pdf")

nchar(sample_data)      # Find the length of the string

new_sample_data <- paste("Give to students: ", sample_data)   # Concat string "Give to students: " to the front
new_sample_data

a <- grep("analysis",sample_data, ignore.case = TRUE)     # Returns character vector
b <- grepl("analysis",sample_data, ignore.case = TRUE)    # Returns logical vector
c <- gregexpr("analysis",new_sample_data)                 # Find every place in the string that the word "analysis" is used
d <- regexec("analysis", sample_data, ignore.case = TRUE) # Returns the position of the first instance, or -1 if nothing found

course_title <- substr(new_sample_data, 20,71)   # Extract the course title from the string
course_title