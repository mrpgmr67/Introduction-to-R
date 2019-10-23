##########################################################################
# Data 710: Introduction to R
# Week 8: Assignment - Input / Output
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")                      # Don't forget to set to your working directory to find the files
#
#  Sample.txt and Sample.CSV are both in the working directory.  If they are not, you would either have
#  to include the path to get there or you would have to set your working directory to the location of the files
#
#  Use Scan to check sample.txt file
scan("sample.txt", what="")      # Sample.txt contains characters
scan("sample.txt")               # Example of what happens if it has characters but no "what" is specified

#  Getting input from a keyboard.  Storing it in user_result
user_result <-readline("Please enter your variable name:  ")

#  Basic printing
print(user_result)          
cat(user_result)

#  Reading data into a data frame
sample_dataframe <- read.csv("sample.txt", header = TRUE,sep="\t")    #  First row is a header.  Field separated by a tab (\t)

#  Read in data from a URL
CPU_URL <-"http://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/"     # Base URL
CPU_URL <- paste(CPU_URL,"machine.data", sep="")                                          # File name and separator
CPU_data <- read.csv(CPU_URL)                                                             # Read.CSV() 

#  Saving Data out to a File
write.table(CPU_data,file="CPU_Datafile.txt", row.names = TRUE, col.names = TRUE, sep="\t", eol="\n")
write.csv(CPU_data, file="CPU_Datafile.csv")