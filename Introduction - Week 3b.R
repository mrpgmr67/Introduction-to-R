# Week 3 Introduction - Data 710 Introduction to R Programming
# Data Frames

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