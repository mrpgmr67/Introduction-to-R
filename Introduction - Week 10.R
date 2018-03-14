###################################################################################
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 10 - Graphics
###################################################################################

# Provide some basic summary information from Trees dataset
summary(trees)

# Setup 3 per row
par(mfrow=c(2,3))

# Basic Histograms
hist(trees$Girth)
hist(trees$Volume)
hist(trees$Height)

# The options to select the type of plot you wish to do
help("plot")
help("barplot")
help("boxplot")

# Scatter Plot Girth vs. Height
plot(trees$Girth, trees$Height, col="blue",xlab="Tree Girth", ylab="Tree Height")
abline(lm(trees$Height~trees$Girth))

# Scatter Plot Girth vs. Volume
plot(trees$Girth, trees$Volume, col="red",xlab="Tree Girth", ylab="Tree Volume")
abline(lm(trees$Volume~trees$Girth))

# Scatter Plot Volume vs. Height
plot(trees$Volume, trees$Height, col="purple",xlab="Tree Volume", ylab="Tree Height")
abline(lm(trees$Height~trees$Volume))

# Plot a grid using the Trees dataset showing comparisons of all variables
plot(trees)

# Plot a bar plot
par(mfrow=c(2,3))
barplot(trees$Girth,xlab="Girth",col="blue")
barplot(trees$Height,xlab="Height", col="purple")
barplot(trees$Volume,xlab="Volume", col="red")

# Plot a box plot
boxplot(trees$Girth,xlab="Girth",col="blue")
boxplot(trees$Height,xlab="Height", col="purple")
boxplot(trees$Volume,xlab="Volume", col="red")

# include the locator (watch the console for the x / y coordinates)
text(locator(1),"This point")

# Using the lattice library, draw a 3d graph
library(lattice)              # Include the library
a <- 1:10                     # Create sample vector
b <- 1:15                     # Create another vector
eg <- expand.grid(x=a, y=b)   # Create a data frame using a for x and b for y
eg$z <- eg$x^2 + eg$x * eg$y  # Create a new element z that is X^2 + x * y
wireframe(z ~ x+y, eg)        # Wireframe draws a surface of the result
                              # Also look up persp() and cloud() for other graphing options