###################################################################################
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 14 - Advanced R / Data Mining Example
###################################################################################

# Make sure the graphics library is loaded
require(graphics)

# Create a simple two dimensional matrix
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")

# preprocessing - fill in missing values
#
#   This would be a great place to use the Validate, Dplyr, and Tidyr libraries!
#
# Standardize data if necessary

# Principle Component Analysis (PCA)
model <- prcomp(x,scale = TRUE)
summary(model)
plot(model,main="",col="blue")
pairs(x)
#################   Preprocessing done at this point

#  Create a KMeans Cluster Analysis using Two clusters
(cl <- kmeans(x, 2))                                   # Create two clusters
plot(x, col = cl$cluster)                              # Plot the cluster data
points(cl$centers, col = 1:2, pch = 8, cex = 2)        # And the centers for the clusters (look for stars)

hcluster <-hclust(dist(x))    # do a quick hierarchial cluster on x
plot(hcluster)                # visually plot it

#############################

# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

## cluster centers "fitted" to each obs.:
fitted.x <- fitted(cl);  head(fitted.x)
resid.x <- x - fitted(cl)

## Equalities : ----------------------------------
cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
      c(ss(fitted.x), ss(resid.x),    ss(x)))
stopifnot(all.equal(cl$ totss,        ss(x)),
          all.equal(cl$ tot.withinss, ss(resid.x)),
          ## these three are the same:
          all.equal(cl$ betweenss,    ss(fitted.x)),
          all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
          ## and hence also
          all.equal(ss(x), ss(fitted.x) + ss(resid.x))
)

kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))

#
#  Build it out with five clusters
# 
## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

