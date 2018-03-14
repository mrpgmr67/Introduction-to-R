###################################################################################
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 14b - Advanced R / Data Mining Example (Decision Tree Example)
###################################################################################
library("party")
str(iris)
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)

plot(iris_ctree)
plot(iris_ctree,type="simple")