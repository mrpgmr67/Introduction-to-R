fnCLTProof <- function() {
  variance1 <- 5     # Create a variable for variance
  n1 <- 50           # Create four sample size variables
  n2 <- 500
  n3 <- 5000
  n4 <- 7500
  xBar1 <- rnorm(10000, sd = 5)    # generates multivariate normal random variates (10,000 of them)
  xBarPop <- mean(xBar1)           # calculate the mean
  xBarPopsd <- sd(xBar1)           # calculate the standard deviation
  
  xBar2 <- sample(xBar1, n1)       # takes n samples from the random number pool xBar1
  xBar3 <- sample(xBar1, n2)
  xBar4 <- sample(xBar1, n3)
  xBar5 <- sample(xBar1, n4)
  Labels <-                        # optional step - setup some pretty labels
    c("Population",
      "SmallSample",
      "MedSample",
      "LargeSample",
      "LargerSample")
  meansCompare <- c(xBarPop, mean(xBar2), mean(xBar3), mean(xBar4), mean(xBar5))
  meansTable <- data.frame(Labels, meansCompare)
  meansCompare
  par(mfrow = c(2, 2))                # For the plot, setup a 2x2 grid
  hist(xBar2,
       main = "Small",
       col = "red",
       xlab = "Small Sample")
  hist(xBar3,
       main = "Medium",
       col = "orange",
       xlab = "Medium Sample")
  hist(xBar4,
       main = "Large",
       col = "yellow",
       xlab = "Large Sample")
  hist(xBar5,
       main = "Extra Large",
       col = "green",
       xlab = "Final Sample")
  
  xBar5Mn <- mean(xBar5)                        # draw a bell curve over the last graph
  xBar5sd <- sd(xBar5)                          # calculate standard deviation
  x <- seq(-15, 15, 1)                          # Generate a distribution
  y <- dnorm(x, xBar5Mn, xBar5sd) * 10000
  lines(x, y, lwd = 2, col = "dark green")      # plot the line - it should overlay the bell
  
}
fnCLTProof()  