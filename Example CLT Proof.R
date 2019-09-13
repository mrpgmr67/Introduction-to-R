fnCLTProof <- function(){
  variance1 <- 5
  n1 <-50 ;n2 <-500; n3<-5000; n4<-7500
  xBar1 <- rnorm(10000, sd=5)
  xBarPop <- mean(xBar1)
  xBarPopsd <- sd(xBar1)
  for (i in 1:1){
    xBar2 <- sample(xBar1,n1)
    xBar3 <- sample(xBar1,n2)
    xBar4 <- sample(xBar1,n3)
    xBar5 <- sample(xBar1,n4)
    Labels <- c("Population","SmallSample", "MedSample", "LargeSample", "LargerSample")
    meansCompare <- c(xBarPop,mean(xBar2),mean(xBar3),mean(xBar4),mean(xBar5))
    meansTable <- data.frame(Labels,meansCompare)
    meansCompare
    par(mfrow=c(2,2)) 
    hist(xBar2,main="Small", col="red", xlab="Small Sample")
    hist(xBar3,main="Medium", col="orange", xlab="Medium Sample")
    hist(xBar4,main="Large", col="yellow", xlab="Large Sample")
    hist(xBar5,main="Extra Large", col="green", xlab="Final Sample")
    xBar5Mn <- mean(xBar5)
    xBar5sd <- sd(xBar5)
    x<-seq(-15,15,1)
    y <- dnorm(x,xBar5Mn,xBar5sd)*10000
    lines(x,y,lwd = 2, col="dark green")
  }
}
fnCLTProof()  