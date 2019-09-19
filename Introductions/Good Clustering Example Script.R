library("dplyr")
library("tidyr")
library("validate")
library("magrittr")
library("party")
library("DMwR")

phoneRawData <- read.csv("Phone Records.csv")

# Describe the data - Summary
head(phoneRawData)
tail(phoneRawData)
summary(phoneRawData)

var(phoneRawData$Fixed.Calls)
var(phoneRawData$Duration)
var(phoneRawData$Mobile)
var(phoneRawData$International)
var(phoneRawData$Broadband)

# Check for invalid fields
aCheck <- check_that(phoneRawData,
                     !is.na(Fixed.Calls), 
                     !is.na(Duration),
                     !is.na(Mobile),
                     !is.na(International),
                     !is.na(Broadband))
print(summary(aCheck))

# Visually describe the data
barplot(aCheck, main="NA Check")

boxplot(phoneRawData$Fixed.Calls,
        phoneRawData$Duration,
        phoneRawData$Mobile,
        phoneRawData$International,
        phoneRawData$Broadband,
        las = 2,
        main = "Overall Comparison",
        names=c("Fixed Calls","Duration","Mobile","International","Broadband"))

# Individual Boxplots
par(mfrow=c(2,3))
boxplot(phoneRawData$Fixed.Calls,main="Fixed Calls")
boxplot(phoneRawData$Duration,main="Duration")
boxplot(phoneRawData$Mobile,main="Mobile")
boxplot(phoneRawData$International,main="International")
boxplot(phoneRawData$Broadband,main="Broadband")
par(mfrow=c(1,1))   #Reset graph window

# Histograms
par(mfrow=c(2,3))
hist(phoneRawData$Fixed.Calls,main="Fixed Calls",xlab="")
hist(phoneRawData$Duration,main="Duration",xlab="")
hist(phoneRawData$Mobile,main="Mobile",xlab="")
hist(phoneRawData$International,main="International",xlab="")
hist(phoneRawData$Broadband,main="Broadband",xlab="")
par(mfrow=c(1,1))   #Reset graph window

plot(phoneRawData)

##############################################################################
# Decision Tree
##############################################################################

phoneDataPivot <- gather(phoneRawData,key=Call.Type,value="Length",Fixed.Calls,Duration,Mobile,International,Broadband,-ID,factor_key=TRUE)

phoneTree <- ctree(Call.Type ~ Call.Type, data=phoneDataPivot)
plot(phoneTree, las=2)

rm(phoneDataPivot)
rm(phoneTree)
##############################################################################
# Outlier Detection
#   http://www.rdatamining.com/examples/outlier-detection
##############################################################################

phoneData <- select(phoneRawData,Fixed.Calls,Duration,Mobile,International,Broadband)

# Individual variable outliers

# Fixed Calls
boxplot.stats(phoneData$Fixed.Calls)$out
# Duration
boxplot.stats(phoneData$Duration)$out
# Mobile
boxplot.stats(phoneData$Mobile)$out
# International
boxplot.stats(phoneData$International)$out
# Broadband
boxplot.stats(phoneData$Broadband)$out

# Multi-variant outlier detection
outlier.scores <- lofactor(phoneData, k=10)
plot(density(outlier.scores, na.rm=TRUE))

# Top Multi-variant Outliers
outliersOrdered <- order(outlier.scores, decreasing=T)[1:100]

# Location of the top outliers
outliersOrdered
phoneData.outliersRemoved <- phoneData[-outliersOrdered,]        # Not used but kept as an example

##############################################################################
# Clustering
##############################################################################
wssplot <- function(wssData, numClust=12, seed=12345){
  wss <- (nrow(wssData)-1)*sum(apply(wssData,2,var))
  for (i in 2:numClust){
    set.seed(seed)
    wss[i] <- sum(kmeans(wssData, centers=i)$withinss)
  }
  plot(1:numClust, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

# Run PCA analysis to identify variables for clustering
model <- prcomp(phoneData,center=TRUE, scale = TRUE)
summary(model)
plot(model,main="",col="blue")
biplot(model,scale=0, cex=.7)

pairs(phoneData)

# Remove Broadband & International
phoneDataScale <- select(phoneData,Fixed.Calls,Duration,Mobile)

# Estimate the number of clusters for Kmeans
wssplot(phoneDataScale)

# Create clusters
(clust <- kmeans(phoneDataScale, 4))
plot(phoneDataScale, col = clust$cluster)                         # Visually check clusters
points(clust$centers, col = 1:4, pch = 8, cex = 2)

# Add cluster data to the phone data
phoneDataCluster <- cbind(phoneRawData,clust$cluster)

# Fix clust$cluster name
colnames(phoneDataCluster) <- c(names(phoneRawData[1,1:6]),"Cluster")
head(phoneDataCluster)
##############################################################################
# Output Clustering Results
##############################################################################

# Sort data
phoneDataCluster <- arrange(phoneDataCluster,Cluster,ID)
head(phoneDataCluster)