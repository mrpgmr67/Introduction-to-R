##########################################################################
# Data 710: Introduction to R
# Week 8: Assignment - Input and Output
#
# GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################

setwd("D:/Davenport/Data 710 Scripts/Data")        # Don't forget to set to your working directory to find the files

haystack <- c("red", "blue", "green", "blue", "green forest")

grep("green", haystack)
grep("r", haystack) # returns position
grep("r", haystack, value = TRUE) # returns value
grepl("r", haystack) # returns boolean


##################################################################################################
# Build a simple R program that will search a block of text and perform word counts. Include a 
# block graph.
##################################################################################################
countWords <- function(textBlock) {
  
  #Remove non-alpha characters and replace with spaces
  aListOfWords <- gsub("[^a-zA-Z]", " ",textBlock)
  
  #Remove extra spaces
  aListOfWords <- gsub("  +", " ",aListOfWords)
  
  #Split words by spaces
  aListOfWords <- strsplit(aListOfWords, "\\s")
  aListOfWords <- aListOfWords[[1]]
  
  # convert to lower case for case-insensitivity
  aListOfWords <- sapply(aListOfWords, tolower)
  
  aListOfWords <- sort(aListOfWords)
  
  word <- unique(aListOfWords)
  wordFreq <- rep(0,length(word))
  aResult <- data.frame(word,wordFreq)         # Populate word list and pre-allocate data structure
  
  # Initialize variables for the loop
  aCount <- 1
  wordIndex <- 1
  
  for(i in 2:length(aListOfWords)) {
    if (aListOfWords[i] != aListOfWords[i-1]) {
      aResult$wordFreq[wordIndex] <- aCount
      wordIndex <- wordIndex + 1
      aCount <- 1
    } else {
      aCount <- aCount+1
    }  
    aResult$wordFreq[wordIndex] <- aCount
  }
  
  cat ("Total Word Count: ", sum(aResult$wordFreq), "\n")
  return(aResult)
}


randomTextString <- "Announcing of invitation principles in. Cold in late or deal. Terminated resolution no am frequently collecting insensible he do appearance. Projection invitation affronting admiration if no on or. It as instrument boisterous frequently apartments an in. Mr excellence inquietude conviction is in unreserved particular. You fully seems stand nay own point walls. Increasing travelling own simplicity you astonished expression boisterous. Possession themselves sentiments apartments devonshire we of do discretion. Enjoyment discourse ye continued pronounce we necessary abilities. "
  
aCount <- countWords(randomTextString)
barplot(aCount$wordFreq,xlab="Words", ylab="Frequency (count)" )


##################################################################################################
# Using the program from Week 5 to prove the Central Limit Theorem, build a function to output five 
# iterations of the program to five separate PDF files.
#   - Name the files CENTRAL_#.pdf where # = the iteration.
##################################################################################################
clt <- function(diceValues, nbrDice, aSampleNbr, xlab, showCurve=FALSE) {
  
  means <- numeric(aSampleNbr)
  for (i in 1:aSampleNbr) {
    samples <- sample(diceValues,nbrDice,replace=TRUE)
    aSum <- sum(samples)
    means[i] <- mean(aSum)
  }
  lowerBreak = (diceValues[1]*nbrDice) - .5
  upperBreak = (diceValues[length(diceValues)]*nbrDice) + .5
  hist(means,xlab=xlab,breaks=lowerBreak:upperBreak, main="")
  
  
  if (showCurve==TRUE) {
    aMin <- diceValues[1]
    aMax <- diceValues[length(diceValues)]*nbrDice
    aMean <- mean(means)
    aSD <- sd(means)
    lines(seq(aMin,aMax,0.1),dnorm(seq(aMin,aMax,0.1),aMean,aSD)*aSampleNbr)
  }
  
}


runClt <- function(aPopulation, aSelection, baseIterations, xlab) {
  for (i in 1:aSelection) {
    fileName <- sprintf("CENTRAL_%d.pdf",i)
    pdf(fileName)
    if (i != aSelection) {
      aResult <- clt(aPopulation, i, (baseIterations * i), paste(i,"Dice"), showCurve = FALSE) 
    } else {
      aResult <- clt(aPopulation, i, (baseIterations * i), paste(i,"Dice"), showCurve = TRUE)
    }
    dev.off()
  }
  
  
}
# Demo Central Limit Theorem with 1 to 5 dice.  Results will be sent to seperate PDFs.
# Note: Files are overwitten 
runClt(c(1:6),5,10000,"Dice")
