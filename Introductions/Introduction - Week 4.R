#####################################################################################
# 
#   Data 710:  Introduction to R
#   Michael S. Pomatto  (mpomatto@davenport.edu)
#   Week 4 - Factors and Tables
#
#   GITHUB:  https://github.com/mrpgmr67/Introduction-to-R
#
##########################################################################
any_vector <- c(5,12,13,12, 10, 15)

# Create a factor
sample_factor <- factor(any_vector)

#  Let's look at the structure
str(sample_factor)
unclass(sample_factor)

any_vector_categories <- c("K", "M", "I", "I", "I", "M")

#  Note lengths must be the same
length(sample_factor)
length(any_vector_categories)

# Temporarily apply function
tapply(any_vector, any_vector_categories, mean)

# Split Function
split(1:length(any_vector), any_vector)


#  Create a Table
new_vector <- c(2,3,4,5)
new_list <- list(c(4,2,4,5), c("A", "B", "B", "C"))
tapply(new_vector, new_list, length)

new_table <-table(new_list)
addmargins((new_table))

# Must create the sub table function
subtable <- function(tbl, subnames){
  tblarray <-unclass(tbl)
  dcargs <-list(tblarray)
  ndims <-length(subnames)
  for (i in 1:ndims){
    dcargs[[i+1]]<-subnames[[i]]
  }
  subarray <-do.call("[", dcargs)
  dims <-lapply(subnames, length)
  subtbl <-array(subarray,dims,dimnames=subnames)
  class(subtbl) <- "table"
  return(subtbl)
}

subtable(new_table, list(new_list == "A"))

# Aggregate function
aggregate(new_vector,new_list,mean)

# Cut function (cut the table into two breaks without a label) simulating a coin toss
cut(new_table,2, c("Heads", "Tails"))
new_table

# Create a new table using the "Lynx" dataset built into R
help(lynx)
tblLynx <- table(lynx)
