# Author: David Simpson
# Date: 2018.07.13
# Updated: 2018.07.13
# Action: Analysis Script

# Set Up
rm(list=ls()) # Clear enviornment
setwd("/Users/dsimp/GitHub/BmoreRev/analysis") # Set Working Directory
getwd()

# Libraries
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("foreign") # Read STATA files
library("haven") # Needed to read STATA files
library("gmodels") # will allow you to use CrossTable() to do cross tabs

# Analysis
setwd("/Users/dsimp/GitHub/BmoreRev/data")
data = read.csv("RevData.csv")
head(data)


data = data[2:44]
n = 4 # n = number of variables
output = as.data.frame(matrix(rep(0,n*length(1978:2017)),length(1978:2017),n))
output 

test = as.matrix(c(rep(0,6)),2,3)
print(test)

test = matrix(c(5,6,7,7),nrow = 2,ncol=2, byrow = TRUE)
print(test)
?as.matrix
mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat





year = as.matrix(c(1978:2017))
realprop = c(data[1,4:43])

set = as.matrix(cbind(year,realprop))

2017-1978
?as.matrix()
                