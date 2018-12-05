# Title:    Bryn R help
# Author:   David Simpson
# Created:  31 October 2018
# Updated:  31 October 2018
# Inputs:   (1) spss file "B_data.por" from: "Website Name"
# Outputs:  (1) None

setwd("/Users/dsimp/GitHub/Bryn/raw") # Change this to your working directory. 
# I always store my orignal data sets in "raw" data sub folder. And then any newly created
# data in a "data" sub folder
library("foreign") 
rm(list = ls())  
data = read.spss("B_data.por", to.data.frame=TRUE)

# as.character() converts the factor variable to a string variable. This is it will just be text
data$Congress_Q =  as.character(data$Q5CF1) # Creates a new column equal to your question of interest.
data$Congress_Q[is.na(data$Congress_Q)]="Missing" #Convert the NA to the text "Missing"

data$Q_Values = 0 # New column where you want the values to go
# Next i will use the which comand inside of the [].
# When the [] is put after a column name eg data$Q_Values, whatever numbers appear inside of []
# will cause r to return the observations corresponding to those rows.
# Which(Some condition) will return the row values that meet that condition
# All together, we want to identify certain rows by that meet a condition in the Congress_Q variable
# and create a coreesponding value in the Q_Values variable
data$Q_Values[which(data$Congress_Q=="Completely agree ")] = 1 # Change to whatever value you want
data$Q_Values[which(data$Congress_Q=="Mostly agree ")] = 2 # Change if wanted
data$Q_Values[which(data$Congress_Q=="Mostly disagree ")] = 3
data$Q_Values[which(data$Congress_Q=="Completely disagree ")] = 4
data$Q_Values[which(data$Congress_Q=="Don't know/Refused ")] = 5
data$Q_Values[which(data$Congress_Q=="Missing ")] = 0

data$Q_Values = as.numeric(data$Q_Values) # Makes sure that your values in the Q_values column are numbers and not text characters (strings)

# Note: You should check out the Survey documentation, but it looks like the missing values are from the possibility 
# that maybe different people got asked different sets of questions. So this data set is reproting complete
# data per question.

# If you only care about your question you can drop the others by doing the following
keep= c(7,15:50)
data=data[,keep] # now the data set only has the original question, the demogaphic info, and the two new columns
org=c(1,36,37,2:35) 
data=data[,org] # organize the data so that the two new columns are next to the original quesiton

# If you want to drop all observations that are missing you can do the following
data = data[which(data$Congress_Q != "Missing"),] # Drops all rows with "Missing"

# After this step there are only 504 observations

# For peace of mind you can also recode the Congress_Q variables so that they no longer have a space after the text
data$Congress_Q[which(data$Congress_Q=="Completely agree ")] = "Completely agree" # Change to whatever value you want
data$Congress_Q[which(data$Congress_Q=="Mostly agree ")] = "Mostly agree" # Change if wanted
data$Congress_Q[which(data$Congress_Q=="Mostly disagree ")] = "Mostly disagree"
data$Congress_Q[which(data$Congress_Q=="Completely disagree ")] = "Completely disagree"
data$Congress_Q[which(data$Congress_Q=="Don't know/Refused ")] = "Don't know/Refused"

# If you want to wait until the end to create your scale you could do it here. Note the spaces are removed now
# Obviously change to however you want to make your scale
data$Q_Values[which(data$Congress_Q=="Completely agree")] = 1 
data$Q_Values[which(data$Congress_Q=="Mostly agree")] = 1 
data$Q_Values[which(data$Congress_Q=="Mostly disagree")] = -1
data$Q_Values[which(data$Congress_Q=="Completely disagree")] = -1
data$Q_Values[which(data$Congress_Q=="Don't know/Refused")] = 0
