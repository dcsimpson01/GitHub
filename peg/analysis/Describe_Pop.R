# Author: David Simpson
# Date: 2018.03.11
# Updated: 2018.03.11
# Action: Describe Population Data

# ********************************************************************************
#                                 * File Manager*
# ********************************************************************************

# Set Up

rm(list=ls()) # Clear enviornment
setwd("/Users/dsimp/GitHub/peg/data") # Set Working Directory
getwd()

# Libraries
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("foreign") # Read STATA files
library("haven") # Needed to read STATA files
library("gmodels") # will allow you to use CrossTable() to do cross tabs

# ********************************************************************************
#                                 * Analyze *
# ********************************************************************************
data = read.csv("pop92.csv")
unique(data$State) # 51 Observations = 50 states and the district of Columbia
CrossTable(data$State, prop.r = T,  format="SPSS") # Frequency of each state

