# Author: David Simpson
# Date: 2018.03.11
# Updated: 2018.04.29
# Action: Describe Data Sets

# ********************************************************************************
#                                 * File Manager*
# ********************************************************************************




# ********************************************************************************
#                                 * BEGIN Prep *
# ********************************************************************************

# Set Up

rm(list=ls()) # Clear enviornment
setwd("/Users/dsimp/GitHub/peg/analysis") # Set Working Directory
getwd()

# Libraries
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("foreign") # Read STATA files
library("haven") # Needed to read STATA files
library("gmodels") # will allow you to use CrossTable() to do cross tabs
library("usmap")

# ********************************************************************************
#                                 * Describe *
# ********************************************************************************
# Counties Data Set
load("/Users/dsimp/GitHub/peg/data/counties.Rda")
dim(data)
glimpse(data)
# Data Set has 3,141 County Level Observations
# Plan: Construct a data set using each of the counties and then drop observations according to Berry

ggplot(data, aes(x = log(LandArea),
                 y= Pop90/1000, col = log(H_Units90))) +
  geom_point()

data$fips = data$FIPS
data$log = log(data$Pop90)

# Helpful Link: (https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html)
usmap::plot_usmap(data = data, values = "log", regions = "counties", lines = "black") + 
  scale_fill_continuous(
    low = "white", high = "green", name = "Population (1990)", label = scales::comma
  ) + theme(legend.position = "right")+
  labs(title = "US Counties (1990)", subtitle = "Log Population of US Counties")


# General Data Set
rm(data)
load("general92.Rda")

# Measures of overlapp, and Measures of balh



data = read.csv("general92.csv")
unique(data$State) # 51 Observations = 50 states and the district of Columbia
CrossTable(data$State, prop.r = T,  format="SPSS") # Frequency of each state
data = read_dta("general92.dta")
