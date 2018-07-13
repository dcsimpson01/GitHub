# Author: David Simpson
# Date: 2018.04.29
# Updated: 2018.04.29
# Title: Consruct.R
# Action: Construct the dataset for analysis

# ********************************************************************************
#                                 * File Manager*
# ********************************************************************************

# Input: 
  # (1) counties.Rda
  # (2) general92.Rda
  # (3) special92.Rda
  # (4) school92.Rda

# ********************************************************************************
#                                 * BEGIN Prep *
# ********************************************************************************
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
library("readxl")

# ********************************************************************************
#                                 * Construct *
# ********************************************************************************
setwd("/Users/dsimp/GitHub/peg/data/")
load("counties.Rda") # Load Counties Data

# results = data.frame(matrix(rep(0,4*nrow(data)),nrow(data),4))
results = data

#----------------------------------- Begin Change FIPS to fips --------------------------
names = colnames(results)
names[1] = "fips"
colnames(results) = names
#----------------------------------- End Change FIPS to fips --------------------------

#----------------------------------- Begin Build Crosswalk --------------------------
# Build Crosswalk
#cross = read.csv("/Users/dsimp/GitHub/peg/raw/FIPS_Cross/nchs2fips_county1990.csv")
crosswalk = data.frame(matrix(rep(0,8*nrow(results)),nrow(results),8))
colnames(crosswalk) = c("St Name", "St Abr","Co Name" ,"So Order", "Co Order",
                              "fips St","fips Co","fips")
results=results[order(results$State,results$Name),]
count = c(2,3,6,7,8)
count2 = c(5,4,2,3,1)
crosswalk[,count] = results[,count2]

code = read_xlsx("/Users/dsimp/GitHub/peg/raw/StateOrderCode.xlsx")

# Fill in State name and the order in the crosswalk
# State Info
count = 0
for (i in 1:nrow(crosswalk)){
  for (j in 1:nrow(code)){
    if(crosswalk[i,2] == code[j,2]){
      crosswalk[i,1]=code[j,1]
      crosswalk[i,4]=code[j,3]
    }
    }
  count = count + 1
  print(count)
  print(crosswalk[i,1])
}

# County Info
code = code[2:52,]
for(i in 1:nrow(code)){
  order = which(crosswalk[,1]==code$`State Name`[i])
  crosswalk[order,5]=1:length(order)
}

# Add zeros to county info
glimpse(crosswalk)
crosswalk$`Co Order`=as.character(crosswalk$`Co Order`)
for (i in 1:nrow(crosswalk)){
  if (nchar(crosswalk$`Co Order`[i])<2){
    crosswalk$`Co Order`[i] = paste("00",crosswalk$`Co Order`[i],sep="")
  }
  if (nchar(crosswalk$`Co Order`[i])<3){
    crosswalk$`Co Order`[i] = paste("0",crosswalk$`Co Order`[i],sep="")
  }
}
rm(code)
rm(names)
rm(order)

setwd("~/GitHub/peg/raw/data") # Save the data for later
save(crosswalk, file ="crosswalk.Rda")
write.csv(crosswalk, "crosswalk.csv", row.names = FALSE, quote = TRUE)
#----------------------------------- End Build Crosswalk --------------------------

#----------------------------------- Begin General Prep ---------------------------

load("general92.Rda") # Load General Purpose Government Data

# New Variable Destinations
data$State = substr(data$GOVCODE,1,2)
data$Type = substr(data$GOVCODE,3,3)
data$County = substr(data$GOVCODE,4,6)
data$Unit = substr(data$GOVCODE,7,9)
data$fips = ""
data$FipsState = ""
data$FipsCounty = ""
data$ST = ""
data$CountyName = ""
# Reorder Data
order = c(1,100:108,2:99)
data = data[,order]
data = data[,c(1:37)] # Remove government composition data (return to for a later date)

glimpse(data)

count = 0
start_time = Sys.time()
for (i in 1:nrow(data)){
  for (j in 1:nrow(crosswalk)){
    if ((data$State[i] == crosswalk$`So Order`[j]) & (data$County[i] == crosswalk$`Co Order`[j] )){
      data$fips[i] = crosswalk$fips[j]
      data$FipsState[i] = crosswalk$`fips St`[j]
      data$FipsCounty[i] = crosswalk$`fips Co`[j]
      data$ST[i] = crosswalk$`St Name`[j]
      data$CountyName[i] = crosswalk$`Co Name`[j]
    }
  }
  count = count +1
  print(count)
  print(data$State[i])
  end_time = Sys.time()
  end_time - start_time
}

save(data, file ="test.Rda")
write.csv(data, "test.csv", row.names = FALSE, quote = TRUE)


unique(data$fips)

# Fill Variables
data$State = 
print(names(data))

#----------------------------------- End General Prep ---------------------------




#----------------------------------- Begin Next  ---------------------------



