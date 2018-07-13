# Author: David Simpson
# Updated: 2018.02.26
# Name: cs_finances.R
# Files Accessed:(1) 
#                (2) 


# Summary: Complete Streets Analysis using transportation data.

rm(list = ls()) # Clear enviornment
setwd("C:/Users/David.Simpson/Documents/FlaGit/Complete_Streets/analysis") # Set Working directory
getwd()

# Libraries
#install.packages("kableExtra") # Install package
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("knitr") # For R Mark Down
library("kableExtra") # For Making Nice Tables
library("scales")



# Data: Save a copy to raw for record keeping and to data for analysis
data= read.csv("file:///C:/Users/David.Simpson/Documents/FlaGit/Budget/data/gensets/FY12-18 Transportation.csv"
)
setwd("C:/Users/David.Simpson/Documents/FlaGit/Complete_Streets/raw") # Set Working directory
write.csv(data,"cs_trans.csv", row.names = FALSE)
setwd("C:/Users/David.Simpson/Documents/FlaGit/Complete_Streets/data") # Set Working directory
write.csv(data,"cs_trans.csv", row.names = FALSE)

# See Data Set
glimpse(data) #Observations: 5,298 and Variables: 53

# Yearly Info:

sum(data$FY12_Adopted)
sum(data$FY13_Adopted)
sum(data$FY15_Adopted[which(data$Fund.Name=="Motor Vehicle")])
sum(data$FY15_Adopted)
sum(data$FY16_Adopted)
sum(data$FY17_Adopted)
sum(data$FY18_Adopted)

sum(data$FY12_Net.Actual)
sum(data$FY13_Net.Actual)
sum(data$FY14_Net.Actual)
sum(data$FY15_Net.Actual)
sum(data$FY16_Net.Actual)
sum(data$FY17_Net.Actual)
sum(data$FY18_Net.Actual)

# 2017 Info

sum(data$FY17_Adopted)
sum(data$FY17_Adjusted)
sum(data$FY17_BAPS.Actual)
sum(data$FY17_Net.Actual)

sum(data$FY17_Adopted[which(data$Fund.Name=="General")])
sum(data$FY17_Adjusted[which(data$Fund.Name=="General")])
sum(data$FY17_BAPS.Actual[which(data$Fund.Name=="General")])
sum(data$FY17_Net.Actual[which(data$Fund.Name=="General")])


# Table #1: Dollars By Fund and Year (Adopted)-----------------------------------
FundNames =names(summary(data$Fund.Name))
NameList = c("FY12_Adopted","FY13_Adopted",
             "FY14_Adopted","FY15_Adopted",
             "FY16_Adopted","FY17_Adopted",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)
outcome = matrix(rep(0,70),10,7)

count = 1
for (j in 1:length(colnum)){
  for (i in 1:length(FundNames)){
    print(FundNames[i])
    outcome[i,count]=dollar(sum(data[,colnum[j]][which(data$Fund.Name==FundNames[i])]))
  }    
  print(colnum[j])
  outcome[10,count] = dollar(sum(data[,colnum[j]]))
  count = count+1
}
x= c(FundNames, "Total")
table = as.data.frame(cbind(x,outcome))
colnames(table) = c("Fund Names","FY12 Adopted", "FY13 Adopted",
                     "FY14 Adopted","FY15 Adopted","FY16 Adopted",
                     "FY17 Adopted","FY18 Adopted")
table1_Adopted = table

# Table #2: Dollars By Fund and Year (Adjusted)-----------------------------------
FundNames =names(summary(data$Fund.Name))
NameList = c("FY12_Adjusted","FY13_Adjusted",
             "FY14_Adjusted","FY15_Adjusted",
             "FY16_Adjusted","FY17_Adjusted",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)
outcome = matrix(rep(0,70),10,7)

count = 1
for (j in 1:length(colnum)){
  for (i in 1:length(FundNames)){
    print(FundNames[i])
    outcome[i,count]=dollar(sum(data[,colnum[j]][which(data$Fund.Name==FundNames[i])]))
  }    
  print(colnum[j])
  outcome[10,count] = dollar(sum(data[,colnum[j]]))
  count = count+1
}
x= c(FundNames, "Total")
table = as.data.frame(cbind(x,outcome))
colnames(table) = c("Fund Names","FY12 Adjusted", "FY13 Adjusted",
                    "FY14 Adjusted","FY15 Adjusted","FY16 Adjusted",
                    "FY17 Adjusted","FY18 Adopted")
table2_Adjusted = table


# Table #3: Dollars By Fund and Year (BAPS Actual)-----------------------------------
FundNames =names(summary(data$Fund.Name))
NameList = c("FY12_BAPS.Actual","FY13_BAPS.Actual",
             "FY14_BAPS.Actual","FY15_BAPS.Actual",
             "FY16_BAPS.Actual","FY17_BAPS.Actual",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)
outcome = matrix(rep(0,70),10,7)

count = 1
for (j in 1:length(colnum)){
  for (i in 1:length(FundNames)){
    print(FundNames[i])
    outcome[i,count]=dollar(sum(data[,colnum[j]][which(data$Fund.Name==FundNames[i])]))
  }    
  print(colnum[j])
  outcome[10,count] = dollar(sum(data[,colnum[j]]))
  count = count+1
}
x= c(FundNames, "Total")
table = as.data.frame(cbind(x,outcome))
colnames(table) = c("Fund Names","FY12 Actual", "FY13 Actual",
                    "FY14 Actual","FY15 Acutal","FY16 Actual",
                    "FY17 Actual","FY18 Adopted")
table3_BAPS_Actuals = table

# Table #4: Dollars By Fund and Year (Net Actuals) -----------------------------------
FundNames =names(summary(data$Fund.Name))
NameList = c("FY12_Net.Actual","FY13_Net.Actual",
             "FY14_Net.Actual","FY15_Net.Actual",
             "FY16_Net.Actual","FY17_Net.Actual",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)
outcome = matrix(rep(0,70),10,7)

count = 1
for (j in 1:length(colnum)){
  for (i in 1:length(FundNames)){
    print(FundNames[i])
    outcome[i,count]=dollar(sum(data[,colnum[j]][which(data$Fund.Name==FundNames[i])]))
  }    
  print(colnum[j])
  outcome[10,count] = dollar(sum(data[,colnum[j]]))
  count = count+1
}
x= c(FundNames, "Total")
table = as.data.frame(cbind(x,outcome))
colnames(table) = c("Fund Names","FY12 Actual", "FY13 Actual",
                    "FY14 Actual","FY15 Actual","FY16 Actual",
                    "FY17 Actual","FY18 Adopted")
table4_Net_Actuals = table

# End Dollars by Fund Section ---------------------------



# Table #5: Dollars By Service (Adopted)-----------------------------------
outcome = as.data.frame(matrix(rep(0,306),34,9))
LongName = unique(paste(data[,3]," ",data[ ,4]))
outcome[,1] = c(as.numeric(substr(LongName,1,3)),"")
outcome[,2] = c(substring(LongName,5),"Total")
NameList = c("FY12_Adopted","FY13_Adopted",
             "FY14_Adopted","FY15_Adopted",
             "FY16_Adopted","FY17_Adopted",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)

count = 3
for (j in 1:length(colnum)){
  for (i in 1:33){
    print(outcome$V1[i])
    outcome[i,count] = dollar(sum(data[,colnum[j]][which(data$Program.Id==outcome$V1[i])]))
  }
    print(colnum[j])
    outcome[34,count] = dollar(sum(data[,colnum[j]]))
    count = count +1
}
colnames(outcome) = c("ID", "Service Name", "FY12 Adopted", "FY13 Adopted",
                     "FY14 Adopted","FY15 Adopted","FY16 Adopted",
                     "FY17 Adopted","FY18 Adopted")
table5_serAdopt = outcome


# Table #6: Dollars By Service (Adjusted)-----------------------------------
outcome = as.data.frame(matrix(rep(0,306),34,9))
LongName = unique(paste(data[,3]," ",data[ ,4]))
outcome[,1] = c(as.numeric(substr(LongName,1,3)),"")
outcome[,2] = c(substring(LongName,5),"Total")
NameList = c("FY12_Adjusted","FY13_Adjusted",
             "FY14_Adjusted","FY15_Adjusted",
             "FY16_Adjusted","FY17_Adjusted",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)

count = 3
for (j in 1:length(colnum)){
  for (i in 1:33){
    print(outcome$V1[i])
    outcome[i,count] = dollar(sum(data[,colnum[j]][which(data$Program.Id==outcome$V1[i])]))
  }
  print(colnum[j])
  outcome[34,count] = dollar(sum(data[,colnum[j]]))
  count = count +1
}
colnames(outcome) = c("ID", "Service Name", "FY12 Adjusted", "FY13 Adjusted",
                      "FY14 Adjusted","FY15 Adjusted","FY16 Adjusted",
                      "FY17 Adjusted","FY18 Adopted")
table6_serAdjust = outcome

# Table #7: Dollars By Service (BAPS Actual)-----------------------------------
outcome = as.data.frame(matrix(rep(0,306),34,9))
LongName = unique(paste(data[,3]," ",data[ ,4]))
outcome[,1] = c(as.numeric(substr(LongName,1,3)),"")
outcome[,2] = c(substring(LongName,5),"Total")
NameList = c("FY12_BAPS.Actual","FY13_BAPS.Actual",
             "FY14_BAPS.Actual","FY15_BAPS.Actual",
             "FY16_BAPS.Actual","FY17_BAPS.Actual",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)

count = 3
for (j in 1:length(colnum)){
  for (i in 1:33){
    print(outcome$V1[i])
    outcome[i,count] = dollar(sum(data[,colnum[j]][which(data$Program.Id==outcome$V1[i])]))
  }
  print(colnum[j])
  outcome[34,count] = dollar(sum(data[,colnum[j]]))
  count = count +1
}
colnames(outcome) = c("ID", "Service Name", "FY12 Actual", "FY13 Actual",
                      "FY14 Actual","FY15 Actual","FY16 Actual",
                      "FY17 Actual","FY18 Adopted")
table6_serBAPS_Actual = outcome

# Table #8: Dollars By Service (Net Actual)-----------------------------------
outcome = as.data.frame(matrix(rep(0,306),34,9))
LongName = unique(paste(data[,3]," ",data[ ,4]))
outcome[,1] = c(as.numeric(substr(LongName,1,3)),"")
outcome[,2] = c(substring(LongName,5),"Total")
NameList = c("FY12_Net.Actual","FY13_Net.Actual",
             "FY14_Net.Actual","FY15_Net.Actual",
             "FY16_Net.Actual","FY17_Net.Actual",
             "FY18_Adopted")
colnum = which(colnames(data) %in% NameList)

count = 3
for (j in 1:length(colnum)){
  for (i in 1:33){
    print(outcome$V1[i])
    outcome[i,count] = dollar(sum(data[,colnum[j]][which(data$Program.Id==outcome$V1[i])]))
  }
  print(colnum[j])
  outcome[34,count] = dollar(sum(data[,colnum[j]]))
  count = count +1
}
colnames(outcome) = c("ID", "Service Name", "FY12 Actual", "FY13 Actual",
                      "FY14 Actual","FY15 Actual","FY16 Actual",
                      "FY17 Actual","FY18 Adopted")
table6_serNet_Actual = outcome


# End Dollars by Service Section --------------------------------------------
