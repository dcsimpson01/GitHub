# Author:       David Simpson
# Title:        
# Class:        
# Created:      20 December 2018
# Edited:       20 December 2018
# Adapted From: 
# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1) 
#               (2)

# rm(list=ls())

###########################################
#  Begin: Mean Center Data Across All Districts (for relevant variables)
###########################################
# Mean Center Function
center_data <- function(x){
  x - mean(x)
}
# Data Transformation
dataM$x106mean <- center_data(data$x106mean) 
dataM$x106kmean <- center_data(data$x106kmean)
dataM$lc <- center_data(data$lc)
dataM$splc <- center_data(data$splc)
dataM$nsplc <- center_data(data$nsplc)
dataM$oplc <- center_data(data$oplc)
dataM$ilc <- center_data(data$ilc)
dataM$C_pctsp <- center_data(data$C_pctsp)
dataM$C_pctnsp <- center_data(data$C_pctnsp)
dataM$C_pcti <- center_data(data$C_pcti)
dataM$C_pctop <- center_data(data$C_pctop)
# Note when the C_pctsp and C_pctnsp terms are mean centered C_pctnsp drops out of the Clinton Table 1 Models
###########################################
#  End: Mean Center Data Across All Districts
###########################################

###########################################
#  Begin: Mean Center Data by Party (for relevant variables)
###########################################
# Create Centering Matrix

ones <-as.matrix(data[,c("R","D")])  # Dummy variable matrix using R and D dummy variables

m <- solve(t(ones)%*%ones)%*%t(ones)                         # Mean finder matrix
c<-(diag(nrow(data))-ones%*%solve(t(ones)%*%ones)%*%t(ones)) # Mean Centering Matrix

# Uncomment next four lines to spot check code
#m%*%as.matrix(data$x106mean)          # Means for legislator Ideal Point 
#mean(data$x106mean[which(data$R==1)]) # Compare
#mean(data$x106mean[which(data$R==0)]) # Compare
#c%*%as.matrix(data$x106mean)          # Centered Ideal Points

# Data Transformation
dataMp$x106mean <- c%*%as.matrix(data$x106mean) 
dataMp$x106kmean <- c%*%as.matrix(data$x106kmean)
dataMp$lc <- c%*%as.matrix(data$lc)
dataMp$splc <- c%*%as.matrix(data$splc)
dataMp$nsplc <- c%*%as.matrix(data$nsplc)
dataMp$oplc <- c%*%as.matrix(data$oplc)
dataMp$ilc <- c%*%as.matrix(data$ilc)
dataMp$C_pctsp <- c%*%as.matrix(data$C_pctsp)
dataMp$C_pctnsp <- c%*%as.matrix(data$C_pctnsp)
dataMp$C_pcti <- c%*%as.matrix(data$C_pcti)
dataMp$C_pctop <- c%*%as.matrix(data$C_pctop)

rm(ones,m,c) # Remove values
###########################################
#  End: Mean Center Data by Party
###########################################




