# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Documentation  ##########################################################################
# Author:       David Simpson
# Title:        Clinton(2006)Rep
# Class:        GR8265 Political Inequality in the United States
# Created:      19 October 2018
# Edited:       27 October 2018
# Adapted From: Joshua D. Clinton Replication data for: Representation in Congress: 
#               Constituents and Roll Calls in the 106th House
# Source URL:   https://hdl.handle.net/1902.1/10573
# Citation:     Joshua D. Clinton, 2009, "Replication data for: Representation in Congress: 
#               Constituents and Roll Calls in the 106th House", 
#               https://hdl.handle.net/1902.1/10573, Harvard Dataverse, V1, 
#               UNF:3:t+/VddUfFADKtOKo6VsarQ== [fileUNF]
#               https://cfss.uchicago.edu/persp013_interaction_terms.html#estimating_model_with_two_continuous_variables
# Input Files:  (1)
#               (2)
# Source Files: (1)
#               (2)
# Output Files: (1) PDFs for Figure 1: "plot1-1.pdf" through "plot1-4.pdf"
#               (2)
# Packages:     (1) "ggplot2"     # For creating data visualizations: https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
#               (2) "ggpubr"      # An Expansion to ggplot2: https://rpkgs.datanovia.com/ggpubr/index.html
#               (3) "dplyr"        # Reshaping Data: https://dplyr.tidyverse.org
#               (4) "stargazer"   # Create LATEX Table: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf 
#               (5) "gridExtra"   # For muliple plots on a page: https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#               (6) "lmtest"      # For testing linear regression models: https://cran.r-project.org/web/packages/lmtest/lmtest.pdf
#               (7) "sandwich"    # For Robust Covariance Matrix Estimators: https://cran.r-project.org/web/packages/sandwich/sandwich.pdf
#               (8) "foreign"     # Read data from other programs: https://cran.r-project.org/web/packages/foreign/foreign.pdf
#               (9) "RColorBrewer"# Color Schemes for printing: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf
#               (10) "interplot"  # Plot the Effects of Variables in Interaction Terms: https://cran.r-project.org/web/packages/interplot/interplot.pdf

######################################
##### Begin Section 1: Set Up  #######
######################################

rm(list=ls()) # Clear Environment
set.seed(8265) 
setwd("/Users/dsimp/GitHub/Clinton(2006)Rep") # Set Working Directory

packages <- c("ggplot2","ggpubr","dplyr", "stargazer","gridExtra","lmtest","sandwich","RColorBrewer")  # List of packages
new.packages <-packages[!(packages %in% installed.packages()[,"Package"])]                       # Uninstalled packages
if(length(new.packages)) install.packages(new.packages) # Install new packages
lapply(packages, library, character.only = TRUE)        # Load packages

#install.packages("broom")
library("broom") # to reshape model output
library("tidyr")
library("stringr")
library("kableExtra")

# Presenation Type: 
color <- c("#0000ff","#ff0000") # For online
color <- c("#2b83ba","#d7191c") # For photocoppying

# ggplot theme
theme_set(theme_minimal()) # For Miminal Look
theme_set(theme_classic()) # For Classic Look
options(digits = 3) # Set the printed significant digits to 3

# Upload data: Clinton (2006) data is labeled HOUSE.106
HOUSE.106 <- read.table(file="raw/dataverse_files/HOUSE.106.AKN.R")
data <- HOUSE.106 # Refer to Simpson's Code with data

# Make Summary Table
tables = 1 # Indicator for the Data Summary Table
source("analysis/latextables.R") # Print a Summary Table of the data in LATEX using stargazer

# Additional Data Prep #
source("analysis/weights.R")     # Create Weights
data$R <- 0                      # For Democrat (pty==100), & Independent (pty==328) who all Caucus with the Dems
data$R[data$pty==200] <- 1       # Change to 1 for Republican Party Indicator
data$D <- 1 - data$R             # For Democratic Party Indicator
data$partyname <- "Democrat"     # Democrat Party ID
data$partyname[data$R==1] <- "Republican" # Republican Party ID

# Create State Fixed Effects
data$state <- as.character(data$cdcoden)%>% #Create String Identifiers for each state
  str_pad(4,"left", pad = "0")%>%
  str_sub(1,2)

# Generate Functions
# Functions include: (1) Marginal Effects Plots
source("analysis/functions.R")

# Mean Centered Data#
dataM <- data # Use to make mean centered data
dataMp <- data # Use to make mean centered data by party
source("analysis/MeanData.R")
######################################
##### End Section 1: Set Up  ####
######################################

#####################################
# District Plots
#####################################
source("analysis/districtplots.R") # To run the histograms of key votes

#####################################
# Histograms
#####################################
# Edit: To choose which histograms you want to run and when
source("analysis/histograms.R") # To run the histograms of key votes


###########################################
#  Begin: Shift Data
###########################################
#shift_data <- function(x){
#  x+2
#}
#data$shift_x106mean <- shift_data(data$x106mean) 
#data$shift_x106kmean <- shift_data(data$x106kmean) 
###########################################
#  End: Shift Data function
###########################################

###########################################
#  Begin: Simpson Regression Analysis
###########################################

################### Replicate and analyze Clinton Table 1 ###################
source("analysis/functions.R")
# Clinton Table 1 Analysis: Used for Simpson Table 2
summary(Ct1_1 <-  lm(x106mean ~ lc + R, data = data, weights = C_N)) # Clinton Table 1 Model 1 --> No subgroups
summary(Ct1_2 <- lm(x106mean ~ C_pctspid + C_pctnspid + R, data = data, weights = C_N)) # Clinton Table 1 Model 2 has subgroups and is misspecified
summary(Ct1_2b <- (lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))) # Clinton Table 1 Model 2 Correctly Specified
summary(Ct1_2b_nc <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))) # Clinton Table 1 Model 2 Correctly Specified (No Constant Term). Model written for table formatting
summary(Ct1_2b_mc <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = dataM, weights = C_N))) # Table 1 Model 2 Correctly Specified, No Constant. Mean Centered Data
summary(Ct1_2b_3g <- (lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop +  R, data = data, weights = C_N))) # Table 1 Model with all variables

##### Begin Testing For the Future #####
# F-Test to see whether Non-Same-Party Contributes to the Regression (rm = Restricted model. Compared to nc model)
(Ct1_2b_rm <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + C_pctsp +  R, data = data, weights = C_N))) # Clinton Table 1 Model 2 Correctly Specified (No Constant Term). 
anova(Ct1_2b_rm,Ct1_2b_nc) # Results strongly indicate that Non-Same-Party improves estimation
rm(Ct1_2b_rm)

# F-Test to see whether three groups is better than two groups (nc = Restricted model. Compared to 3g model)
# Not sure that I can do an F-test in this way
anova(Ct1_2b_nc,Ct1_2b_3g) # Results strongly indicate that Non-Same-Party improves estimation


# Save for later sensativity analysis
# FE
#summary(Ct1_2b_fe <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R + factor(state), data = data, weights = C_N)))
# Heteroskedasticity
#library(lmtest)
#library(sandwich)
#coeftest(Ct1_1, vcov = vcovHC(Ct1_1, type="HC1"))

##### End Testing For the Future #####
# Summary Table
tables = 2
source("analysis/latextables.R") #Will Provide Table 2
rm(Ct1_1,Ct1_2,Ct1_2b,Ct1_2b_nc,Ct1_2b_mc,Ct1_2b_3g) # Remove the models to save space

# Clinton Table 1 Model 2 Correctly Specified (No Constant Term). Model written for function syntax
summary(Ct1_2b_me <- lm(x106mean ~ 0+ C_pctsp * splc  + C_pctnsp * nsplc + R, data = data, weights = C_N))
me = 1
source("analysis/marginaleffects.R") # Marginal Effects Plots for Each Interaction Term in Ct1_2b_me
rm(Ct1_2b_me)

# Clinton Table 1 Model 3 Group Model
summary(Ct1_2b_3gme <- lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc+ R, data = data, weights = C_N))
me = 1.1
source("analysis/marginaleffects.R") 
rm(Ct1_2b_3gme)

#################
# Clinton Table 1 Model 2 and 3 Group Model (Key Votes) For Appendix 
#################
summary(Ct1_2bk <- (lm(x106kmean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))) # Clinton Table 1 Model 2 Correctly Specified
summary(Ct1_2b_nck <- (lm(x106kmean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))) # Clinton Table 1 Model 2 Correctly Specified (No Constant Term). Model written for table formatting
summary(Ct1_2b_3gk <- (lm(x106kmean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop +  R, data = data, weights = C_N))) # Table 1 Model with all variables
tables = 2.2
source("analysis/latextables.R")
rm(Ct1_2bk,Ct1_2b_nck,Ct1_2b_3gk) # Remove the models to save space

summary(Ct1_2b_3keygme <- lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc+ R, data = data, weights = C_N))
me = 1.2
source("analysis/marginaleffects.R") 
rm(Ct1_2b_3keygme)
#################

# Replicate Clinton Table 2
summary(Ct2_5 <- lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N))
summary(Ct2_5b <- lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N))
summary(Ct2_5b_nc <- lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N))
summary(Ct2_5b_3g <- lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N))


summary(Ct2_8 <- lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N))
summary(Ct2_8b <- lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N))
summary(Ct2_8b_nc <- lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N))
summary(Ct2_8b_3g <- lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N))

tables = 3
source("analysis/latextables.R") #Will Provide Table 3
rm(Ct2_5,Ct2_5b,Ct2_5b_nc,Ct2_5b_3g,Ct2_8,Ct2_8b,Ct2_8b_nc,Ct2_8b_3g) # Remove the models to save space

# Marginal Effect Analysis
summary(Ct2_5b_me <- lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N))
summary(Ct2_8b_me <- lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N))

# Note, Models with state fixed effects do not improve the explainatory power.
#summary(lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc + factor(state), data = subset(data, R==0), weights = C_N))

me = 2
source("analysis/marginaleffects.R") # Marginal Effects Plots for 3 Groups within party
rm(Ct2_5b_me,Ct2_8b_me)
########## WITH KEY VOTES

# Replicate Clinton Table 3
summary(Ct3_5 <- lm(x106kmean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N))
summary(Ct3_5b <- lm(x106kmean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N))
summary(Ct3_5b_nc <- lm(x106kmean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N))
summary(Ct3_5b_3g <- lm(x106kmean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N))


summary(Ct3_8 <- lm(x106kmean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N))
summary(Ct3_8b <- lm(x106kmean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N))
summary(Ct3_8b_nc <- lm(x106kmean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N))
summary(Ct3_8b_3g <- lm(x106kmean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N))

tables = 4
source("analysis/latextables.R") #Will Provide Table 3
rm(Ct3_5,Ct3_5b,Ct3_5b_nc,Ct3_5b_3g,Ct3_8,Ct3_8b,Ct3_8b_nc,Ct3_8b_3g) # Remove the models to save space

# Marginal Effect Analysis
summary(Ct3_5c_me <- lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N))
summary(Ct3_8c_me <- lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N))

me = 3
source("analysis/marginaleffects.R") # Marginal Effects Plots for 3 Groups within party
rm(Ct3_5c_me,Ct3_8c_me)

###########################################
#  End: Simpson Regression Analysis
###########################################


############# Metrics From Clinton #######


###########################################
#   Create Reliabilities Use O'Brien
###########################################

#   Geographic
xbar.lc<-mean(lc) # average ideology across all districts 
msa.lc<-sum(numg*(lc-xbar.lc)^2)/(length(lc)-1) # SUM (1/n-1) Num District Resp *(ideology diff)^2 --> Variance waited by number of people in the pop
msra.lc.p<-sum(numg*sdlc^2) / (sum(numg) - length(lc))
msra.lc.up<-sum((numg*sdlc^2)/(numg-1))/length(lc)
r.lc<-(msa.lc-msra.lc.up)/msa.lc                # AKN -- .86

#   Same-Party
xbar.splc<-sum(splc*numsp)/sum(numsp)
msa.splc<-sum(numsp*(splc-xbar.splc)^2)/(length(splc)-1)
msra.splc.p<-sum(numsp*sdsplc^2) / (sum(numsp) - length(splc))
msra.splc.up<-sum((numsp*sdsplc^2)/(numsp-1))/length(splc)
(msa.splc-msra.splc.up)/msa.splc                # .97

#   Non-same-party                          #   Non-Wgt.
xbar.nsp<-sum(nsplc*numnsp)/sum(numnsp)               
msa.nsp<-sum(numnsp*(nsplc -xbar.nsp)^2)/(length(splc)-1)
msra.nsp.p<-sum(numnsp*sdnsplc^2) / (sum(numnsp) - length(splc))
msra.nsp.up<-sum((numnsp*sdnsplc^2)/(numnsp-1))/length(splc)
(msa.nsp-msra.nsp.up)/msa.nsp                   # .89


###########################################
#   Wright, Erikson, McIver
###########################################

eev<-mean(sdlc/numg)
tv.lc<-var(lc)
(tv.lc-eev)/tv.lc   #  AKN .84

eev<-mean(sdsplc/numsp)
tv.splc<-var(splc)
(tv.splc-eev)/tv.splc      # AKN .95

eev<-mean(sdnsplc/numnsp)
tv.nsplc<-var(nsplc)
(tv.nsplc-eev)/tv.nsplc    # AKN .85

#   Reps Only
eev<-mean(sdsplc[R==1]/numsp[R==1])
tv.splc<-var(splc[R==1])
(tv.splc-eev)/tv.splc      # .32

eev<-mean(sdnsplc[R==1]/numnsp[R==1])
tv.nsplc<-var(nsplc[R==1])
(tv.nsplc-eev)/tv.nsplc    # .63

#   Dems Only
eev<-mean(sdsplc[R==0]/numsp[R==0])
tv.splc<-var(splc[R==0])
(tv.splc-eev)/tv.splc      # .70

eev<-mean(sdnsplc[R==0]/numnsp[R==0])
tv.nsplc<-var(nsplc[R==0])
(tv.nsplc-eev)/tv.nsplc    # .64


#eev<-mean(HOUSE.106$sdilc/HOUSE.106$numilc)    #   Indpendents
#tv.ilc<-var(HOUSE.106$ilc)
#r.ilc<- (tv.ilc-eev)/tv.ilc # .33

#eev<-mean(HOUSE.106$sdoplc/HOUSE.106$numoplc)  #   Out-party
#tv.oplc<-var(HOUSE.106$oplc)
#r.oplc<- (tv.oplc-eev)/tv.oplc  # .77
