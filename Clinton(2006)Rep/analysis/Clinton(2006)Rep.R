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

# ########## ### ### ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
##### Begin Section 1: Set Up  ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls()) # Clear Environment
set.seed(8265) 
setwd("/Users/dsimp/GitHub/Clinton(2006)Rep") # Set Working Directory

packages <- c("ggplot2","ggpubr","dplyr", "stargazer","gridExtra","lmtest","sandwich","gridExtra","RColorBrewer")  # List of packages
new.packages <-packages[!(packages %in% installed.packages()[,"Package"])]                       # Uninstalled packages
if(length(new.packages)) install.packages(new.packages) # Install new packages
lapply(packages, library, character.only = TRUE)        # Load packages

#install.packages("broom")
library("broom") # to reshape model output
library("tidyr")
library("stringr")  

# Presenation Type: 
color <- c("#0000ff","#ff0000") # For online
color <- c("#2b83ba","#d7191c") # For photocoppying

# ggplot theme
theme_set(theme_classic()) # For Classic Look
theme_set(theme_minimal()) # For Miminal Look
options(digits = 3) # Set the printed significant digits to 3

# Upload data: Clinton (2006) data is labeled HOUSE.106
HOUSE.106 <- read.table(file="raw/dataverse_files/HOUSE.106.AKN.R")
data <- HOUSE.106 # Refer to Simpson's Code with data

# Summary Table
tables = 1
source("analysis/latextables.R") #  Print a Summary Table of the data in LATEX using stargazer

# Create Republican Indicator Variable
data$R <- 0                      # Set as democrat --> Note this also classifies pty==328 as Democrat
data$R[data$pty==200] <- 1       # Change to 1 for Republican
data$partyname <- "Democrat"   # Democrat Party ID
data$partyname[data$R==1] <- "Republican" # Republican Party ID

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
##### End Section 1: Set Up  ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#####################################
# Simpson District Plots
#####################################
source("analysis/districtplots.R") # To run the histograms of key votes

#####################################
# Simpson Histograms
#####################################
# Edit: To choose which histograms you want to run and when
source("analysis/histograms.R") # To run the histograms of key votes

###########################################
#  Begin: Simpson Weights
###########################################
# Clinton weights process from Clinton Stata Code:
data$C_N = data$numsp + data$numi + data$numop #Clinton uses for weights and to weight standard errors
data$C_pctsp = data$numsp/data$C_N             # Same party share
data$C_pctnsp = data$numnsp/data$C_N           # Non same party share
data$C_pcti = data$numi/data$C_N               # Independent share
data$C_pctop = data$numop/data$C_N             # Opposite party share
 
# Clinton's Interaction Variables
data$C_pctspid = data$C_pctsp * data$splc     # Same party percent * same party id
data$C_pctnspid = data$C_pctnsp*data$nsplc    # Non Same party percent * Non same party id
data$C_pctiid = data$C_pcti*data$ilc				  #	Independent percent * idenependent party id	
data$C_pctopid = data$C_pctop*data$oplc	      #	Opposite party percent * opposite party id

# Simpson weights
data$S_N = data$numg                          # Full district sample as the denominator party share
data$S_pctsp = data$numsp/data$S_N            # Same party share           
data$S_pctnsp = data$numnsp/data$S_N          # Non same party share
data$S_pcti = data$numi/data$S_N              # Independent share
data$S_pctop = data$numop/data$S_N            # Opposite party share

# Edit: Drop these variables and use actual interactions
data$S_pctspid = data$S_pctsp * data$splc     # Same party percent * same party id
data$S_pctnspid = data$S_pctnsp*data$nsplc    # Non Same party percent * Non same party id
data$S_pctiid = data$S_pcti*data$ilc				  #	Independent percent * idenependent party id	
data$S_pctopid = data$S_pctop*data$oplc	      #	Opposite party percent * opposite party id

###########################################
#  End: Simpson Weights
###########################################

###########################################
#  Begin: Fixed Effects
###########################################
data$state <- as.character(data$cdcoden)%>% #Create String Identifiers for each state
  str_pad(4,"left", pad = "0")%>%
  str_sub(1,2)
###########################################
#  End: Fixed Effects
###########################################

###########################################
#  Begin: Mean Center Data
###########################################
center_data <- function(x){
  x - mean(data$x106mean)
}
data$mc_x106mean <- center_data(data$x106mean) 
data$mc_x106kmean <- center_data(data$x106kmean)

###########################################
#  End: Mean Center Data
###########################################

###########################################
#  Begin: Shift Data
###########################################
shift_data <- function(x){
  x+2
}
data$shift_x106mean <- shift_data(data$x106mean) 
data$shift_x106kmean <- shift_data(data$x106kmean) 
###########################################
#  End: Shift Data
###########################################

# Test
summary(lm(x106mean ~0+C_pctsp * splc  + C_pctnsp * nsplc +R, data = data))
summary(lm(x106mean ~0+C_pctsp * splc +R, data = data))

((.924-.91)/3)/((1-.924)/425)


summary(lm(x106mean ~C_pctsp  + splc  + C_pctnsp + nsplc +R, data = data))
summary(lm(x106mean ~ poly(C_pctsp,2) + splc  + poly(C_pctnsp,2) + nsplc +R, data = data))
summary(lm(x106mean ~ 0 + poly(C_pctsp,2) + splc  + poly(C_pctnsp,2) + nsplc +R, data = data))

summary(lm(x106mean ~ln(C_pctsp) * splc  + ln(C_pctnsp) * nsplc +R, data = data))

summary(lm(x106mean ~ 0 + poly(C_pctsp,2)*splc  + poly(C_pctnsp,2)*nsplc +R, data = data))
summary(lm(x106mean ~ 0 + poly(C_pctsp,3)*splc  + poly(C_pctnsp,3)*nsplc +R, data = data))

summary(lm(x106mean ~ 0 + poly(C_pctsp,1)*splc  + poly(C_pcti,1)*ilc+ poly(C_pctop,1)*oplc +R, data = data))

summary(lm(x106mean ~ 0 + lc + poly(C_pctsp,1)*splc  + poly(C_pcti,1)*ilc+ poly(C_pctop,1)*oplc +R, data = data))


summary(lm(x106mean ~ 0 + poly(C_pctsp,2)*splc  + poly(C_pcti,2)*ilc+ poly(C_pctop,2)*oplc +R, data = data))
summary(lm(x106mean ~ 0 + poly(C_pctsp,3)*splc  + poly(C_pcti,3)*ilc+ poly(C_pctop,3)*oplc +R, data = data))

summary(lm(x106mean ~ 0 + poly(C_pctsp,3)*splc  + poly(C_pcti,3)*ilc+ poly(C_pctop,3)*oplc +R, data = subset(data, R==0)))


attach(data)
test<- lm (x106mean ~ lc, data = data)
plot(lc,x106mean,xlab="District Average",ylab="Legislator Ideolgoy")
curve(coef(test)[1]+coef(test)[2]*x,add=TRUE)


summary(lm(x106mean ~ 0 + poly(C_pctsp,1)*splc  + poly(C_pcti,1)*ilc+ poly(C_pctop,1)*oplc + R, data = data))

summary(lm(lc ~ 0 + C_pctsp*splc + C_pctnsp*nsplc +R, data = data))
summary(lm(lc ~ 0 + C_pctsp*splc + C_pcti*ilc + C_pctop*oplc +R, data = data))


summary(lm(lc ~ 0 + C_pctsp*splc + C_pcti*ilc + C_pctop*oplc + poly(C_pctsp,1) + poly(C_pcti,1) + poly(C_pctop,3) +R, data = data))

summary(lm(splc ~ 0  + poly(C_pcti,1)*ilc+ poly(C_pctop,1)*oplc, data = data))
summary(lm(C_pctsp ~ C_pctop, data = data))
summary(lm(splc ~ 0  + poly(C_pcti,1)*ilc+ poly(C_pctop,1)*oplc, data = data))



hist(data$C_pctsp)
min(x106kmean)

install.packages("quantreg")
library("quantreg")


plot(density(resid(Ct1_2)))
plot(density(resid(Ct1_2b)))


plot(x = data$C_pctopid,y = resid(Ct1_1), # data$C_pctsp
     ylab="Residuals", xlab='Variable', col = (R+5)
     main ="Residual Plot" )+
  abline(0,0)

?plot()

plot(data$ilc[which(R==0)],resid(Ct2_8c_1), # data$C_pctsp
     ylab="Residuals", xlab='Variable',
     main ="Residual Plot" )+
  abline(0,0)

plot(x = data$x106mean[which(R==0)],y = fitted(Ct2_8c_1), # data$C_pctsp
     ylab="Fitted", xlab='Variable',
     main ="Residual Plot" )+
  abline(0,0)



plot(x = data$C_pctop[which(R==1)],y = resid(Ct2_5c_1), # data$C_pctsp
     ylab="Residual", xlab='Variable',
     main ="Residual Plot" )+
  abline(0,0)

plot(x= fitted(Ct1_2),y = resid(Ct1_2), # data$C_pctsp
     xlab='Variable', ylab="Residuals", 
     main ="Residual Plot" )

plot(x= fitted(Ct1_2b),y = resid(Ct1_2b), # data$C_pctsp
     xlab='Variable', ylab="Residuals", 
     main ="Residual Plot" )

plot(fitted(Ct1_1),x106mean, # data$C_pctsp
     ylab="Ideal Points", xlab='Fitted',
     main ="Residual Plot" )

cor(resid(Ct2_8c_1),data$splc[which(R==0)])


Ct2_5c_1 = lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N)
Ct2_8c_1 = lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N)

Ct2_5 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N)
Ct2_5b = lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
Ct2_5b_1 = lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
Ct2_5c = lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N)

plot(x = data$C_pctnspid[which(R==1)],y = resid(Ct2_5), # data$C_pctsp
     ylab="Residual", xlab='Variable',
     main ="Residual Plot" )+
  abline(0,0)

plot(x = fitted(Ct2_5),y = Ct2_5$model$x106mean, # data$C_pctsp
     ylab="Actual", xlab='Predicted',
     main ="Residual Plot" )+
  abline(0,1)+
  abline(0,0)

plot(y = fitted(Ct2_5c_1),x = Ct2_5c_1$model$x106mean, # data$C_pctsp
     ylab="Actual", xlab='Predicted',
     main ="Residual Plot" )+
  abline(0,1)+
  abline(0,0)
lines(x = Ct2_5c_1$model$x106mean,y = fitted(Ct2_5c),col="green")

plot(x = data$C_pctsp[which(R==1)] , y = Ct2_5c_1$model$x106mean)+
  lines(Ct2_5c_1)


plot(x = fitted(Ct2_8),y = Ct2_8$model$x106mean, # data$C_pctsp
     ylab="Actual", xlab='Predicted',
     main ="Residual Plot" )+
  abline(0,1)+
  abline(0,0)

corr(data$C_pctop[which(R==0)], resid(Ct2_5))

Ct2_8 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N)
Ct2_8b = lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
Ct2_8b_1 = lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
Ct2_8c = lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N)


?cor.test()
###########################################
#  Begin: Simpson Regression Analysis
###########################################

################### Replicate and analyze Clinton Table 1 ###################
source("analysis/functions.R")
Ct1_1 <-  lm(x106mean ~ lc + R, data = data, weights = C_N)
Ct1_1 # Clinton Table 1 Model 1
Ct1_2 <- lm(x106mean ~ C_pctspid + C_pctnspid + R, data = data, weights = C_N)
Ct1_2 # Clinton Table 1 Model 2
Ct1_2b <- (lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
Ct1_2b # Clinton Table 1 Model 2 Correctly Specified
Ct1_2b_1 <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
Ct1_2b_1 # Clinton Table 1 Model 2 Correctly Specified (No Constant Term). Model written for table formatting
Ct1_2b_fe <- (lm(x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R + factor(state), data = data, weights = C_N))
Ct1_2b_fe

table = 2
source("analysis/latextables.R") #Will Provide Table 2

Ct1_2b_2 <- lm(x106mean ~ 0+ C_pctsp * splc  + C_pctnsp * nsplc + R, data = data, weights = C_N)
Ct1_2b_2 # Clinton Table 1 Model 2 Correctly Specified (No Constant Term). Model written for function syntax

Ct1_2b_3 <- lm(x106mean ~ 0+ C_pctsp * splc  + C_pctnsp * nsplc + R + factor(state), data = data, weights = C_N)
Ct1_2b_3 # Clinton Table 1 Model 2 Correctly Specified (No Constant Term) & Add FE. Model written for function syntax



# Use an F-Test to see if there is a statistical difference between the model with opposite party and the model without.
summary(lm(x106mean ~ 0+ C_pctsp * splc  + C_pctnsp * nsplc + R , data = data, weights = C_N))
summary(lm(x106mean ~ 0+ C_pctsp * splc  + C_pcti * ilc + C_pctop * oplc + R, data = data, weights = C_N))
summary(lm(x106mean ~ 0+ C_pctsp * splc  + C_pcti * ilc + R, data = data, weights = C_N))

summary(lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = data, weights = C_N))
summary(lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N))
summary(lm(x106mean ~  C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N))
# Important to let data vary with % opposite party



# Marginal Effects Plots for Each Interaction
me = 1
source("analysis/marginaleffects.R")


################### ANALYSIS WITH MEAN CENTERED DATA ###################

Ct1_2b_mc <- (lm((x106mean-mean(x106mean)) ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R + factor(state), data = data, weights = C_N))
Ct1_2b_mc <- (lm((x106mean-mean(x106mean)) ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R + factor(state), data = data, weights = C_N))



Ct1_2b_mc <- (lm(mc_x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R + factor(state), data = data, weights = C_N))
Ct1_2b_mc


Ct1_2b_shift <- (lm(shift_x106mean ~ 0 + C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R , data = data, weights = C_N))
Ct1_2b_shift

### TEST
lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N)
lm((x106mean - mean(x106mean)) ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N)


# Replicate Clinton Table 2
Ct2_5 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N)
Ct2_5b = lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
Ct2_5b_1 = lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
Ct2_5c = lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N)


Ct2_8 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N)
Ct2_8b = lm(x106mean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
Ct2_8b_1 = lm(x106mean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
Ct2_8c = lm(x106mean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N)

tables = 3
source("analysis/latextables.R") #Will Provide Table 3

# Marginal Effect Analysis
Ct2_5c_1 = lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N)
Ct2_8c_1 = lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N)

summary(lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc + factor(state), data = subset(data, R==0), weights = C_N))
# Note, Models with state fixed effects do not improve the explainatory power.

#summary(lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N))
#summary(lm(x106mean ~ 0 + C_pctsp * splc + C_pcti * ilc, data = subset(data, R==0), weights = C_N))


marginal_effect(Ct2_5c_1,"C_pctsp","splc")%>% 
  ggplot(aes(z, dy.dx)) +
  geom_line() +
  geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
  geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Marginal Effect of Percent Same-Party",
       subtitle = "By Same-Party Ideology",
       x = "Same-Party Ideology",
       y = "Estimated marginal effect")
ggsave("drafts/me2-1.pdf")
print("me2-1 is Saved")

 marginal_effect(Ct2_5c_1,"splc","C_pctsp")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Same-Party Ideology",
        subtitle = "By Percent Same-Party",
        x = "Percent Same-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me2-2.pdf")
 print("me2-2 is Saved")
 
 marginal_effect(Ct2_5c_1,"C_pcti","ilc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Independent",
        subtitle = "By Independent Ideology",
        x = "Independent Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me2-3.pdf")
 print("me2-3 is Saved")
 
 marginal_effect(Ct2_5c_1,"ilc","C_pcti")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Independent Ideology",
        subtitle = "By Percent Independent",
        x = "Percent Independent",
        y = "Estimated marginal effect")
 ggsave("drafts/me2-4.pdf")
 print("me2-4 is Saved")
 
 marginal_effect(Ct2_5c_1,"C_pctop","oplc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party",
        subtitle = "By Opposite-Party Ideology",
        x = "Opposite-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me2-5.pdf")
 print("me2-5 is Saved")
 
 marginal_effect(Ct2_5c_1,"oplc","C_pctop")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
        subtitle = "By Percent Opposite-Party",
        x = "Percent Opposite-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me2-6.pdf")
 print("me2-6 is Saved")
 
# FOR DEMS

quantile(model.frame(Ct2_8c_1)$splc, c(.05,.95)) 
quantile(model.frame(Ct2_8c_1)$splc, c(.95))
 
 
marginal_effect(Ct2_8c_1,"C_pctsp","splc")%>% 
  ggplot(aes(z, dy.dx)) +
  geom_rect(data=model.frame(Ct2_8c_1), aes(xmin=quantile(model.frame(Ct2_8c_1)$splc, c(.05)),
                                            xmax=quantile(model.frame(Ct2_8c_1)$splc, c(.95)),
                                            ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.1, inherit.aes = FALSE)+
  geom_line() +
  geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
  geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_rug(data = model.frame(Ct2_8c_1), aes(x=model.frame(Ct2_8c_1)$splc),inherit.aes = FALSE)+
  labs(title = "Marginal Effect of Percent Same-Party",
       subtitle = "By Same-Party Ideology",
       x = "Same-Party Ideology",
       y = "Estimated marginal effect")
ggsave("drafts/me3-1.pdf")
print("me3-1 is Saved")
 
 Ct2_8c_1$terms$C_pctsp
 Ct2_8c_1[C_pctsp]
 model.frame(Ct2_8c_1)$C_pctsp
 model.frame(Ct2_8c_1)$"C_pctsp"
 coef(Ct2_8c_1)["splc"] + coef(Ct2_8c_1)["C_pctsp:splc"]*model.frame(Ct2_8c_1)$C_pctsp
 
 dy.dx <- beta.hat[[eff_var]] + beta.hat[[interaction]] * z
 
 
 marginal_effect(Ct2_8c_1,"splc","C_pctsp")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_rect(data=model.frame(Ct2_8c_1), aes(xmin=.306, xmax=.662, ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.1, inherit.aes = FALSE)+
   geom_line() +
   geom_rug(data = model.frame(Ct2_8c_1), aes(x=model.frame(Ct2_8c_1)$C_pctsp),inherit.aes = FALSE)+
   #geom_point(data = model.frame(Ct2_8c_1),aes(x=model.frame(Ct2_8c_1)$C_pctsp,
   #          y=coef(Ct2_8c_1)["splc"] + coef(Ct2_8c_1)["C_pctsp:splc"]*model.frame(Ct2_8c_1)$C_pctsp),
   #          inherit.aes = FALSE)+
   #geom_rect(data=model.frame(Ct2_8c_1), aes(xmin=.306, xmax=.662, ymin=-Inf, ymax=+Inf), fill='pink', alpha= 0.1, inherit.aes = FALSE)+
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Same-Party Ideology",
        subtitle = "By Percent Same-Party",
        x = "Percent Same-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me3-2.pdf")
 print("me3-2 is Saved")
 # Question what share of the districts are above .6%
 

 
 
 marginal_effect(Ct2_8c_1,"C_pcti","ilc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Independent",
        subtitle = "By Independent Ideology",
        x = "Independent Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me3-3.pdf")
 print("me3-3 is Saved")
 
 marginal_effect(Ct2_8c_1,"ilc","C_pcti")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Independent Ideology",
        subtitle = "By Percent Independent",
        x = "Percent Independent",
        y = "Estimated marginal effect")
 ggsave("drafts/me3-4.pdf")
 print("me3-4 is Saved")
 
 marginal_effect(Ct2_8c_1,"C_pctop","oplc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party",
        subtitle = "By Opposite-Party Ideology",
        x = "Opposite-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me3-5.pdf")
 print("me3-5 is Saved")
 
 marginal_effect(Ct2_8c_1,"oplc","C_pctop")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
        subtitle = "By Percent Opposite-Party",
        x = "Percent Opposite-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me3-6.pdf")
 print("me3-6 is Saved")
 
 
 ########## WITH KEY VOTES
 
 # Replicate Clinton Table 2
 Ct3_5 = lm(x106kmean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N)
 Ct3_5b = lm(x106kmean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
 Ct3_5b_1 = lm(x106kmean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
 Ct3_5c = lm(x106kmean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N)
 
 
 Ct3_8 = lm(x106kmean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N)
 Ct3_8b = lm(x106kmean ~ C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
 Ct3_8b_1 = lm(x106kmean ~ 0 + C_pctspid + C_pctnspid+ splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
 Ct3_8c = lm(x106kmean ~ 0 + C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N)
 
 tables = 4
 source("analysis/latextables.R") #Will Provide Table 3
 
 # Marginal Effect Analysis
 Ct3_5c_1 = lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==1), weights = C_N)
 Ct3_8c_1 = lm(x106kmean ~ 0 + C_pctsp * splc + C_pcti * ilc + C_pctop * oplc, data = subset(data, R==0), weights = C_N)
 
 
 marginal_effect(Ct3_5c_1,"C_pctsp","splc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Same-Party",
        subtitle = "By Same-Party Ideology",
        x = "Same-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-1.pdf")
 print("me4-1 is Saved")
 
 marginal_effect(Ct3_5c_1,"splc","C_pctsp")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Same-Party Ideology",
        subtitle = "By Percent Same-Party",
        x = "Percent Same-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-2.pdf")
 print("me4-2 is Saved")
 
 marginal_effect(Ct3_5c_1,"C_pcti","ilc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Independent",
        subtitle = "By Independent Ideology",
        x = "Independent Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-3.pdf")
 print("me4-3 is Saved")
 
 marginal_effect(Ct3_5c_1,"ilc","C_pcti")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Independent Ideology",
        subtitle = "By Percent Independent",
        x = "Percent Independent",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-4.pdf")
 print("me4-4 is Saved")
 
 marginal_effect(Ct3_5c_1,"C_pctop","oplc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party",
        subtitle = "By Opposite-Party Ideology",
        x = "Opposite-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-5.pdf")
 print("me4-5 is Saved")
 
 marginal_effect(Ct3_5c_1,"oplc","C_pctop")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
        subtitle = "By Percent Opposite-Party",
        x = "Percent Opposite-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me4-6.pdf")
 print("me4-6 is Saved")
 
 ## FOR DEMS
 
 
 marginal_effect(Ct3_8c_1,"C_pctsp","splc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Same-Party",
        subtitle = "By Same-Party Ideology",
        x = "Same-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-1.pdf")
 print("me5-1 is Saved")
 
 marginal_effect(Ct3_8c_1,"splc","C_pctsp")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Same-Party Ideology",
        subtitle = "By Percent Same-Party",
        x = "Percent Same-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-2.pdf")
 print("me5-2 is Saved")
 
 marginal_effect(Ct3_8c_1,"C_pcti","ilc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Independent",
        subtitle = "By Independent Ideology",
        x = "Independent Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-3.pdf")
 print("me5-3 is Saved")
 
 marginal_effect(Ct3_8c_1,"ilc","C_pcti")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Independent Ideology",
        subtitle = "By Percent Independent",
        x = "Percent Independent",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-4.pdf")
 print("me5-4 is Saved")
 
 marginal_effect(Ct3_8c_1,"C_pctop","oplc")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party",
        subtitle = "By Opposite-Party Ideology",
        x = "Opposite-Party Ideology",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-5.pdf")
 print("me5-5 is Saved")
 
 marginal_effect(Ct3_8c_1,"oplc","C_pctop")%>% 
   ggplot(aes(z, dy.dx)) +
   geom_line() +
   geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
   geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
   geom_hline(yintercept = 0) +
   labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
        subtitle = "By Percent Opposite-Party",
        x = "Percent Opposite-Party",
        y = "Estimated marginal effect")
 ggsave("drafts/me5-6.pdf")
 print("me5-6 is Saved")
 
 
 
 
 
 
 
 
 
 
 
rm(option)
choice <- c(Ct2_5c_1$coefficients)[which(str_detect(names(Ct2_5c_1$coefficients), ":"))] # Identify possible interaction terms
option <- c(paste("C_pcti","ilc",sep=":"),paste("ilc","C_pcti",sep=":")) # Identify possible ordering of your terms of interest

# Loop to identify correct interaction term
for (i in 1:length(choice)){
  for(j in 1:length(option)){
    if(option[j]==names(choice)[[i]]){
      interaction <- names(choice)[[i]]
    }
  }
}
interaction


Ct2_5 # Clinton Table 2 Model 5

# Correct Values for Clinton Table 2




#Ct1_3      
#Ct1_3        # Clinton Table 1 Model 3 Need to figure out how to replicate EIV reg

# Replicate Clinton with weights as dependent variabless
Ct1_2 <- lm(x106mean ~ C_pctsp*splc + C_pctnsp*nsplc + R, data = data, weights = C_N)
Ct1_2b <- (lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
Ct1_2b

# Replicate with SP OP and IND and Clinton weigths
(Ct1_2c <- lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + data$splc + data$ilc + data$oplc + data$C_pctsp + data$C_pcti + data$C_pctop  +  R, data = data, weights = C_N))

# Table of Initial Models
tables = 3
source("analysis/latextables.R") #Includes

# Replicate Clinton Table 2
Ct2_5 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==1), weights = C_N)
Ct2_5 # Clinton Table 2 Model 5

Ct2_8 = lm(x106mean ~ C_pctspid + C_pctnspid, data = subset(data, R==0), weights = C_N)
Ct2_8 # Clinton Table 2 Model 8

Ct2_5b = lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==1), weights = C_N)
Ct2_5b # Compare to Clinton Table 2 Model 5
Ct2_5c = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==1), weights = C_N)
Ct2_5c # Compare to Clinton Table 2 Model 5

Ct2_8b = lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
Ct2_8b_1 = lm(x106mean ~ splc*C_pctsp + nsplc*C_pctnsp + splc + nsplc + C_pctsp + C_pctnsp, data = subset(data, R==0), weights = C_N)
summary(Ct2_8b_1) # Writing the terms as an interaction or as a separate term gives the same result
Ct2_8b # Compare to Clinton Table 2 Model 8
Ct2_8c = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop, data = subset(data, R==0), weights = C_N)
Ct2_8c # Compare to Clinton Table 2 Model 8

stargazer(Ct2_5,Ct2_5b,Ct2_5c,Ct2_8,Ct2_8b,Ct2_8c, omit.stat=c("LL","ser","f"),
          title = "Table 2 Replication - Clinton Weights",
          dep.var.labels=c("Legislator Ideal Point"),
          column.labels=c("C-M5","E-M5:Int","E-M5:3G", "C-M8","E-M8:A","E-M8:3G"),
          covariate.labels=c("% SP x SP Ideology","% NSP x NSP Ideology","% IND x IND Ideology","% OP x OP Ideology",
                             "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                             "% SP","% NSP","% IND","% OP"))



Ct1_2b = lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)
Ct1_2b
Ct1_2b_2 = lm(x106mean ~  splc*C_pctsp+ nsplc*C_pctnsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)

interplot(m = Ct1_2b_1, var1 = "splc", var2 = "C_pctsp" )
interplot(m = Ct1_2b_2, var1 = "nsplc", var2 = "C_pctnsp" )
coefficients(Ct1_2b_2)["nsplc"]
vcov(Ct1_2b_2)





summ(Ct1_2b_2)
summ(Ct1_2b_2, center = TRUE)
interact_plot(Ct1_2b_2,pred="splc", modx = "C_pctsp", plot.points= TRUE)
?interact_plot()
data$C_pctnsp

#### Plot Standard Errors
Ct1_2b_2 = lm(x106mean ~  splc*C_pctsp+ nsplc*C_pctnsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)

coefficients(Ct1_2b_2)["splc"]
b_splc = coefficients(Ct1_2b_2)["splc"]
b_splc_C_pctsp = coefficients(Ct1_2b_2)["splc:C_pctsp"]
test = seq(-2,2.01, by= .01)
test = as.data.frame(matrix(c(rep(1,2*length(test))),ncol=2, byrow =FALSE,dimnames = list(NULL,c("beta1","beta2"))))
test$beta1 = b_splc
test$beta2 = b_splc_C_pctsp
# pick back up here later
# https://cfss.uchicago.edu/persp013_interaction_terms.html#estimating_model_with_two_continuous_variables

  av = matrix(c(rep(0,15)),nrow=3, byrow =TRUE) 

  
Ct1_2b_2 = lm(x106mean ~  splc*C_pctsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)
  
tidy(lm(x106mean ~  splc*C_pctsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
source("analysis/functions.R") # To run special functions


Ct1_2b_2
Ct1_2b_2$coefficients
test = names(Ct1_2b_2$coefficients)[[which(str_detect(names(Ct1_2b_2$coefficients), ":C_pctsp"))]]


## Write Function for Standard Errors
Ct1_2 <- lm(x106mean ~ C_pctsp*splc + C_pctnsp*nsplc + R, data = data, weights = C_N)
Ct1_2b <- (lm(x106mean ~ C_pctspid + C_pctnspid + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
Ct1_2c <- (lm(x106mean ~  C_pctsp*splc + C_pctnsp*nsplc + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))
Ct1_2d <- (lm(x106mean ~  C_pctnsp*nsplc + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N))



interaction <- names(model$coefficients)[[which(str_detect(names(model$coefficients), ":"))]]
interaction <- names(Ct1_2c$coefficients)[[which(str_detect(names(Ct1_2c$coefficients), ":"))]]
interaction <- names(Ct1_2d$coefficients)[[which(str_detect(names(Ct1_2d$coefficients), ":"))]]

interaction <- c("test","test1")

Ct1_2c$coefficients[which(str_detect(names(Ct1_2c$coefficients), "^splc"))]


interaction <- c(Ct1_2c$coefficients)[[which(str_detect(names(Ct1_2c$coefficients), "splc"))]]


source("analysis/functions.R")

Ct1_2c <- (lm(x106mean ~ 0 +  C_pctsp*splc + C_pctnsp*nsplc +  R, data = data, weights = C_N))
marginal_effect (Ct1_2c,"splc","C_pctsp")%>% 
  ggplot(aes(z, dy.dx)) +
  geom_line() +
  geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
  geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Marginal effect of Same Party Ideology",
       subtitle = "By Same Party Ideology",
       x = "Same Party Ideology",
       y = "Estimated marginal effect")


marginal_effects (Ct1_2c,"C_pctsp","splc")%>% 
  ggplot(aes(z, dy.dx)) +
  geom_line() +
  geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
  geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Marginal effect of Same Party Ideology",
       subtitle = "By Same Party Ideology",
       x = "Same Party Ideology",
       y = "Estimated marginal effect")



print(seq(min(Ct1_2c$model[[mod_var]]), max(Ct1_2c$model[[mod_var]])))

Ct1_2c[["splc"]]



marginal_effects (Ct1_2c,"C_pctnsp","nsplc")%>% 
  ggplot(aes(z, dy.dx)) +
  geom_line() +
  geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
  geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Marginal effect of Same Party Ideology",
       subtitle = "By Same Party Ideology",
       x = "Non-Same Party Ideology",
       y = "Estimated marginal effect")

lm(splc ~ 0 + nsplc, data = data)










# mod_var - name of moderating variable in the interaction
instant_effect <- function(model, mod_var){
  # get interaction term name
  int.name <- names(model$coefficients)[[which(str_detect(names(model$coefficients), ":"))]]
  marg_var <- str_split(int.name, ":")[[1]][[which(str_split(int.name, ":")[[1]] != mod_var)]]
  
  # store coefficients and covariance matrix
  beta.hat <- coef(model)
  cov <- vcov(model)
  
  # possible set of values for mod_var
  if(class(model)[[1]] == "lm"){
    z <- seq(min(-0), max(model$model[[mod_var]]))
  } else {
    z <- seq(min(model$data[[mod_var]]), max(model$data[[mod_var]]))
  }
  
  # calculate instantaneous effect
  dy.dx <- beta.hat[[marg_var]] + beta.hat[[int.name]] * z
  
  # calculate standard errors for instantaeous effect
  se.dy.dx <- sqrt(cov[marg_var, marg_var] +
                     z^2 * cov[int.name, int.name] +
                     2 * z * cov[marg_var, int.name])
  
  # combine into data frame
  data_frame(z = z,
             dy.dx = dy.dx,
             se = se.dy.dx)
}

  # line plot
  instant_effect(Ct1_2b_2, "splc") %>%
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    labs(title = "Marginal effect of GOP",
         subtitle = "By respondent conservatism",
         x = "Respondent conservatism",
         y = "Estimated marginal effect")
  
  
  
  
  
tidy(lm(x106mean ~  splc*C_pctsp+ nsplc*C_pctnsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)0
  
tidy(lm(x106mean ~ splc + nsplc, data = filter(data, R==0)))
  

  
  
# Plot for splc

# Facet histogram grid with plotly
test = ggplot(data, aes(x=data$x106mean))+
  geom_histogram(alpha=0.5,binwidth = .1, color = "white") #,aes(y=..density..))
test + facet_grid(data$partyname ~ .) + scale_fill_manual(values=color)
ggplotly()


ggplot(data)

h1 = ggplot(data, aes(x=data$x106mean, color=data$partyname,fill=data$partyname  ))+
  geom_histogram(position = "identity",alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  theme_classic()+
  geom_rug()+
  labs(title="Distribution of Legislator Ideal Points",x="Legislator Ideal Point", y = "Count",
       color = "Party")+ #guides(fill = FALSE) 
  theme(legend.position="none")+
  geom_vline(data = av, aes(xintercept = av$`Mean Ideology`), linetype="dashed")+
  scale_colour_manual(values=color)+
  scale_fill_manual(values=color)
#geom_density(alpha = .6)
h1




Ct1_2b_2 = lm(x106mean ~  splc*C_pctsp+ nsplc*C_pctnsp + splc + nsplc + C_pctsp + C_pctnsp +  R, data = data, weights = C_N)





# key votes
# key votes
# key votes





# Replicate Clinton Table 1
Ct1k_1 = lm(x106mean ~ lc + R, data = data, weights = C_N)
Ct1k_1 # Clinton Table 1 Model 1
Ct1k_2 = lm(x106mean ~ C_pctspid + C_pctnspid + R, data = data, weights = C_N)
Ct1k_2 # Clinton Table 1 Model 2
#Ct1_3      
#Ct1_3        # Clinton Table 1 Model 3 Need to figure out how to replicate EIV reg

# Replicate Clinton with weights as dependent variabless
Ct1k_2b = lm(x106kmean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctsp + data$C_pctnsp +  R, data = data, weights = C_N)
Ct1k_2b

# Replicate with SP OP and IND and Clinton weigths
Ct1k_2c = lm(x106kmean ~ C_pctspid + C_pctiid + C_pctopid + data$splc + data$ilc + data$oplc + data$C_pctsp + data$C_pcti + data$C_pctop  +  R, data = data, weights = C_N)
Ct1k_2c
stargazer(Ct1k_1,Ct1k_2,Ct1k_2b,Ct1k_2c, omit.stat=c("LL","ser","f"),
          title = "Table 1 Replication - Clinton Weights",
          dep.var.labels=c("Legislator Ideal Point (Key Votes)"),
          column.labels=c("C-M1","C-M2","E-M2: All", "E-M2: 3G"),
          covariate.labels=c("Mean Ideology","% SP x SP Ideology","% NSP x NSP Ideology","% IND x IND Ideology","% OP x OP Ideology",
                             "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                             "% SP","% NSP","% IND","% OP","GOP Indicator"))

# Replicate Clinton Table 2
Ct2k_5 = lm(x106kmean ~ C_pctspid + C_pctnspid, data = data, weights = C_N, subset = R==1)
Ct2k_5 # Clinton Table 2 Model 5
Ct2k_8 = lm(x106kmean ~ C_pctspid + C_pctnspid, data = data, weights = C_N, subset = R==0)
Ct2k_8 # Clinton Table 2 Model 8

Ct2k_5b = lm(x106kmean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctsp + data$C_pctnsp, data = data, weights = C_N, subset = R==1)
Ct2k_5b # Compare to Clinton Table 2 Model 5
Ct2k_5c = lm(x106kmean ~ C_pctspid + C_pctiid + C_pctopid + data$splc + data$ilc + data$oplc + data$C_pctsp + data$C_pcti + data$C_pctop, data = data, weights = C_N, subset = R==1)
Ct2k_5c # Compare to Clinton Table 2 Model 5

Ct2k_8b = lm(x106kmean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctsp + data$C_pctnsp, data = data, weights = C_N, subset= R==0)
Ct2k_8b # Compare to Clinton Table 2 Model 8
Ct2k_8c = lm(x106kmean ~ C_pctspid + C_pctiid + C_pctopid + data$splc + data$ilc + data$oplc + data$C_pctsp + data$C_pcti + data$C_pctop, data = data, weights = C_N, subset= R==0)
Ct2k_8c # Compare to Clinton Table 2 Model 8

stargazer(Ct2k_5,Ct2k_5b,Ct2k_5c,Ct2k_8,Ct2k_8b,Ct2k_8c, omit.stat=c("LL","ser","f"),
          title = "Table 2 Replication (Key Votes) - Clinton Weights",
          dep.var.labels=c("Legislator Ideal Point (Key Votes)"),
          column.labels=c("C-M5","E-M5:Int","E-M5:3G", "C-M8","E-M8:Int","E-M8:3G"),
          covariate.labels=c("% SP x SP Ideology","% NSP x NSP Ideology","% IND x IND Ideology","% OP x OP Ideology",
                             "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                             "% SP","% NSP","% IND","% OP"))























Ct2_5b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid, data = data, weights = C_N, subset = R==1)
Ct2_5b # Compare to Clinton Table 2 Model 5



Ct2_5b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid, data = data, weights = C_N, subset = R==1)
Ct2_5b # Compare to Clinton Table 2 Model 5
Ct2_8b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid, data = data, weights = C_N, subset = R==0)
Ct2_8b # Compare to Clinton Table 2 Model 8





Ct1_2c = lm(x106mean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctnsp + data$C_pctsp +  R, data = data, weights = C_N,subset = R==1)
Ct1_2c
Ct1_2d = lm(x106mean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctnsp + data$C_pctsp +  R, data = data, weights = C_N, subset = R==0)
Ct1_2d     

# Replicate with Democrats Republicans and Independents and Clinton weigths
Ct1_2b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + R, data = data, weights = C_N)
Ct1_2b # Compare to Clinton Table 1 Model 2

stargazer(Ct1_2c,Ct1_2d)



# Replicate Clinton Table 2
Ct2_5 = lm(x106mean ~ C_pctspid + C_pctnspid, data = data, weights = C_N, subset = R==1)
Ct2_5 # Clinton Table 2 Model 5
Ct2_8 = lm(x106mean ~ C_pctspid + C_pctnspid, data = data, weights = C_N, subset = R==0)
Ct2_8 # Clinton Table 2 Model 8

Ct2_5b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid, data = data, weights = C_N, subset = R==1)
Ct2_5b # Compare to Clinton Table 2 Model 5
Ct2_8b = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid, data = data, weights = C_N, subset = R==0)
Ct2_8b # Compare to Clinton Table 2 Model 8

Ct1_2e = lm(x106mean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctnsp + data$C_pctsp + data$numsp + data$numnsp + R, data = data, weights = C_N,subset = R==1)
Ct1_2e
Ct1_2f = lm(x106mean ~ C_pctspid + C_pctnspid + data$splc + data$nsplc + data$C_pctnsp + data$C_pctsp + data$numsp + data$numnsp + R, data = data, weights = C_N, subset = R==0)
Ct1_2f     
stargazer(Ct1_2e,Ct1_2f)



##### now include all interactions 


Ct2_5c = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop + numsp + numi + numop, data = data, weights = C_N, subset = R==1)
Ct2_5c # Compare to Clinton Table 2 Model 5
Ct2_8c = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop + numsp + numi + numop, data = data, weights = C_N, subset = R==0)
Ct2_8c # Compare to Clinton Table 2 Model 8
stargazer(Ct2_5c,Ct2_8c)


# key votes
Ct2_5ck = lm(x106kmean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop + numsp + numi + numop, data = data, weights = C_N, subset = R==1)
Ct2_5ck # Compare to Clinton Table 2 Model 5
Ct2_8ck = lm(x106kmean ~ C_pctspid + C_pctiid + C_pctopid + splc + ilc + oplc + C_pctsp + C_pcti + C_pctop + numsp + numi + numop, data = data, weights = C_N, subset = R==0)
Ct2_8ck # Compare to Clinton Table 2 Model 8
stargazer(Ct2_5ck,Ct2_8ck)




data$C_pctsp = data$numsp/data$C_N             # Same party share
data$C_pctnsp = data$numnsp/data$C_N           # Non same party share
data$C_pcti = data$numi/data$C_N               # Independent share
data$C_pctop = data$numop/data$C_N             # Opposite party share

data$C_pctspid = data$C_pctsp * data$splc     # Same party percent * same party id
data$C_pctnspid = data$C_pctnsp*data$nsplc    # Non Same party percent * Non same party id
data$C_pctiid = data$C_pcti*data$ilc				  #	Independent percent * idenependent party id	
data$C_pctopid = data$C_pctop*data$oplc	      #	Opposite party percent * opposite party id



# Make a table here to compare with Clinton's results
stargazer(Ct1_1,Ct1_2,Ct1_2b, title ="Table 1 Replication - Clinton Weights",omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Legislator Ideal Point"),
          column.labels=c("C-M1","C-M2","R-M2: 3 Groups"),
          covariate.labels=c("Mean Ideology","Same Party Id.","NonSame Party Id.","Independent Id.","Opposite Party Id.",
                             "GOP Indicator")) # Table 1 Comparison

stargazer(Ct2_5,Ct2_5b,Ct2_8,Ct2_8b,title ="Table 2 Replication - Clinton Weights", omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Legislator Ideal Point"),
          column.labels=c("C-M5: GOP","R-M5: 3 Groups","C-M8 DEM","R-M5: 3 Groups"),
          covariate.labels=c("Same Party Id.","NonSame Party Id.","Independent Id.","Opposite Party Id." )) # Table 2 Comparison


# Replicate with Democrats Republicans and Independents and Simpson weigths
St1_1 = lm(x106mean ~ lc + R, data = data, weights = S_N)
St1_1 # Compare to Clinton Table 1 Model 1 -- Redone with full sample weights
St1_2 = lm(x106mean ~ S_pctspid + S_pctnspid + R, data = data, weights = S_N)
St1_2 # Compare to Clinton Table 1 Model 2 --- changed the weights
St1_2b = lm(x106mean ~ S_pctspid + S_pctiid + S_pctopid + R, data = data, weights = S_N)
St1_2b # Compare to Clinton Table 1 Model 2 --- Three groups and changed the weights

St2_5 = lm(x106mean ~ S_pctspid + S_pctiid + S_pctopid + R, data = data, weights = S_N, subset = R==1)
St2_5 # Compare to Clinton Table 2 Model 5
St2_8 = lm(x106mean ~ S_pctspid + S_pctiid + S_pctopid + R, data = data, weights = S_N, subset = R==0)
St2_8 # Compare to Clinton Table 2 Model 8

stargazer(Ct1_1,St1_1,Ct1_2,St1_2,St1_2b, omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Legislator Ideal Point"),
          column.labels=c("C-M1","R-M1 Full N","C-M2","R-M2 Full N","R-M2 N+3G"),
          covariate.labels=c("Mean Ideology","Same Party Id.","NonSame Party Id.","Independent Id.","Opposite Party Id." )) # Table 1 Comparison
stargazer(Ct2_5,St2_5,Ct2_8,St2_8)
          
           # Table 2 Comparison




t1_0 = lm(x106mean ~ lc + R , data=HOUSE.106)
coeftest(t1_0, vcov = vcovHC(t1_0, type="HC1"))
t1_0
t1_1 = lm(x106mean ~ lc + R , data=HOUSE.106, weights = numg)
t1_1
weights2= numsp + numi + numop
t1_2 = lm(x106mean ~ lc + R , data=HOUSE.106, weights = weights2)
t1_2
t1_3 = lm(x106mean ~ pctspid  + pctnspid + pty , data=HOUSE.106)
t1_3
stargazer(t1_0,t1_1, t1_2, t1_3)

###########################################
#  End: Simpson Regression Analysis
###########################################






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
