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

##################
# Summary Table 1
##################

cor(data$x106mean,data$x106kmean) # Correlation between the two vote types is .944

# Party Votes Table
s1 = matrix(c(rep(0,4*3)),nrow=3)
# All Votes Mean
s1[1,1] <- mean(data$x106mean[which(data$R==1)])
s1[2,1] <- mean(data$x106mean[which(data$R==0)])
s1[3,1] <- mean(data$x106mean)
# All Votes SD
s1[1,2] <- sd(data$x106mean[which(data$R==1)])
s1[2,2] <- sd(data$x106mean[which(data$R==0)])
s1[3,2] <- sd(data$x106mean)
# Key Votes Mean
s1[1,3] <- mean(data$x106kmean[which(data$R==1)])
s1[2,3] <- mean(data$x106kmean[which(data$R==0)])
s1[3,3] <- mean(data$x106kmean)
# Key Votes SD
s1[1,4] <- sd(data$x106kmean[which(data$R==1)])
s1[2,4] <- sd(data$x106kmean[which(data$R==0)])
s1[3,4] <- sd(data$x106kmean)
# Make latex Output
rownames(s1) <- c("Republican","Democratic","All")
colnames(s1) <- c("Mean","SD","Mean","SD")

kable(s1,"latex",caption ="Legislator Ideal Points",booktabs=T)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c(" ", "All Votes" = 2, "Key Votes" = 2))%>%
  row_spec(0, align = "c")%>%
  add_footnote(c("The correlation coefficient between the two ideology measures is .944"))%>%
  capture.output()%>%
  writeLines("drafts/summary/summary1.txt")

rm(s1) # Drop s1

##################
# Summary Test Table 1
##################

# T-tests
a <- tidy(t.test(x106mean~R,data=data, var.equal = FALSE))       # Two sided t-test with All Votes Ideal Points
b <- tidy(t.test(abs(x106mean)~R,data=data, var.equal = FALSE))  # Two sided t-test with Absolute Value of All Votes Ideal Points
sum(data$x106mean[data$R==0]>=0) # 51
sum(data$x106mean[data$R==1]<=0) # 0

c <- tidy(t.test(x106kmean~R,data=data, var.equal = FALSE))      # Two sided t-test with Key Votes Ideal Points
d <- tidy(t.test(abs(x106kmean)~R,data=data, var.equal = FALSE)) # Two sided t-test with Absolute Value of Key Votes Ideal Points
sum(data$x106kmean[data$R==0]>=0) # 5
sum(data$x106kmean[data$R==1]<=0) # 1

s1s <- matrix(c(t(a),t(b),t(c),t(d)),nrow=10)[1:8,1:4]
rownames(s1s) <- c("Difference","Democratic","Republican","t-stat","p-value","df","CI-Low","CI-High")
colnames(s1s) <- c("Ideal","Abs Ideal","Ideal","Abs Ideal")
s1s <-s1s[c(3,2,1,4:8),1:4]

# Make latex Output
kable(s1s,"latex",caption="Welch's t-test - Party Mean Ideal Points",booktabs=T)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("","All Votes" = 2, "Key Votes" = 2))%>%
  row_spec(0, align = "c")%>%
  add_footnote(c("Note: Among all votes Republicans (Democrats) have 0 (51) observation(s) less (greater) than zero. Among key votes
  #               Republicans (Democrats) have 1 (5) observation(s) less (greater) than zero."))%>%
  capture.output()%>%
  writeLines("drafts/summary/summarystat1.txt")

rm(a,b,c,d,s1s) # Drop a, b, c, d and s1s


##################
# Summary Table 2
##################

s2 = matrix(c(rep(0,10*4)),nrow=10)
# Ideology Means
s2[1,1] <- mean(data$lc[which(data$R==1)])         # GOP  
s2[2,1] <- mean(data$splc[which(data$R==1)])       # GOP
s2[3,1] <- mean(data$nsplc[which(data$R==1)])      # GOP
s2[4,1] <- mean(data$ilc[which(data$R==1)])        # GOP
s2[5,1] <- mean(data$oplc[which(data$R==1)])       # GOP
s2[6,1] <- mean(data$lc[which(data$R==0)])         # DEM  
s2[7,1] <- mean(data$splc[which(data$R==0)])       # DEM
s2[8,1] <- mean(data$nsplc[which(data$R==0)])      # DEM
s2[9,1] <- mean(data$ilc[which(data$R==0)])        # DEM
s2[10,1] <- mean(data$oplc[which(data$R==0)])      # DEM
# Ideology SD
s2[1,2] <- sd(data$lc[which(data$R==1)])         # GOP  
s2[2,2] <- sd(data$splc[which(data$R==1)])       # GOP
s2[3,2] <- sd(data$nsplc[which(data$R==1)])      # GOP
s2[4,2] <- sd(data$ilc[which(data$R==1)])        # GOP
s2[5,2] <- sd(data$oplc[which(data$R==1)])       # GOP
s2[6,2] <- sd(data$lc[which(data$R==0)])         # DEM  
s2[7,2] <- sd(data$splc[which(data$R==0)])       # DEM
s2[8,2] <- sd(data$nsplc[which(data$R==0)])      # DEM
s2[9,2] <- sd(data$ilc[which(data$R==0)])        # DEM
s2[10,2] <- sd(data$oplc[which(data$R==0)])      # DEM
# Share Average
#s2[1,3] <- mean(data$lc[which(data$R==1)])         # GOP  
s2[2,3] <- mean(data$C_pctsp[which(data$R==1)])       # GOP
s2[3,3] <- mean(data$C_pctnsp[which(data$R==1)])      # GOP
s2[4,3] <- mean(data$C_pcti[which(data$R==1)])        # GOP
s2[5,3] <- mean(data$C_pctop[which(data$R==1)])       # GOP
#s2[6,3] <- mean(data$lc[which(data$R==0)])         # DEM  
s2[7,3] <- mean(data$C_pctsp[which(data$R==0)])       # DEM
s2[8,3] <- mean(data$C_pctnsp[which(data$R==0)])      # DEM
s2[9,3] <- mean(data$C_pcti[which(data$R==0)])        # DEM
s2[10,3] <- mean(data$C_pctop[which(data$R==0)])      # DEM
# Share SD
#s2[1,4] <- sd(data$lc[which(data$R==1)])         # GOP  
s2[2,4] <- sd(data$C_pctsp[which(data$R==1)])       # GOP
s2[3,4] <- sd(data$C_pctnsp[which(data$R==1)])      # GOP
s2[4,4] <- sd(data$C_pcti[which(data$R==1)])        # GOP
s2[5,4] <- sd(data$C_pctop[which(data$R==1)])       # GOP
#s2[6,4] <- sd(data$lc[which(data$R==0)])         # DEM  
s2[7,4] <- sd(data$C_pctsp[which(data$R==0)])       # DEM
s2[8,4] <- sd(data$C_pctnsp[which(data$R==0)])      # DEM
s2[9,4] <- sd(data$C_pcti[which(data$R==0)])        # DEM
s2[10,4] <- sd(data$C_pctop[which(data$R==0)])      # DEM
# Make latex Output
rownames(s2) <- c("District","Same-Party","Non-Same-Party","Independent","Opposite-Party","District","Same-Party","Non-Same-Party","Independent","Opposite-Party")
colnames(s2) <- c("Mean","SD","Mean","SD")

kable(s2,"latex",caption ="Group Ideology and Share by Legislator Party",booktabs=T)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c(" ", "Ideology" = 2, "Shares" = 2))%>%
  row_spec(0, align = "c")%>%
  #add_footnote(c(""))%>%
  group_rows("Republican", 1, 5) %>%
  group_rows("Democratic", 6, 10)%>%
  capture.output()%>%
  writeLines("drafts/summary/summary2.txt")

# Replace the 0.000
tx  <- readLines("drafts/summary/summary2.txt")
tx2  <- gsub(pattern = "0.000", replace = "", x = tx)
writeLines(tx2, con="drafts/summary/summary2.txt")

t.test(ilc~R,data=data, var.equal = FALSE)
t.test(data$ilc[which(data$R==0)],mu=0, var.equal = FALSE)

rm(s2)
rm(tx)
rm(tx2)
