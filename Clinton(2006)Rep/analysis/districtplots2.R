# Author:       David Simpson
# Title:        
# Class:        
# Created:      20 December 2018
# Edited:       21 December 2018
# Adapted From: 
# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1) 
#               (2)

# rm(list=ls())

#####################################
# Begin: Create Share Plots
#####################################

#Plot 1 - Ideal Points vs Same-Party Share
sp1 <- ggplot(data, aes(x = data$C_pctsp, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$C_pctsp, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Same-Party Share",
       subtitle = "Same-Party Share of District Voters",
       x ="Same-Party Share", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
sp1
ggsave("drafts/plots/shareplot1.pdf")

#Plot 2 - Ideal Points vs Non-Same-Party Share
sp2 <- ggplot(data, aes(x = data$C_pctnsp, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$C_pctnsp, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Non-Same-Party Share",
       subtitle = "Non-Same-Party Share of District Voters",
       x ="Non-Same-Party Share", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
sp2
ggsave("drafts/plots/shareplot2.pdf")

#Plot 3 -  Ideal Points vs Opposite Share
sp3 <- ggplot(data, aes(x = data$C_pctop, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$C_pctop, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Opposite Party Share",
       subtitle = "Opposite Party Share of District Voters",
       x ="Opposite Party Share", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
sp3
ggsave("drafts/plots/shareplot3.pdf")

#Plot 4 - Ideal Points vs Independent Share
sp4 <- ggplot(data, aes(x = data$C_pcti, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$C_pcti, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Independent Share",
       subtitle = "Independent Share of District Voters",
       x ="Independent Share", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
sp4
ggsave("drafts/plots/shareplot4.pdf")

# Remove plots
rm(sp1,sp2,sp3,sp4)

#####################################
# End: Create Share Plots
#####################################