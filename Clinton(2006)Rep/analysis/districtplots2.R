# Author:       David Simpson
# Title:        
# Class:        
# Created:      09 November 2018
# Edited:       09 November 2018
# Adapted From: 
# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1) 
#               (2)

# rm(list=ls())

#####################################
# Begin: Simpson Replication of Figure 2
#####################################
#Plot 1 - Ideal Points vs District Mean Ideology
p1 <- ggplot(data, aes(x = data$lc, y = data$x106mean, col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$lc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs District Ideology",
       subtitle = "District Mean Ideology",
       x ="District Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_abline(intercept = 0)+
  guides(col=FALSE) + theme(legend.position="bottom") + guides(shape = FALSE)
p1
ggsave("drafts/plot1-1.pdf")

#Plot 2 - Ideal Points vs Same-Party Mean Ideology
p2 <- ggplot(data, aes(x = data$splc, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$splc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Same-Party Mean Ideology",
       subtitle = "District Same-Party Mean Ideology",
       x ="Same-Party Mean Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p2
ggsave("drafts/plot1-2.pdf")

#Plot 3 - Ideal Points vs Non-Same-Party Mean Ideology
p3 <- ggplot(data, aes(x = data$nsplc, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$nsplc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Non-Same-Party Mean Ideology",
       subtitle = "District Non-Same-Party Mean Ideology",
       x ="Non-Same-Party Mean Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p3
ggsave("drafts/plot1-3.pdf")

#Plot 4 -  Ideal Points vs Opposite Mean Ideology
p4 <- ggplot(data, aes(x = data$oplc, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$oplc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Opposite Party Mean Ideology",
       subtitle = "District Opposite Party Mean Ideology",
       x ="Opposite Party Mean Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p4
ggsave("drafts/plot1-4.pdf")

#Plot 5 - Ideal Points vs Independent Mean Ideology
p5 <- ggplot(data, aes(x = data$ilc, y = data$x106mean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$ilc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs Independent Mean Ideology",
       subtitle = "District Independent Mean Ideology",
       x ="Independent Mean Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p5
ggsave("drafts/plot1-5.pdf")

#Plot 6 - Same Party Mean vs. District Mean
p6 <- ggplot(data, aes(x = data$lc, y = data$splc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$lc, y = data$splc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle =  "Same-Party Mean vs. District Mean",
       x ="District Mean Ideology", y= "Same-Party Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p6
ggsave("drafts/plot1-6.pdf")

#Plot 7 - Non-Same-Party Mean vs District Mean
p7 <- ggplot(data, aes(x = data$lc, y = data$nsplc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$lc, y = data$nsplc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Non-Same-Party Mean vs District Mean", 
       x ="District Mean Ideology", y= "Non-Same-Party Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p7
ggsave("drafts/plot1-7.pdf")

#Plot 8 - Opposite Party Mean vs District Mean
p8 <- ggplot(data, aes(x = data$lc, y = data$oplc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$lc, y = data$oplc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Opposite Party Mean vs District Mean", 
       x ="District Mean Ideology", y= "Opposite Party Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p8
ggsave("drafts/plot1-8.pdf")

#Plot 9 - Independent Mean vs District Mean
p9 <- ggplot(data, aes(x = data$lc, y = data$ilc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$lc, y = data$ilc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Independent Mean vs District Mean", 
       x ="District Mean Ideology", y= "Independent Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p9
ggsave("drafts/plot1-9.pdf")


#Plot 10 - Non-Same Party Mean vs Same-Party Mean
p10 <- ggplot(data, aes(x = data$splc, y = data$nsplc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$splc, y = data$nsplc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Non-Same-Party Mean vs Same-Party Mean", 
       x ="Same-Party Mean Ideology", y= "Non-Same-Party Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p10
ggsave("drafts/plot1-10.pdf")

#Plot 11 - Opposite Party Mean vs Same Party Mean 
p11 <- ggplot(data, aes(x = data$splc, y = data$oplc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$splc, y = data$oplc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Opposite Party Mean vs Same-Party Mean", 
       x ="Same-Party Mean Ideology", y= "Opposite Party Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p11
ggsave("drafts/plot1-11.pdf")

#Plot 12 - Independent Mean vs Same Party Mean
p12 <- ggplot(data, aes(x = data$splc, y = data$ilc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  geom_smooth(aes(x = data$splc, y = data$ilc),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "District Ideology Comparison",
       subtitle = "Independent Mean vs Same-Party Mean", 
       x ="Same-Party Mean Ideology", y= "Independent Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p12
ggsave("drafts/plot1-12.pdf")
rm(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)
#####################################
# End: Simpson Replication of Figure 2
#####################################

#####################################
# Begin: Simpson Figure 2 Looking at Key Votes and Independents
#####################################
#Plot 1 - 
g1b <- ggplot(data, aes(x = C_pctsp, y = splc, col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  #geom_smooth(aes(x = data$C_pctsp*splc, y = data$x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  labs(title = "Legislator Ideal Point vs District Ideology",
       subtitle = "Key Votes Ideal Point vs District Mean Ideology",
       subtitle = "District d Ideology",
       x ="District Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom") + guides(shape = FALSE)
g1b #+ scale_x_continuous(breaks = seq(0, 100, by = 10))+
 # scale_y_continuous(breaks = seq(-2.5, 2.5, by = .5))
#ggsave("drafts/plot1-1b.pdf")

  
#Plot 2 - 
p2b <- ggplot(data, aes(x = data$splc, y = data$x106kmean,  col = partyname, shape=partyname))+
  geom_point() + geom_smooth(method = "lm")  +
  labs(title = "Legislator Ideal Point vs District Ideology",
       subtitle = "Key Votes Ideal Point vs District Same Party Mean Ideology",
       x ="Same Party Mean Ideology", y= "Legislator Ideal Point", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p2b
ggsave("plot1-2b.pdf")


#Plot 3 - 
p3b <- ggplot(data, aes(x = data$lc, y = data$ilc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  labs(title = "District Ideology Comparison",
       subtitle =  "District Mean vs. Independent Mean",
       x ="District Mean Ideology", y= "Independent Mean Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p3b
ggsave("drafts/plot1-3b.pdf")

#Plot 4 - 
p4b <- ggplot(data, aes(x = data$splc, y = data$oplc, col = partyname, shape=partyname))+
  geom_point() +  geom_smooth(method = "lm")  +
  labs(title = "District Ideology Comparison",
       subtitle =  "Same Party Mean vs. Opposite Party Mean",
       x ="District Mean Ideology", y= "Opposite Party Ideology", 
       shape="Rep Party")+
  scale_colour_manual(values=color)+
  guides(col=FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
p4b
ggsave("drafts/plot1-4b.pdf")
rm(p1b,p2b,p3b,p4b)
#####################################
# End: Simpson Figure 2 - Looking at Key Votes and Independents
#####################################
