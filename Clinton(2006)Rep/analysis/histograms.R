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
# Note:         Theme is set in the main script


# rm(list=ls())


#####################################
# Begin: Simpson Histogram Votes and KeyVotes
#####################################
#Plot 1 - 

### Distribution of All Votes
h1 = ggplot(data, aes(x=data$x106mean, color=data$partyname,fill=data$partyname  ))+
  geom_histogram(position = "identity",alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  geom_rug()+
  labs(title="Distribution of Legislator Ideal Points",
       subtitle = "By Political Party",
       x="Legislator Ideal Point", y = "Count",
       color = "Party")+ #guides(fill = FALSE) 
  theme(legend.position="none")+
  geom_vline(xintercept = mean(data$x106mean), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$x106mean[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$x106mean[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+  
  scale_colour_manual(values=color)+
  scale_fill_manual(values=color)
h1
ggsave("drafts/histogram/histogram-1.pdf")

### Distribution of Key Votes
h2 = ggplot(data, aes(x=data$x106kmean, color=data$partyname, fill=data$partyname ))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+
  geom_rug()+
  labs(title="Distribution of Legislator Ideal Points",
       subtitle = "By Political Party on Key Votes Only",       
       x="Legislator Ideal Points (Key Votes)", y = "Count",
       color = "Party")+
  theme(legend.position="none")+
  geom_vline(xintercept = mean(data$x106kmean), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$x106kmean[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$x106kmean[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+  
  scale_colour_manual(values=color)+
  scale_fill_manual(values=color)
h2
ggsave("drafts/histogram/histogram-2.pdf")

hc <- ggplot(data, aes(x = data$x106mean, y = data$x106kmean,  col = partyname, shape=partyname)) +
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_abline(intercept = 0)+
  scale_x_continuous(limits=c(-2.2,2.2))+
  scale_y_continuous(limits=c(-2.2,2.2))+
  labs(title="Legislator Ideal Point (Key Votes) vs Legislator Ideal Point",
       x ="Legislator Ideal Point", y= "Legislator Ideal Point (Key Votes)")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
hc
ggsave("drafts/histogram/histo_change.pdf")

#####################################
# End: Simpson Histogram Votes and KeyVotes
#####################################

#####################################
# Begin: Simpson Histogram Ideology
#####################################

### Distribution of District Mean Ideology
h3 <- ggplot(data, aes(x=data$lc, color=data$partyname,fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  labs(title="Distribution District Ideology",
       subtitle = "By Legislator's Political Party",
       x="District Mean Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$lc), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$lc[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$lc[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+  
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h3
ggsave("drafts/histogram/histogram-3.pdf")

### Distribution of District SP Mean Ideology
h4 <- ggplot(data, aes(x=data$splc, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  labs(title="Distribution of District SP Ideology",
       subtitle = "By Legislator's Political Party",
       x="District SP Mean Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$splc), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$splc[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$splc[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h4
ggsave("drafts/histogram/histogram-4.pdf")

### Distribution of District NSP Mean Ideology
h5 <- ggplot(data, aes(x=data$nsplc, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  labs(title="Distribution of District NSP Ideology",
       subtitle = "By Legislator's Political Party",
       x="District NSP Mean Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$nsplc), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$nsplc[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$nsplc[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h5
ggsave("drafts/histogram/histogram-5.pdf")


### Distribution of District OP Mean Ideology
h6 <- ggplot(data, aes(x=data$oplc, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  theme_classic()+
  labs(title="Distribution District OP Ideology",
       subtitle = "By Legislator's Political Party",
       x="District Mean OP Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$oplc), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$oplc[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$oplc[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h6
ggsave("drafts/histogram/histogram-6.pdf")

### Distribution of District IND Mean Ideology
h7 <- ggplot(data, aes(x=data$ilc, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .1)+ #,aes(y=..density..))+
  theme_classic()+
  labs(title="Distribution District IND Ideology",
       x="District Mean IND Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$ilc), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$ilc[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$ilc[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h7
ggsave("drafts/histogram/histogram-7.pdf")

hd <- ggplot(data, aes(x = data$ilc, y = data$oplc,  col = partyname, shape=partyname)) +
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_abline(intercept = 0)+
  scale_x_continuous(limits=c(-.7,1.2))+
  scale_y_continuous(limits=c(-.7,1.2))+
  labs(title="Opposite Party Mean vs Independent Mean",
       x ="Independent Mean Ideology", y= "Opposite Party Mean Ideology")+
  scale_colour_manual(values=color)+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)
hd
ggsave("drafts/histogram/histo_diff.pdf")

rm(h1,h2,h3,h4,h5,h6,h7,hc,hd)
#####################################
# End: Simpson Histogram Ideology
#####################################
