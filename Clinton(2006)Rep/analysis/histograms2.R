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
# Begin: Histogram of Party Shares
#####################################
### Distribution of District SP Share
h21 <- ggplot(data, aes(x=data$C_pctsp, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .05)+ #,aes(y=..density..))+
  labs(title="Distribution of District SP Share",
       subtitle = "By Legislator's Political Party",
       x="District SP Share", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$C_pctsp), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$C_pctsp[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$C_pctsp[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h21
ggsave("drafts/histogram/histogram2-1.pdf")

### Distribution of District NSP Mean Ideology
h22 <- ggplot(data, aes(x=data$C_pctnsp, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .05)+ #,aes(y=..density..))+
  labs(title="Distribution of District NSP Share",
       subtitle = "By Legislator's Political Party",
       x="District NSP Mean Ideology", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$C_pctnsp), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$C_pctnsp[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$C_pctnsp[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h22
ggsave("drafts/histogram/histogram2-2.pdf")


### Distribution of District OP Mean Ideology
h23 <- ggplot(data, aes(x=data$C_pctop, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .05)+ #,aes(y=..density..))+
  theme_classic()+
  labs(title="Distribution District OP Share",
       subtitle = "By Legislator's Political Party",
       x="District OP Share", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$C_pctop), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$C_pctop[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$C_pctop[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h23
ggsave("drafts/histogram/histogram2-3.pdf")

### Distribution of District IND Mean Ideology
h24 <- ggplot(data, aes(x=data$C_pcti, color=data$partyname, fill=data$partyname))+
  geom_histogram(position = "identity", alpha=0.5,binwidth = .05)+#,binwidth = .1)+ #,aes(y=..density..))+
  theme_classic()+
  labs(title="Distribution District IND Share",
       x="District Mean IND Share", y = "Count",
       color = "Party")+
  geom_rug()+
  geom_vline(xintercept = mean(data$C_pcti), color = "black", linetype = "dotdash",size=.7)+
  geom_vline(xintercept = mean(data$C_pcti[which(data$partyname=="Democrat")]), color = color[1], linetype = "longdash",size=.7)+ 
  geom_vline(xintercept = mean(data$C_pcti[which(data$partyname=="Republican")]), color = color[2], linetype = "dashed",size=.7)+    
  scale_colour_manual(values=color) +
  scale_fill_manual(values=color)+
  theme(legend.position="none")
h24
ggsave("drafts/histogram/histogram2-4.pdf")

rm(h21,h22,h23,h24)
#####################################
# End: Simpson Histogram Ideology
#####################################
