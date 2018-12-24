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

#######################
# Comparison Plots
#######################

# Prepare Data for ggplot interpretation
# Dataset will be each district repeated 3 times sorted first by party and then percent same party
# Column 1 is district ID, Column 2 is group ID  (options are SameParty, Independent, Opposite)
# Column 3 is group ideology, Column 4 is group share
# Column 5 partyname, column 6 district ideology
# Column 7 legislator ideal point (all votes)
# Column 8 legislator ideal point (key votes)
# Column 9 the district plot order (Dems by percent same party, followed by GOP by reveresed percent same party)

data2 <- data[order(-data$R,data$C_pctsp),] # Order data DEM to GOP by percent same party
switch <- data2[which(data2$R==1),] # select GOP districts
switch <- switch[order(-switch$C_pctsp),] #order GOP from smallest percent same party to larger
data2[which(data2$R==1),] <-switch   # Replace the order of GOP
trial <- as.data.frame(matrix(c(rep(0,432*3*9)),nrow=432*3,ncol=9))
trial[1:432,2] <- c(rep("SameParty",432)) #
trial[1:432,1] <- data2[,c("cdcoden")]
trial[1:432,3:8] <- data2[,c("splc","C_pctsp","partyname","lc","x106mean","x106kmean")]
trial[1:432,9]  <- c(1:432)
trial[433:864,2] <- c(rep("Independent",432))
trial[433:864,1] <- data2[,c("cdcoden")]
trial[433:864,3:8] <- data2[,c("ilc","C_pcti","partyname","lc","x106mean","x106kmean")]
trial[433:864,9]  <- c(1:432)
trial[865:1296,2] <- c(rep("OppositeParty",432))
trial[865:1296,1] <- data2[,c("cdcoden")]
trial[865:1296,3:8] <- data2[,c("oplc","C_pctop","partyname","lc","x106mean","x106kmean")]
trial[865:1296,9]  <- c(1:432)

trial2 <- trial
colnames(trial2) <- c("cdcoden","Group","ideology","share","partyname","lc","x106mean","x106kmean","order")

# Test Direction 
# ggplot(data = trial) + geom_bar(aes(y = V4, x = V1, fill = V2),stat="identity")

 
q <-ggplot(data= subset(trial), aes(x = factor(V9), y = V4, fill = V3)) +
  geom_bar(position = position_stack(), stat = "identity", width =.8) +
  #geom_text(aes(label = V2), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip()+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2],name="Group Ideology\n(District Bars)")+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2],name="Legislator Ideology\n(Points)")

# All Votes 
q+ geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V7))/(2*max(trial$V7)), size=1.0),show.legend = F) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V7))/(2*max(trial$V7)), colour=trial$V7))+
  labs(title = "Congressional District Group Distributions and Legislator Ideology (All Votes)",
       subtitle = "District Groups Shares and Scaled Legislator Ideology (Scaled by All Votes)",
       y ="Group Shares (Bars)\nScaled Legislator Ideology (Points)", x= "Districts")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position="bottom")#,axis.title.x=element_blank())
ggsave("drafts/compare/compare1.pdf")

# All Votes scaled by Key Votes
q+ geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V8))/(2*max(trial$V8)), size=1.0),show.legend = F) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V8))/(2*max(trial$V8)), colour=trial$V7))+
  labs(title = "Congressional District Group Distributions and Legislator Ideology (All Votes)",
       subtitle = "District Groups Shares and Scaled Legislator Ideology (Scaled by Key Votes)",
       y ="Group Shares (Bars)\nScaled Legislator Ideology (Points)", x= "Districts")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position="bottom")#,axis.title.x=element_blank())
ggsave("drafts/compare/compare1b.pdf")

#guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)

q+ geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), size=1.0),show.legend = F) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), colour=trial$V8))+
  labs(title = "Congressional District Group Distributions and Legislator Ideology (Key Votes)",
       subtitle = "District Groups Shares and Scaled Legislator Ideology (Scaled by Key Votes)",
       y ="Group Shares (Bars)\nScaled Legislator Ideology (Points)", x= "Districts")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position="bottom")#,axis.title.x=element_blank())
ggsave("drafts/compare/compare2.pdf")


p <-ggplot(data= subset(trial2), aes(x = factor(order), y = share, fill = ideology)) +
  geom_bar(position = position_stack(), stat = "identity", width =.8) +
  #geom_text(aes(label = V2), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip()+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])

p + geom_point(data = trial2,aes(x =factor(order), y = (trial2$x106mean+max(trial2$x106mean))/(2*max(trial2$x106mean)), size=1.0)) + 
  geom_point(data = trial2,aes(x =factor(order), y = (trial2$x106mean+max(trial2$x106mean))/(2*max(trial2$x106mean)), colour=trial2$x106mean))

p + geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), size=1.0)) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), colour=trial$V8))




test <- data$x106kmean - data$x106mean
test1 <- data$lc - data$ilc

summary(lm(test~lc+R,data=subset(data)))
summary(lm(test~splc+ilc+oplc+R,data=subset(data)))
mean(test)
mean(test[which(data$R==1)])
mean(test[which(data$R==0)])







