###########################################################################
# Simpson Replication Project
# 
###########################################################################


############################################################################
## Plot constituency ideology, calculate reliability of constituency means
##
##  Code for "Representation in Congress: Constituents and Roll Calls"
##  Forthcoming in Journal of Politics
## 
##  106th house
##  Josh Clinton
##  Princeton University
##  May 2005
############################################################################

rm(list=ls())

library(ggplot2)
#install.packages("ggpubr")
install.packages("gridExtra")
library("gridExtra")
library("ggpubr")

setwd("/Users/dsimp/GitHub/Clinton(2006)Rep/analysis")

HOUSE.106<-read.table(file="/Users/dsimp/GitHub/Clinton(2006)Rep/raw/dataverse_files/HOUSE.106.AKN.R")

R<-HOUSE.106$pty
R[R==100]<-0
R[R==200]<-1
R[R==328]<-0


#####################################
#   PLOT JOINT DISTRIBUTION
#####################################

rep<-HOUSE.106$pty
rep[rep==100]<-2
rep[rep==200]<-17
rep[rep==328]<-2

postscript(file="/Users/dsimp/GitHub/Clinton(2006)Rep/docs/Figure1.ps",
           horizontal=F,width=7,height=7)
par(mfrow=c(2,2))
plot(HOUSE.106$lc,HOUSE.106$x106mean,xlab="District Ideology",ylab="Rep. Induced Preference",pch=rep)
plot(HOUSE.106$splc,HOUSE.106$x106mean,xlab="Same-Party Ideology",ylab="Rep. Induced Preference", pch=rep)
plot(HOUSE.106$lc,HOUSE.106$splc,ylab="Same-Party Ideology",xlab="District Ideology", pch=rep)
plot(HOUSE.106$splc,HOUSE.106$nsplc,ylab="Same-Party Ideology",xlab="Non-Same-Party Ideology", pch=rep)
dev.off()

############# Replication Figure 1 #######################
party = R
party[party==0] = "Democrat"
party[party==1] = "Republican"

p1= ggplot(HOUSE.106, aes(x = HOUSE.106$lc, y = HOUSE.106$x106mean, col = party, shape=party))+
  geom_point() + geom_smooth(method = "lm")  +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))+
  labs(x ="District Ideology", y= "Rep Induced Preference", 
       title = "Rep. Preference vs Dist. Ideology", 
       shape="Rep Party")+
  guides(col=FALSE)+ theme(legend.position="bottom") + guides(shape = FALSE)
p1

p2= ggplot(HOUSE.106, aes(x = HOUSE.106$splc, y = HOUSE.106$x106mean, col = party, shape=party))+
  geom_point() + geom_smooth(method = "lm")  +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))+
  labs(x ="Same Party Ideology", y= "Rep Induced Preference", 
       title = "Rep. Preference vs Same Party Ideology", 
       shape="Rep Party")+
  guides(col=FALSE, shape = FALSE) # + theme(legend.position="bottom")
p2


p3= ggplot(HOUSE.106, aes(x = HOUSE.106$lc, y = HOUSE.106$splc, col = party, shape=party))+
  geom_point() +  geom_smooth(method = "lm")  +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))+
  labs(x ="District Ideology", y= "Same Party Ideology", 
       title = "Dist.: Same Party vs Average", 
       shape="Rep Party")+
  guides(col=FALSE) + theme(legend.position="bottom")
p3


p4= ggplot(HOUSE.106, aes(x = HOUSE.106$splc, y = HOUSE.106$nsplc, col = party, shape=party))+
  geom_point() +  geom_smooth(method = "lm")  +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))+
  labs(x ="Same Party Ideology", y= "Nonsame Party Ideology", 
       title = "Dist. Ideology: Nonsame Party vs Same Party", 
       shape="Rep Party")+
  guides(col=FALSE) + theme(legend.position="bottom")
p4


plot1 = ggarrange(p1, p2, p3, p4, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

plot1
setwd("/Users/dsimp/GitHub/Clinton(2006)Rep/docs")
ggsave(filename="plot1.pdf", plot=plot1,width=8.5,height=11)
############# Replication Figure 1 #######################


###########################################
#   Get Descriptives
###########################################

attach(HOUSE.106)
median(splc)
median(lc)

sd(splc)
sd(lc)

###########################################
#   Create Weights
###########################################
poo<-numsp+numnsp             
a<-(numsp/poo)
b<-(numnsp/poo)

c<-(numi/poo)
d<-(numop/poo)

R<-HOUSE.106$pty
R[R==100]<-0
R[R==200]<-1
R[R==328]<-0
R.indx<- R*seq(1,dim(HOUSE.106)[1],by=1)       #   Republican index

pctspid<-a*splc
pctnspid<-b*nsplc

###########################################
#   Create Reliabilities Use O'Brien
###########################################

#   Geographic
xbar.lc<-mean(lc)
msa.lc<-sum(numg*(lc-xbar.lc)^2)/(length(lc)-1)
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
