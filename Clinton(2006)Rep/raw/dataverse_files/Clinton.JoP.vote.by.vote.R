############################################################################
## Read raw votes data from Poole & Rosenthal
## Recodes for missing data, re-shape as a matrix, and analyze individually
##
##  106th house
##  Josh Clinton
##  Princeton University
##  May 2005
############################################################################

#   Functions for reading data in

options(object.size=10e9)

readfunc <- function(file="data",first,last){
  foo <- scan(file=file,what=list(x=""),sep="\n")$x
  z <- substring(foo,first=first,last=last)
  z
}

readvotes <- function(file="data",first){
  foo <- readLines(file)
  n <- length(foo)
  m <- length(first:nchar(foo[1]))
  votes <- matrix(NA,n,m)
  for (i in 1:n){
    votes[i,] <- as.numeric(unlist(strsplit(substring(foo[i],first=first),"")))
  }
  votes
}

############################################################
##  Read Data in    
############################################################

h106name  <- readfunc(file="***PATH***/hou106kh.ord.txt",26,36)
h106party <- readfunc(file="***PATH***/hou106kh.ord.txt",21,24)
h106votes  <- readvotes(file="***PATH***/hou106kh.ord.txt",37)

#####################################################################
##  Prepare Data for BUGS
#####################################################################

h106votes[h106votes==0] <- NA
h106votes[h106votes==6] <- 0
h106votes[h106votes>1] <- NA

#   Change nrow to number of legs

votes <- h106votes

votes<-votes[-c(49,50,61,169,259,260,411,412),]     #   Votes both sessions drop Martinez (D,R),Brown, Livingston Drop Goode and Forbes
h106name[c(49,50,61,169,259,260,411,412)]           #   check


#key.vote.1<-votes[,c(55,57,102,205,211,242,274,317,331,420,463,488,542)]   #   session 1 votes
#key.vote.2<-votes[,c(651,801,815,835,861,887,963,1006,947,878,1036,1083)]  #   session 2 votes
key.vote<-votes[,c(55,57,102,205,211,242,274,317,331,420,463,488,542,651,801,815,835,861,887,963,1006,947,878,1036,1083)]

## calculate vote margins

h106margin <- apply(votes,2,
                function(x){
                  ok <- !is.na(x)
                  z <- c(sum(x[ok]==0),sum(x[ok]==1),sum(!ok))
                  z
                }
                )
h106margin <- t(h106margin)
h106totvot <- apply(h106margin,1,function(x)sum(x[1:2]))

## indicators for lop-sided votes (less than 2.5% either for or against)

h106lop <- apply(h106margin,1,
             function(x){
               lop <- x[1]/h106totvot < .025 | x[2]/h106totvot < .025 
               lop
             }
             )
h106lop<-h106lop[1,]
votes<-votes[,!h106lop]     #   drop-lop-sided votes

##########################################
#   Get summary model for individual votes
#   Table 4 results
##########################################

#   Pooled Model
#   Get constituency preference measures

HOUSE.106 <- read.table(file="***PATH***/HOUSE.106.AKN.ipe.R")

R<-HOUSE.106$pty
R[R==100]<-0
R[R==200]<-1
R[R==328]<-0

coef<-matrix(NA,nrow=4,ncol=dim(votes)[2])          #   Create matrices for holding results
pvalue<-matrix(NA,nrow=4,ncol=dim(votes)[2])

pctsplc<-HOUSE.106$splc*(HOUSE.106$numsp/(HOUSE.106$numsp+HOUSE.106$numnsp))    #   Create weighted pref measures: same-party
pctnsplc<-HOUSE.106$nsplc*(HOUSE.106$numnsp/(HOUSE.106$numsp+HOUSE.106$numnsp)) #   Create weighted pref measures: non-same-party
    
for(i in 1:dim(votes)[2]){
    poo1 <- glm(votes[,i]~R+pctsplc+pctnsplc,family=binomial(link="logit"))     #   logit model for each roll-call
    pvalue[,i]<-summary(poo1)$coef[,4]                                          #   store p-values    
    coef[,i]<-summary(poo1)$coef[,1]                                            #   store coefficients
    }

sig<-matrix(0,nrow=dim(pvalue)[1],ncol=dim(pvalue)[2])  

for(i in 1:dim(pvalue)[1]){
    for(j in 1:dim(pvalue)[2]){
        sig[i,j]<-as.logical(pvalue[i,j]< .05)      #   Test for which coefficeints are significnat at .05
    }}

poo1<-rep(NA,times=dim(sig)[2])     #   Create holding matrices
poo2<-rep(NA,times=dim(sig)[2])
poo3<-rep(NA,times=dim(sig)[2])
poo4<-rep(NA,times=dim(sig)[2])
poo5<-rep(NA,times=dim(sig)[2])
poo6<-rep(NA,times=dim(sig)[2])
poo7<-rep(NA,times=dim(sig)[2])

for(i in 1:dim(sig)[2]){            #   Now count the incidence of significant coefficients across roll calls
    poo1[i] <- as.logical(sig[2,i]==0 & sig[3,i]==0 & sig[4,i]==1)      #   JUST NSPLC
    poo2[i] <- as.logical(sig[2,i]==0 & sig[3,i]==1 & sig[4,i]==1)      #   SPLC and NSPLC
    poo3[i] <- as.logical(sig[2,i]==0 & sig[3,i]==1 & sig[4,i]==0)      #   JUST SPLC
    poo4[i] <- as.logical(sig[2,i]==1 & sig[3,i]==0 & sig[4,i]==0)      #   JUST PARTY
    poo5[i] <- as.logical(sig[2,i]==1 & sig[3,i]==0 & sig[4,i]==1)      #   NSPLC and PARTY
    poo6[i] <- as.logical(sig[2,i]==1 & sig[3,i]==1 & sig[4,i]==1)      #   SPLC and NSPLC and PARTY
    poo7[i] <- as.logical(sig[2,i]==1 & sig[3,i]==1 & sig[4,i]==0)      #   SPPLC and PARTY
    }

dim(sig)[2] - sum(poo1) - sum(poo2) - sum(poo3) - sum(poo4) - sum(poo5) - sum(poo6) - sum(poo7)

#   Replicate the analysis for Republicans Only
###############################################################################################################

votesR<-votes[R==1,]    #   select votes by Republican incumbents only

#   Trim to non-unamious votes -- 582 votes
#   Smaller number because we are conditioning on the party of the incumbent

h106margin <- apply(votesR,2,
                function(x){
                  ok <- !is.na(x)
                  z <- c(sum(x[ok]==0),sum(x[ok]==1),sum(!ok))
                  z
                }
                )
h106margin <- t(h106margin)
h106totvot <- apply(h106margin,1,function(x)sum(x[1:2]))

## indicators for lop-sided votes (less than 2.5% either for or against)

h106lop <- apply(h106margin,1,
             function(x){
               lop <- x[1]/h106totvot < .025 | x[2]/h106totvot < .025 
               lop
             }
             )
h106lop<-h106lop[1,]
votesR<-votesR[,!h106lop]

coefR<-matrix(NA,nrow=3,ncol=dim(votesR)[2])
pvalueR<-matrix(NA,nrow=3,ncol=dim(votesR)[2])

for(i in 1:dim(votesR)[2]){
    pooR <- glm(votesR[,i]~pctsplc[R==1]+pctnsplc[R==1],family=binomial(link="logit"))
    pvalueR[,i]<-summary(pooR)$coef[,4]
    coefR[,i]<-summary(pooR)$coef[,1]
    }

sigR<-matrix(0,nrow=dim(pvalueR)[1],ncol=dim(pvalueR)[2])

for(i in 1:dim(pvalueR)[1]){
    for(j in 1:dim(pvalueR)[2]){
        sigR[i,j]<-as.logical(pvalueR[i,j]< .05)
    }}


poo1<-rep(NA,times=dim(sigR)[2])
poo2<-rep(NA,times=dim(sigR)[2])
poo3<-rep(NA,times=dim(sigR)[2])

for(i in 1:dim(sigR)[2]){
    poo1[i] <- as.logical(sigR[2,i]==0 & sigR[3,i]==1)      #   JUST NSPLC
    poo2[i] <- as.logical(sigR[2,i]==1 & sigR[3,i]==1)      #   SPLC and NSPLC
    poo3[i] <- as.logical(sigR[2,i]==1 & sigR[3,i]==0)      #   JUST SPLC
    }

sum(poo1)
sum(poo2)
sum(poo3)
dim(sigR)[2] - sum(poo1) - sum(poo2) - sum(poo3)


#   Replicate the analysis for Democrats Only
###############################################################################################################

votesD<-votes[R==0,]

#   Trim to non-unamious votes -- 582 votes

h106margin <- apply(votesD,2,
                function(x){
                  ok <- !is.na(x)
                  z <- c(sum(x[ok]==0),sum(x[ok]==1),sum(!ok))
                  z
                }
                )
h106margin <- t(h106margin)
h106totvot <- apply(h106margin,1,function(x)sum(x[1:2]))

## indicators for lop-sided votes (less than 2.5% either for or against)

h106lop <- apply(h106margin,1,
             function(x){
               lop <- x[1]/h106totvot < .025 | x[2]/h106totvot < .025 
               lop
             }
             )
h106lop<-h106lop[1,]
votesD<-votesD[,!h106lop]

coefD<-matrix(NA,nrow=3,ncol=dim(votesD)[2])
pvalueD<-matrix(NA,nrow=3,ncol=dim(votesD)[2])
signD<-rep(NA,times=dim(votesD)[2])

for(i in 1:dim(votesD)[2]){
    pooD <- glm(votesD[,i]~pctsplc[R==0]+pctnsplc[R==0],family=binomial(link="logit"))
    pvalueD[,i]<-summary(pooD)$coef[,4]
    coefD[,i]<-summary(pooD)$coef[,1]
    signD[i]<-as.logical(summary(pooR)$coef[2,1] > 0) == as.logical(summary(pooD)$coef[3,1] > 0)
    }

sigD<-matrix(0,nrow=dim(pvalueD)[1],ncol=dim(pvalueD)[2])

for(i in 1:dim(pvalueD)[1]){
    for(j in 1:dim(pvalueD)[2]){
        sigD[i,j]<-as.logical(pvalueD[i,j]< .05)
    }}

poo1<-rep(NA,times=dim(sigD)[2])
poo2<-rep(NA,times=dim(sigD)[2])
poo3<-rep(NA,times=dim(sigD)[2])

for(i in 1:dim(sigD)[2]){
    poo1[i] <- as.logical(sigD[2,i]==0 & sigD[3,i]==1)      #   JUST SPLC
    poo2[i] <- as.logical(sigD[2,i]==1 & sigD[3,i]==1)      #   SPLC and NSPLC
    poo3[i] <- as.logical(sigD[2,i]==1 & sigD[3,i]==0)      #   JUST SPLC
    }

sum(poo1)
sum(poo2)
sum(poo3)

dim(sigD)[2] - sum(poo1) - sum(poo2) - sum(poo3)
