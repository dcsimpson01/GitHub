# User: David Simpson
# Note: I edited the below for my purposes
# Date: 4.10.2018
# Author: Jonathan Kastellec
# File from: http://www.princeton.edu/~jkastell/mrp_primer.html
# File Accompanies: Estimating State Public Opinion with Multi-level 
# Regression and Poststratification using R (with Jeffrey Lax and Justin Phillips)

rm(list=ls(all=TRUE))
# install.packages("arm")

setwd("/Users/dsimp/GitHub/MRP_Practice/analysis")

library("arm") # Contains functions to implement and analyze multilevel models inclueind lmer
library("foreign")
library("dplyr") # For data manipulation and other -- Added by David

# Steps Overview:
# Step 1 - gather national opinion polls. See data in raw folder
  # Ex (1) estaimates state-level opinion
  #    (2) Uses ive national polls in 2004

# Step 2 - Recode the polls - See below Step 2
#         Recode as necesary so they can be combined into a single internally-consistent dataset
#   Goal: (1) Use demographic and geographic cahracterisitics to create group (i.e. categorical) variables. 
#         -- Alows for more efficient estimation and you dont need to exclude a reference category
#         -- Ex: Combine race and gender into a single variable w/ six possible categories.
#         -- Can also group by age, educaiton, interaction between age and education state. 
#         -- When identifying respondent demographic data in surveys, be sure to only use data that is also
#            available from the census (otherwise you will not be able to properly post stratify). 

#         -- If you are using survey responses from multiple polls or years you can also create group variables
#            for these as well. This helps control for poll, question wording, and year effects (we do this below)

# Step 3 - Create a separate dataset of state-level predictors
#   Goal: State level effects can be modeled using additional state-level predictors such as region or state-level (aggregate)
#         demographics (e.g. those not avial. in survey or census)
#         -- Adding group-levle predictors usually reduced unexplained group level vriation thrus reducitn group level standard deviation  
# Step 4 - Collect census data to enable poststratification
#   Goal: To weight data by district demogrpahics
#         -- MRP requries more than just the simple state level statistics such as the number of females or AA in a state
#         -- If your model treats opinion as a function of gender, race, age, and education you need to know the number of AA females
#            age 18-29 who are college graduates
#         -- Need a dataset of the population counts for each demogrpahic-state type (or "cell")
# Step 5 - Fit a regression model for an individual survey response given demographics and geogrpahy
#         --
# Step 6 - Postratify the demographic-geographic types
# 


setwd("/Users/dsimp/GitHub/MRP_Practice/raw")

# Step 2: read in megapoll and attach
marriage.data <- read.dta("gay_marriage_megapoll.dta", convert.underscore = TRUE) # Converts variables names with underscores to periods
glimpse(marriage.data)

# Step 3: read in state-level dataset
Statelevel <- read.dta("state_level_update.dta",convert.underscore = TRUE)
glimpse(Statelevel)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),] # Sort by numerical order of state's initials

# Step 4: read in Census data
Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census <- Census[order(Census$cstate),]
Census$cstate.initnum <-  match(Census$cstate, Statelevel$sstate) # Creates a numerical order of state's intials to match to Statelevel data 
# match function to create a variable indicating the state initial number for each cell in the Census data 

#Create index variables for use in individual-level model and in the poststratification

  #At level of megapoll

marriage.data$race.female <- (marriage.data$female *3) + marriage.data$race.wbh# from 1 for white males to 6 for hispanic females
marriage.data$age.edu.cat <- 4 * (marriage.data$age.cat -1) + marriage.data$edu.cat# from 1 for 18-29 with low edu to 16 for 65+ with high edu
marriage.data$p.evang.full <- Statelevel$p.evang[marriage.data$state.initnum]# proportion of evangelicals in respondent's state
marriage.data$p.mormon.full <-Statelevel$p.mormon[marriage.data$state.initnum]# proportion of mormon's in respondent's state
marriage.data$p.relig.full <- marriage.data$p.evang.full + marriage.data$p.mormon.full# combined evangelical + mormom proportions
marriage.data$p.kerry.full <- Statelevel$kerry.04[marriage.data$state.initnum]# kerry's % of 2-party vote in respondent's state in 2004

  #At census level (same coding as above for all variables)

Census$crace.female <- (Census$cfemale *3) + Census$crace.WBH 
Census$cage.edu.cat <- 4 * (Census$cage.cat -1) + Census$cedu.cat 
Census$cp.evang.full<-  Statelevel$p.evang[Census$cstate.initnum]
Census$cp.mormon.full <- Statelevel$p.mormon[Census$cstate.initnum]
Census$cp.relig.full <- Census$cp.evang.full + Census$cp.mormon.full
Census$cp.kerry.full <-  Statelevel$kerry.04[Census$cstate.initnum]


# Step 5: run individual-level opinion model
#   Model treates each individual's response as a funciton of his or her demographics and state
#     (for individual i, with indexes j,k,l,m.s and,p for race-gender combo, age cat, edu cat, region
#     state, and poll respectively, and including an age-edu interaction):

#   Assume evach variable is drawn form a normal distribution with mean zero and some estimated variance for the
#   variables race-gender, age, education, age-edu, poll

#   State effects are in turn modeled as a funciton of the region into which the state falls and the state's 
#   conservative religious percentage and Democratic 2004 vote share

individual.model <- glmer(formula = yes.of.all ~ (1|race.female) + (1|age.cat) 
  + (1|edu.cat) + (1|age.edu.cat) + (1|state) + (1|region) + (1|poll) + p.relig.full
        + p.kerry.full,data=marriage.data, family=binomial(link="logit"))
display(individual.model)

# Note: glmer = Generalized Linear Mixed-Effects Models --> has both fixed effects and random effects
#       Random-effects terms are distinguished by vertical bars ("|") separating expressions for design
#         matrices from grouping factors


#examine random effects and standard errors for race-female
ranef(individual.model)$race.female
se.ranef(individual.model)$race.female

#create vector of state ranefs and then fill in missing ones
state.ranefs <- array(NA,c(51,1))
dimnames(state.ranefs) <- list(c(Statelevel$sstate),"effect")
for(i in Statelevel$sstate){
    state.ranefs[i,1] <- ranef(individual.model)$state[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0 #set states with missing REs (b/c not in data) to zero (Since no repsonses from AK HI)

# Step 6: Post stratification

#create a prediction for each cell in Census data
cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
            +ranef(individual.model)$race.female[Census$crace.female,1]
            +ranef(individual.model)$age.cat[Census$cage.cat,1]
            +ranef(individual.model)$edu.cat[Census$cedu.cat,1]
            +ranef(individual.model)$age.edu.cat[Census$cage.edu.cat,1]
            +state.ranefs[Census$cstate,1]
            +ranef(individual.model)$region[Census$cregion,1]   
            +(fixef(individual.model)["p.relig.full"] *Census$cp.relig.full)
            +(fixef(individual.model)["p.kerry.full"] *Census$cp.kerry.full)
               )

#weights the prediction by the freq of cell                                       
cellpredweighted <- cellpred * Census$cpercent.state

#calculates the percent within each state (weighted average of responses)
statepred <- 100* as.vector(tapply(cellpredweighted,Census$cstate,sum))
statepred
