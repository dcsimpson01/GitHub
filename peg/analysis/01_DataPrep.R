# Author: David Simpson
# Date: 2018.02.24
# Updated: 2018.03.04
# Action: Data Processing

# ********************************************************************************
#                                 * File Manager*
# ********************************************************************************

# Input:	
  # Source: State and County Intercensal Data: 1990-2000 (https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-1990-2000-state-and-county-characteristics.html)
    # (1) stch-icen1992.txt

  #  Source: ICPSR (https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4421?q=coterminous)
    # (1)	04421-0001-Data.dta 	ICPSR Label == General Purpose Governments  --> This is incorrectly labeled. It should be Special District Governments
    # (2)	04421-0002-Data.dta 	ICPSR Label == Special District Governments --> This is incorrectly labeled. It should be General Purpose Governments
    # (3)	04421-0003-Data.dta 	School District Governments


  # Source: ICPSR (https://www.icpsr.umich.edu/icpsrweb/ICPSR/series/12/studies/4420?searchSource=revise&q=census+of+governments&paging.startRow=1)
    # (4)	04420-0003-Data.dta 	Final Restructured Individual Unit File with Added Identifiers 
    # (5) 01.0 FundNames.do --> Author created

  # Source: State and County Intercensal Data: 1990-2000 (https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-1990-2000-state-and-county-characteristics.html)
    # (6) stch-icen1992.txt

  # Source: US Census: 1990 Census Gazetteer Files (https://www.census.gov/geo/maps-data/data/gazetteer1990.html)
    # (7) counties.txt --> author used Excel to save .txt as a .csv for use in this analysis.

# Source: 1992 Presidential Election Totals:

# Output:
    # (1) pop92.csv ---> from stch-icen1992.txt 
    # (2) general92.csv ---> from the incorrectly labeled Special Purpose 04421-0002 - dta
    # (3) special92.csv ---> from the incorrectly labeled General Purpose 04421-0001 - dta

# ********************************************************************************
#                                 * BEGIN PREP*
# ********************************************************************************
rm(list=ls()) # Clear enviornment

setwd("/Users/dsimp/GitHub/peg/analysis") # Set Working Directory
getwd() 

# Libraries
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("foreign") # Read STATA files
library("haven") # Needed to read STATA files
library("gmodels") # will allow you to use CrossTable() to do cross tabs
library("readtext")

# ********************************************************************************
#                                 * END PREP*
# ********************************************************************************


# ********************************************************************************
#                                 * END PREP*
# ********************************************************************************

#                         **************************
#                         ******* DATA SET 1 *******
#                         *******  Pop92.csv *******
#                         **************************
  
# ********************************************************************************
#  Clean State and County Intercensal Estimates: Age and Race Data (stch-icen1992.txt) -----------------------
# ********************************************************************************

data = read.table("/Users/dsimp/GitHub/peg/raw/01_0_DataPrep/State and County Intercensal Data/stch-icen1992.txt", colClasses = c("integer","character","factor","factor","factor","integer") )
keep=c(2,3,4,5,6) # Columns to keep
data = data[keep] # Keep only 
names(data) = c("FIPS", "Age_Group" , "Race_Sex" , "Ethnic_Origin" , "Pop") # Rename variables

# Create Age Variables
data$Age_Group = as.numeric(as.character(data$Age_Group)) # Convert Age Group from Factor variable to numbers
data$Youth = data$Pop * as.numeric(data$Age_Group <= 4)                  # Age=<19 
data$Adult = data$Pop * as.numeric(data$Age_Group>4 & data$Age_Group<14) # 19 < Age < 65
data$Senior = data$Pop *as.numeric(data$Age_Group >= 14)                 # 65 <= Age

# Create Race Variables
data$Hispanic = as.numeric(data$Ethnic_Origin==2)     # Hispanic Dummy Variable
data$Non_Hispanic = as.numeric(data$Ethnic_Origin==1) # Non Hispanic Dummy Variable

data$White_Non =  data$Pop * as.numeric(data$Race_Sex==1 | data$Race_Sex==2) * data$Non_Hispanic # White Non Hispanic
data$White_His =  data$Pop * as.numeric(data$Race_Sex==1 | data$Race_Sex==2) * data$Hispanic     # White Hispanic

data$Black_Non = data$Pop * as.numeric(data$Race_Sex==3 | data$Race_Sex==4) * data$Non_Hispanic  # Black Non Hispanic
data$Black_His = data$Pop * as.numeric(data$Race_Sex==3 | data$Race_Sex==4) * data$Hispanic      # Black Hispanic

data$Native_Non = data$Pop * as.numeric(data$Race_Sex==5 | data$Race_Sex==6)  * data$Non_Hispanic  # Native Non Hispanic
data$Native_His = data$Pop * as.numeric(data$Race_Sex==5 | data$Race_Sex==6)  * data$Hispanic      # Native Hispanic

data$Asian_Non = data$Pop *  as.numeric(data$Race_Sex==7 | data$Race_Sex==8)  * data$Non_Hispanic  # Asian Non Hispanic
data$Asian_His = data$Pop *  as.numeric(data$Race_Sex==7 | data$Race_Sex==8)  * data$Hispanic      # Asian Hispanic

# Collapse Data into one observation per FIPS
keep = c(1,5:8,11:18) # define variables to keep
data = data[keep] # drop unneeded variables 
data = aggregate(data[, 2:13], by=list(data$FIPS), sum) #aggregate the variables

colnames(data)[1] = "FIPS" # Rename first variable
data$State = substr(data$FIPS, 1, 2) # Create state identifier
order = c(1,14,2:13) # Create order
data = data[order] # Reorder variables

setwd("/Users/dsimp/GitHub/peg/data")
save(data, file = "pop92.Rda")
load("pop92.Rda")
#write.csv(data, "pop92.csv", row.names = FALSE)
#write.dta(data,"pop92.dta")
# ******************************************************************************** -----
# End Clean State and County Intercensal Estimates ------------------
# Data Set Created: "pop92.Rda"


#                         **************************
#                         ******* DATA SET 2 *******
#                         **************************

# ********************************************************************************
#  * Import General Purpose Governments Data (04421-0002-Data.dta)* -----------------------
#  * Note: 04421-0002-Data.dta is incorrectly labled by ICPSR as Special Gov*
# ********************************************************************************

data <- read_dta("~/GitHub/peg/raw/01_0_DataPrep/Census of Governments/DS2 Special District Governments (1992) ICPSR_04421/DS0002/04421-0002-Data.dta")
# File contains #38,978 observations.

# To confirm that this data set is the general purpose data. Observe the third character in the
# GOVCODE. The only values are 1 (County Gov), 2 (Municipal Gov), 3 (Township Gov)
x = substring(data$GOVCODE,3,3)
unique(x)

CrossTable(data$RESCODE, prop.r = T,  format="SPSS") #Survey Response 
# Total Observations in Table:  38978 
#  |        1  |        2  |  1 = Government did respond to survey
#  |-----------|-----------|  2 = Government did not respond to survey
#  |    34132  |     4846  | 
#  |   87.567% |   12.433% | 
#  |-----------|-----------|

CrossTable(data$HOMERU, format="SPSS")  # Homerule Status
#Total Observations in Table:  38977 
#  |        0  |        1  |        2  |  0 = Question not answered or not applicable
#  |-----------|-----------|-----------|  1 = Respondent government reported having a homerule charter
#  |       57  |     5143  |    33777  |  2 = Respondent government reported having no homerule charter 
#  |    0.146% |   13.195% |   86.659% | 
#  |-----------|-----------|-----------|

# Number of Missing Observations: 1 (0.00256555%)




setwd("~/GitHub/peg/raw/data")
save(data, file = "general92.Rda")
#write.csv(data,"general92.csv",row.names = FALSE)
#write.dta(data,"general92.dta")
# ******************************************************************************** -----
# General Purpose Governments Data (04421-0002-Data.csv) ------------------
# Data Set Created: "general92.Rda"


#                         **************************
#                         ******* DATA SET 3 *******
#                         **************************

# ********************************************************************************
#  * Import Special Purpose Data (04421-0001-Data.dta)* -----------------------
#  * Note: 04421-0001-Data.dta is incorrectly labled by ICPSR as General Purpose Gov*
# ********************************************************************************

data <- read_dta("~/GitHub/peg/raw/01_0_DataPrep/Census of Governments/DS1 General Purpose Governments (1992) ICPSR_04421/DS0001/04421-0001-Data.dta")

save(data, file = "special92.Rda")
#write.csv(data,"special92.csv",row.names = FALSE)
#write.dta(data,"special92.dta")
load("special92.Rda")
x = substring(data$GOVCODE,3,3)
unique(x) # Confirm all are Special District Govnberments. Indeed they are all a 4.
# ******************************************************************************** -----
# Special Purpose Governments Data (04421-0001-Data.csv) ------------------
# Data Set Created: "special92.Rda"


#                         **************************
#                         ******* DATA SET 4 *******
#                         **************************

# ********************************************************************************
#  * Import School Data (04421-0003-Data.dta)* -----------------------
# ********************************************************************************
data <- read_dta("~/GitHub/peg/raw/01_0_DataPrep/Census of Governments/DS3 School District Governments (1992) ICPSR_04421/DS0003/04421-0003-Data.dta")

setwd("~/GitHub/peg/raw/data")
#write.csv(data,"school92.csv",row.names = FALSE)
#write.dta(data,"school92.dta")
save(data, file = "school92.Rda")


# ******************************************************************************** -----
# School Data (04421-0003-Data.csv) ------------------
# Data Set Created: "school92.Rda"


#                         **************************
#                         ******* DATA SET 5 *******
#                         **************************

# ********************************************************************************
#  * County Data List (counties.csv)* -----------------------
# ********************************************************************************
data <- read.csv("~/GitHub/peg/raw/01_0_DataPrep/Census_Gazetteer_Files/counties.csv",header=FALSE)
data$FIPS = paste(substr(data$V1,1,2),substr(data$V1,6,8),sep="")
data$State = substr(data$V1,1,2)
data$County = substr(data$V1,6,8)
data$Name = substr(data$V1,10,75)
data$ST = substr(data$V1,77,78)
data$Pop90 = as.numeric(substr(data$V1,80,88))
data$H_Units90 = as.numeric(substr(data$V1,90,98))
data$LandArea = as.numeric(substr(data$V1,100,109))
data$WaterArea = as.numeric(substr(data$V1,111,120))
data$Latitude = substr(data$V1,122,130)
data$Longitude = substr(data$V1,132,141)
data = data[,2:12]

setwd("~/GitHub/peg/raw/data") # Save the data for later
save(data,file = "counties.Rda")
#write.csv(data,"counties.csv", row.names=FALSE)
load("counties.Rda")
# ******************************************************************************** -----
# Counties Data (counties.csv) ------------------
# Data Set Created: "counties.Rda"


