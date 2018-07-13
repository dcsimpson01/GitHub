# Author: David Simpson
# Date: 2018.03.11
# Updated: 2018.03.11
# Action: Main Script

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

# Source: 1992 Presidential Election Totals:

# Output:
    # (1) pop92.csv ---> from stch-icen1992.txt
    # (2) general92.csv

# ********************************************************************************
#                                 * Prepare Data*
# ********************************************************************************
rm(list=ls()) # Clear enviornment
setwd("/Users/dsimp/GitHub/peg/analysis") # Set Working Directory
source("DataPrep.R")

# Input Files:


# Output Files:

# ********************************************************************************
#                                 * Describe Data*
# ********************************************************************************
source("Describe_Pop.R")
