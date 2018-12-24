# Author:       David Simpson
# Title:        
# Class:        
# Created:      20 December 2018
# Edited:       20 December 2018
# Adapted From: 
# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1)   
#               (2)

# rm(list=ls())

# Clinton weights process from Clinton Stata Code:
data$C_N <- data$numsp + data$numi + data$numop # Clinton uses for weights and to weight standard errors
data$C_pctsp <- data$numsp/data$C_N             # Same party share
data$C_pctnsp <- data$numnsp/data$C_N           # Non same party share
data$C_pcti <- data$numi/data$C_N               # Independent share
data$C_pctop <- data$numop/data$C_N             # Opposite party share

# Clinton's Interaction Variables
data$C_pctspid <- data$C_pctsp * data$splc     # Same party percent * same party id
data$C_pctnspid <- data$C_pctnsp*data$nsplc    # Non Same party percent * Non same party id
data$C_pctiid <- data$C_pcti*data$ilc				   # Independent percent * idenependent party id	
data$C_pctopid <- data$C_pctop*data$oplc	     # Opposite party percent * opposite party id

# Simpson weights
data$S_N <- data$numg                          # Full district sample as the denominator party share
data$S_pctsp <- data$numsp/data$S_N            # Same party share           
data$S_pctnsp <- data$numnsp/data$S_N          # Non same party share
data$S_pcti <- data$numi/data$S_N              # Independent share
data$S_pctop <- data$numop/data$S_N            # Opposite party share

# Edit: Drop these variables and use actual interactions
data$S_pctspid <- data$S_pctsp * data$splc     # Same party percent * same party id
data$S_pctnspid <- data$S_pctnsp*data$nsplc    # Non Same party percent * Non same party id
data$S_pctiid <- data$S_pcti*data$ilc				   # Independent percent * idenependent party id	
data$S_pctopid <- data$S_pctop*data$oplc	     # Opposite party percent * opposite party id

