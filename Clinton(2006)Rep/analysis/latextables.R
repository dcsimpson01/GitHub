# Author:       David Simpson
# Title:        
# Class:        
# Created:      DD Month YYYY
# Edited:       DD Month YYYY
# Adapted From: 
# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1) 
#               (2)

# rm(list=ls())
if (tables==1){
  data2= data
  colnames(data2) = c("Ideal Point", "Ideal Point KV", "Ideal Point Precision", "Ideal Point KV Precision", "District ID", "Party ID",
                    "Mean Ideolgoy","Mean SP Ideology","Mean NSP Ideology","Mean OP Ideology","Mean IND Ideology","SD Ideology", "StDev SP Ideology",
                    "SD NSP Ideology","SD OP Ideology","SD IND Ideology", "Respondents","SP Respondents","NSP Respondents",
                    "IND Respondents","OP Respondents","Avg 2P Pres Vote","District SqMi", "Tenure")
  writeLines(capture.output(stargazer(data2, float = TRUE, float.env = "sidewaystable",
                                    header = FALSE,
                                    median = TRUE,
                                    type = "latex",
                                    title = "Clinton (2006) Data Set",
                                    notes = c("Note: Ideal points are ideology scores for legislators. \"Ideology\" is an ideology score for district and sub-district constituents.",
                                              "KV = Key Votes. SP = Same-Party, NSP = Non-same-Party, OP = Opposite Party, IND = Independent"),
                                    notes.align = "l",
                                    style = "ajps")), 
           "drafts/stargazer/table1.txt")
  rm(data2)
  print("Table 1 is Saved")

}else if (tables==2){
  writeLines(capture.output(stargazer(Ct1_2,Ct1_2b,Ct1_2b_nc,Ct1_2b_mc,Ct1_2b_3g,
                                      #omit = "state",
                                      omit.stat=c("LL","ser","f"),
                                      #omit.labels = "State FE",
                                      #add.lines = list(c("State FE","No", "No","No","Yes","Yes","No")),
                                      title = "Legislator Ideal Point (All Votes) Responsiveness to Sub-district Groups",
                                      dep.var.labels=c("Legislator Ideal Point","Mean Centered","2Pt Shift"),
                                      #column.labels=c("C-M1","C-M2","E-M2: All", "E-M2: 3G"),
                                      #covariate.labels=c("\\%SP x SP Ideology","\\%NSP x NSP Ideology","SP Ideology","NSP Ideology",
                                      #                   "\\%SP","\\%NSP","GOP Indicator"),
                                      # The above labels match when Ct1_2b_3g is not included.
                                      # The below lables match when Ct1_2b_3g is included.
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),                                      
                                      header = FALSE,
                                      #notes = c("Note: Model 1 is the Original Clinton Model that omits the constitutive",
                                      #          "terms. Model 2 is the correctly specified model. Model 3 is the correclty",
                                      #          "specified model omitting the constant terms for marginal effects",
                                      #          "interpretation. Model 4 uses mean centered data. However, mean centering",
                                      #          "the group share variables causes the \\%NSP variable to drop due to",
                                      #          " multicolinearity."),
                                      #above notes for four models. Below notes for 5 models
                                      notes = c("Note: Model 1 is the Original Clinton Model that omits the constitutive terms. Model 2 ",
                                                "is the correctly specified model. Model 3 is the correclty specified model omitting the",
                                                "constant terms for marginal effects interpretation. Model 4 uses mean centered data.",
                                                "However, mean centering the group share variables causes the \\%NSP variable to drop",
                                                "due to multicolinearity. Model 5 is the model specified with three sub-district groups."),
                                      table.placement = "!htbp",
                                      style = "ajps")),
             "drafts/stargazer/table2.txt")
  print("Table 2 is Saved")
}else if (tables==2.2){  
  writeLines(capture.output(stargazer(Ct1_2bk,Ct1_2b_nck,Ct1_2b_3gk,
                                      #omit = "state",
                                      omit.stat=c("LL","ser","f"),
                                      #omit.labels = "State FE",
                                      #add.lines = list(c("State FE","No", "No","No","Yes","Yes","No")),
                                      title = "Legislator Ideal Point (All Votes) Responsiveness to Sub-district Groups",
                                      dep.var.labels=c("Legislator Ideal Point","Mean Centered","2Pt Shift"),
                                      #column.labels=c("C-M1","C-M2","E-M2: All", "E-M2: 3G"),
                                      #covariate.labels=c("\\%SP x SP Ideology","\\%NSP x NSP Ideology","SP Ideology","NSP Ideology",
                                      #                   "\\%SP","\\%NSP","GOP Indicator"),
                                      # The above labels match when Ct1_2b_3g is not included.
                                      # The below lables match when Ct1_2b_3g is included.
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),                                      
                                      header = FALSE,
                                      #notes = c("Note: Model 1 is the Original Clinton Model that omits the constitutive",
                                      #          "terms. Model 2 is the correctly specified model. Model 3 is the correclty",
                                      #          "specified model omitting the constant terms for marginal effects",
                                      #          "interpretation. Model 4 uses mean centered data. However, mean centering",
                                      #          "the group share variables causes the \\%NSP variable to drop due to",
                                      #          " multicolinearity."),
                                      #above notes for four models. Below notes for 5 models
                                      notes = c("Note: Model 1 is the Original Clinton Model that omits the",
                                                "constitutive terms. Model 2 is the correclty specified model",
                                                "omitting the constant terms for marginal effects interpretation",
                                                "Model 3 is the model specified with three sub-district groups."),
                                      table.placement = "!htbp",
                                      style = "ajps")),
             "drafts/stargazer/table2.2.txt")
  print("Table 2.2 is Saved")
  
}else if (tables==3){
  writeLines(capture.output(stargazer(Ct2_5,Ct2_5b_nc,Ct2_5b_3g,Ct2_8,Ct2_8b_nc,Ct2_8b_3g, omit.stat=c("LL","ser","f"),
                                      title = "Partisan Ideal Point (All Votes) Responsiveness to Sub-district Groups",
                                      dep.var.labels=c("Legislator Ideal Point"),
                                      #column.labels=c("Rep-GOP","GOP 2","GOP 3", "Rep-DEM","DEM 2","DEM 3"),
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),
                                      header = FALSE,
                                      style = "ajps")),
             "drafts/stargazer/table3.txt")
  print("Table 3 is Saved")
  
}else if (tables==4){
  writeLines(capture.output(stargazer(Ct3_5,Ct3_5b_nc,Ct3_5b_3g,Ct3_8,Ct3_8b_nc,Ct3_8b_3g, omit.stat=c("LL","ser","f"),
                                      title = "Legislative Ideal Points (Key Votes) and Sub-District Ideology",
                                      dep.var.labels=c("Legislator Ideal Point"),
                                      #column.labels=c("Rep-GOP","GOP 2","GOP 3", "Rep-DEM","DEM 2","DEM 3"),
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),
                                      header = FALSE,
                                      style = "ajps")),
             "drafts/stargazer/table4.txt")
  print("Table 4 is Saved")
}



