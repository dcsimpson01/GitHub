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
  colnames(data2) = c("Legislator Ideal Pt", "Legisaltor Ideal Pt KV", "Legislator Ideal Pt Prec", "Legislator Ideal Pt KV Prec", "District", "Party",
                    "Mean Ideolgoy","Mean SP Ideology","Mean NSP Ideology","Mean OP Ideology","Mean IND Ideology","StDev Ideology", "StDev SP Ideology",
                    "StDev NSP Ideology","StDev OP Ideology","StDev IND Ideology", "Respondents","SP Respondents","NSP Respondents",
                    "IND Respondents","OP Respondents","Avg 2P Pres Vote","District SqMi", "Tenure")
  writeLines(capture.output(stargazer(data2, float = TRUE, float.env = "sidewaystable",
                                    header = FALSE,
                                    type = "latex",
                                    style = "ajps")), 
           "drafts/table1.txt")
  rm(data2)
  print("Table 1 is Saved")

}else if (tables==2){
  writeLines(capture.output(stargazer(Ct1_2,Ct1_2b,Ct1_2b_1,Ct1_2b_fe,Ct1_2b_mc,Ct1_2b_shift,
                                      omit = "state",
                                      omit.stat=c("LL","ser","f"),
                                      #omit.labels = "State FE",
                                      add.lines = list(c("State FE","No", "No","No","Yes","Yes","No")),
                                      title = "Interction Models with Same-Party and Non-Same-Party",
                                      dep.var.labels=c("Legislator Ideal Point","Mean Centered","2Pt Shift"),
                                      #column.labels=c("C-M1","C-M2","E-M2: All", "E-M2: 3G"),
                                      covariate.labels=c("\\%SP x SP Ideology","\\%NSP x NSP Ideology","SP Ideology","NSP Ideology",
                                                         "\\%SP","\\%NSP","GOP Indicator"),
                                      header = FALSE,
                                      table.placement = "!htbp",
                                      style = "ajps")),
             "drafts/table2.txt")
  print("Table 2 is Saved")
  
}else if (tables==3){
  writeLines(capture.output(stargazer(Ct2_5,Ct2_5b_1,Ct2_5c,Ct2_8,Ct2_8b_1,Ct2_8c, omit.stat=c("LL","ser","f"),
                                      title = "Legislative Ideal Points and Sub-District Ideology",
                                      dep.var.labels=c("Legislator Ideal Point"),
                                      #column.labels=c("Rep-GOP","GOP 2","GOP 3", "Rep-DEM","DEM 2","DEM 3"),
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),
                                      header = FALSE,
                                      style = "ajps")),
             "drafts/table3.txt")
  print("Table 3 is Saved")
  
}else if (tables==4){
  writeLines(capture.output(stargazer(Ct3_5,Ct3_5b_1,Ct3_5c,Ct3_8,Ct3_8b_1,Ct3_8c, omit.stat=c("LL","ser","f"),
                                      title = "Legislative Ideal Points (Key Votes) and Sub-District Ideology",
                                      dep.var.labels=c("Legislator Ideal Point"),
                                      #column.labels=c("Rep-GOP","GOP 2","GOP 3", "Rep-DEM","DEM 2","DEM 3"),
                                      covariate.labels=c("\\% SP x SP Ideology","\\% NSP x NSP Ideology","\\% IND x IND Ideology","\\% OP x OP Ideology",
                                                         "SP Ideology", "NSP Ideology", "IND Ideology","OP Ideology",
                                                         "\\% SP","\\% NSP","\\% IND","\\% OP"),
                                      header = FALSE,
                                      style = "ajps")),
             "drafts/table4.txt")
  print("Table 4 is Saved")
}



