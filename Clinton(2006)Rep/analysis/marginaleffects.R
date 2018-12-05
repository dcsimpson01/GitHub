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


if(me==1){
  marginal_effect(Ct1_2b_3,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_rect(data=model.frame(Ct1_2b_3), aes(xmin=quantile(model.frame(Ct1_2b_3)$C_pctsp, c(.05)),
                                              xmax=quantile(model.frame(Ct1_2b_3)$C_pctsp, c(.95)),
                                              ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = model.frame(Ct1_2b_3), aes(x=model.frame(Ct1_2b_3)$C_pctsp),inherit.aes = FALSE)+
    labs(title = "Marginal Effect of Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")
  ggsave("drafts/me-1.pdf")
  print("me-1 is Saved")
  
  marginal_effect (Ct1_2b_3,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_rect(data=model.frame(Ct1_2b_3), aes(xmin=quantile(model.frame(Ct1_2b_3)$splc, c(.05)),
                                              xmax=quantile(model.frame(Ct1_2b_3)$splc, c(.95)),
                                              ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = model.frame(Ct1_2b_3), aes(x=model.frame(Ct1_2b_3)$splc),inherit.aes = FALSE)+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")
  ggsave("drafts/me-2.pdf")
  print("me-2 is Saved")
  
  marginal_effect (Ct1_2b_3,"nsplc","C_pctnsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_rect(data=model.frame(Ct1_2b_3), aes(xmin=quantile(model.frame(Ct1_2b_3)$C_pctnsp, c(.05)),
                                              xmax=quantile(model.frame(Ct1_2b_3)$C_pctnsp, c(.95)),
                                              ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = model.frame(Ct1_2b_3), aes(x=model.frame(Ct1_2b_3)$C_pctnsp),inherit.aes = FALSE)+
    labs(title = "Marginal Effect of Non-Same-Party Ideology",
         subtitle = "By Percent Non-Same-Party",
         x = "Percent Non-Same-Party",
         y = "Estimated marginal effect")
  ggsave("drafts/me-3.pdf")
  print("me-3 is Saved")
  
  marginal_effect (Ct1_2b_3,"C_pctnsp","nsplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_rect(data=model.frame(Ct2_8c_1), aes(xmin=quantile(model.frame(Ct1_2b_3)$nsplc, c(.05)),
                                              xmax=quantile(model.frame(Ct1_2b_3)$nsplc, c(.95)),
                                              ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = model.frame(Ct1_2b_3), aes(x=model.frame(Ct1_2b_3)$nsplc),inherit.aes = FALSE)+
    labs(title = "Marginal Effect of Percent Non-Same-Party",
         subtitle = "By Non-Same-Party Ideology",
         x = "Non-Same-Party Ideology",
         y = "Estimated marginal effect")
  ggsave("drafts/me-4.pdf")
  print("me-4 is Saved")
}
