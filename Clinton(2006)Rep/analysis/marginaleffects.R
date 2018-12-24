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
  marginal_effect(Ct1_2b_me,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    #geom_rect(data=model.frame(Ct1_2b_me), aes(xmin=quantile(model.frame(Ct1_2b_me)$C_pctsp, c(.05)),
    #                                          xmax=quantile(model.frame(Ct1_2b_me)$C_pctsp, c(.95)),
    #                                          ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_rug(data = model.frame(Ct1_2b_me), aes(x=model.frame(Ct1_2b_me)$C_pctsp),inherit.aes = FALSE)+
    geom_rug(data = data, aes(x=data$C_pctsp, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    #geom_density(data = data, aes(x=data$C_pctsp,y=..scaled../5),col="white",fill = "grey",alpha=.2,adjust=1,inherit.aes = FALSE)+
    geom_density(data = data, aes(x=data$C_pctsp,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me-1.pdf")
  print("me-1 is Saved")
  
  marginal_effect (Ct1_2b_me,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    #geom_rect(data=model.frame(Ct1_2b_me), aes(xmin=quantile(model.frame(Ct1_2b_me)$splc, c(.05)),
    #                                          xmax=quantile(model.frame(Ct1_2b_me)$splc, c(.95)),
    #                                          ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    #geom_rug(data = model.frame(Ct1_2b_me), aes(x=model.frame(Ct1_2b_me)$splc),inherit.aes = FALSE)+
    geom_rug(data = data, aes(x=data$splc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    #geom_density(data = data, aes(x=data$splc,y=..scaled../5),col="white",fill = "grey",alpha=.2,adjust=1,inherit.aes = FALSE)+
    geom_density(data = data, aes(x=data$splc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me-2.pdf")
  print("me-2 is Saved")
  
  marginal_effect (Ct1_2b_me,"nsplc","C_pctnsp")%>% 
    ggplot(aes(z, dy.dx)) +
    #geom_rect(data=model.frame(Ct1_2b_me), aes(xmin=quantile(model.frame(Ct1_2b_me)$C_pctnsp, c(.05)),
    #                                          xmax=quantile(model.frame(Ct1_2b_me)$C_pctnsp, c(.95)),
    #                                          ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_rug(data = model.frame(Ct1_2b_me), aes(x=model.frame(Ct1_2b_me)$C_pctnsp),inherit.aes = FALSE)+
    geom_rug(data = data, aes(x=data$C_pctnsp, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    #geom_density(data = data, aes(x=data$C_pctnsp,y=..scaled../5),col="white",fill = "grey",alpha=.2,adjust=1,inherit.aes = FALSE)+
    geom_density(data = data, aes(x=data$C_pctnsp,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Non-Same-Party Ideology",
         subtitle = "By Percent Non-Same-Party",
         x = "Percent Non-Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me-3.pdf")
  print("me-3 is Saved")
  
  marginal_effect (Ct1_2b_me,"C_pctnsp","nsplc")%>% 
    ggplot(aes(z, dy.dx)) +
    #geom_rect(data=model.frame(Ct1_2b_me), aes(xmin=quantile(model.frame(Ct1_2b_me)$nsplc, c(.05)),
    #                                          xmax=quantile(model.frame(Ct1_2b_me)$nsplc, c(.95)),
    #                                          ymin=-Inf, ymax=+Inf), fill='grey', alpha= 0.05, inherit.aes = FALSE)+
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    #geom_rug(data = model.frame(Ct1_2b_me), aes(x=model.frame(Ct1_2b_me)$nsplc),inherit.aes = FALSE)+
    geom_rug(data = data, aes(x=data$nsplc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    #geom_density(data = data, aes(x=data$nsplc,y=..scaled../5),col="white",fill = "grey",alpha=.2,adjust=1,inherit.aes = FALSE)+
    geom_density(data = data, aes(x=data$nsplc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Non-Same-Party",
         subtitle = "By Non-Same-Party Ideology",
         x = "Non-Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me-4.pdf")
  print("me-4 is Saved")


}else if (me==1.1){
  
  # Effect of %SP by SP Ideology
  marginal_effect(Ct1_2b_3gme,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$splc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$splc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-1.pdf")
  print("meb-1 is Saved")
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct1_2b_3gme,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pctsp, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pctsp,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-2.pdf")
  print("meb-2 is Saved")
  
  # Effect of %IND by IND Ideology
  marginal_effect(Ct1_2b_3gme,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$ilc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$ilc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-3.pdf")
  print("meb-3 is Saved")
  
  # Effect of IND Ideology by %IND
  marginal_effect(Ct1_2b_3gme,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pcti, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pcti,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-4.pdf")
  print("meb-4 is Saved")
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct1_2b_3gme,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$oplc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$oplc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-5.pdf")
  print("meb-5 is Saved")
  
  # Effect of OP Ideology by %OP
  marginal_effect(Ct1_2b_3gme,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pctop, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pctop,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/meb-6.pdf")
  print("meb-6 is Saved")
  
}else if (me==1.2){
  # Effect of %SP by SP Ideology
  marginal_effect(Ct1_2b_3keygme,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$splc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$splc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-1.pdf")
  print("mebk-1 is Saved")
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct1_2b_3keygme,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pctsp, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pctsp,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-2.pdf")
  print("mebk-2 is Saved")
  
  # Effect of %IND by IND Ideology
  marginal_effect(Ct1_2b_3keygme,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$ilc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$ilc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-3.pdf")
  print("mebk-3 is Saved")
  
  # Effect of IND Ideology by %IND
  marginal_effect(Ct1_2b_3keygme,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pcti, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pcti,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-4.pdf")
  print("mebk-4 is Saved")
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct1_2b_3keygme,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$oplc, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$oplc,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-5.pdf")
  print("mebk-5 is Saved")
  
  # Effect of OP Ideology by %OP
  marginal_effect(Ct1_2b_3keygme,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data, aes(x=data$C_pctop, col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data, aes(x=data$C_pctop,y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color)+
    scale_fill_manual(values=color)+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/mebk-6.pdf")
  print("mebk-6 is Saved")
  
    
}else if (me==2){
  ###############################
  # All Votes 3 Group Interactions
  ###############################
  
  ###############################
  ########## GOP ################
  ###############################
  
  
  # Effect of %SP by SP Ideology
  marginal_effect(Ct2_5b_me,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = data[which(data$R==1),], aes(x=data$splc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$splc[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-1.pdf")
  print("me2-1 is Saved")
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct2_5b_me,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pctsp[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pctsp[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-2.pdf")
  print("me2-2 is Saved")
  
  # Effect of %IND by IND Ideology
  marginal_effect(Ct2_5b_me,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$ilc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$ilc[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-3.pdf")
  print("me2-3 is Saved")
  
  # Effect of IND Ideology by %IND
  marginal_effect(Ct2_5b_me,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pcti[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pcti[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-4.pdf")
  print("me2-4 is Saved")
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct2_5b_me,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$oplc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$oplc[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-5.pdf")
  print("me2-5 is Saved")
  
  # Effect of OP Ideology by %OP
  marginal_effect(Ct2_5b_me,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pctop[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pctop[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me2-6.pdf")
  print("me2-6 is Saved")
  
  ###############################
  ########## DEM ################
  ###############################
  
  # Effect of %SP by SP Ideology
  marginal_effect(Ct2_8b_me,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$splc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$splc[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-1.pdf")
  print("me3-1 is Saved")
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct2_8b_me,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pctsp[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pctsp[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-2.pdf")
  print("me3-2 is Saved")
  
  # Question what share of the districts are above .6%?
  
  # Effect of %IND by IND Ideology  
  marginal_effect(Ct2_8b_me,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$ilc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$ilc[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-3.pdf")
  print("me3-3 is Saved")
  
  # Effect of IND Ideology by %IND
  marginal_effect(Ct2_8b_me,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pcti[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pcti[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-4.pdf")
  print("me3-4 is Saved") 
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct2_8b_me,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$oplc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$oplc[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-5.pdf")
  print("me3-5 is Saved")
  
  # Effect of OP Ideology by %OP
  marginal_effect(Ct2_8b_me,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pctop[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pctop[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me3-6.pdf")
  print("me3-6 is Saved")
  
}else if (me==3){
  ###############################
  # Key Votes 3 Group Interactions
  ###############################
  
  ###############################
  ########## GOP ################
  ###############################
  
  # Effect of %SP by SP Ideology
  marginal_effect(Ct3_5c_me,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$splc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$splc[which(data$R==1)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-1.pdf")
  print("me4-1 is Saved")
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct3_5c_me,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pctsp[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pctsp[which(data$R==1)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-2.pdf")
  print("me4-2 is Saved")
  
  # Effect of %IND by IND Ideology
  marginal_effect(Ct3_5c_me,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$ilc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$ilc[which(data$R==1)],y=(..scaled../1),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-3.pdf")
  print("me4-3 is Saved")
  
  # Effect of IND Ideology by %IND 
  marginal_effect(Ct3_5c_me,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pcti[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pcti[which(data$R==1)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-4.pdf")
  print("me4-4 is Saved")
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct3_5c_me,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$oplc[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$oplc[which(data$R==1)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-5.pdf")
  print("me4-5 is Saved")
  
  # Effect of OP Ideology by %OP
  marginal_effect(Ct3_5c_me,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==1),], aes(x=data$C_pctop[which(data$R==1)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==1),], aes(x=data$C_pctop[which(data$R==1)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[2])+
    scale_fill_manual(values=color[2])+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me4-6.pdf")
  print("me4-6 is Saved")
  
  ###############################
  ########## DEM ################
  ###############################
  
  # Effect of %SP by SP Ideology
  marginal_effect(Ct3_8c_me,"C_pctsp","splc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$splc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$splc[which(data$R==0)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Same-Party",
         subtitle = "By Same-Party Ideology",
         x = "Same-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-1.pdf")
  print("me5-1 is Saved") 
  
  
  # Effect of SP Ideology by %SP
  marginal_effect(Ct3_8c_me,"splc","C_pctsp")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pctsp[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pctsp[which(data$R==0)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Same-Party Ideology",
         subtitle = "By Percent Same-Party",
         x = "Percent Same-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-2.pdf")
  print("me5-2 is Saved")
  
  # Effect of %IND by IND Ideology
  marginal_effect(Ct3_8c_me,"C_pcti","ilc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$ilc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$ilc[which(data$R==0)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Independent",
         subtitle = "By Independent Ideology",
         x = "Independent Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-3.pdf")
  print("me5-3 is Saved")
  
  # Effect of IND Ideology by %IND
  marginal_effect(Ct3_8c_me,"ilc","C_pcti")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pcti[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pcti[which(data$R==0)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Independent Ideology",
         subtitle = "By Percent Independent",
         x = "Percent Independent",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-4.pdf")
  print("me5-4 is Saved")
  
  # Effect of %OP by OP Ideology
  marginal_effect(Ct3_8c_me,"C_pctop","oplc")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$oplc[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$oplc[which(data$R==0)],y=(..scaled../2),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Opposite-Party",
         subtitle = "By Opposite-Party Ideology",
         x = "Opposite-Party Ideology",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-5.pdf")
  print("me5-5 is Saved")
  
  # Effect of OP Ideology by $OP
  marginal_effect(Ct3_8c_me,"oplc","C_pctop")%>% 
    ggplot(aes(z, dy.dx)) +
    geom_line() +
    geom_line(aes(y = dy.dx - 1.96 * se), linetype = 2) +
    geom_line(aes(y = dy.dx + 1.96 * se), linetype = 2) +
    geom_hline(yintercept = 0) +
    #geom_vline(xintercept = 0,linetype="dashed")+
    geom_rug(data = data[which(data$R==0),], aes(x=data$C_pctop[which(data$R==0)], col = partyname),inherit.aes = FALSE)+ # This rug provides colors the other does not
    geom_density(data = data[which(data$R==0),], aes(x=data$C_pctop[which(data$R==0)],y=(..scaled../5),col=partyname,fill = partyname),alpha=.3,adjust=1,inherit.aes = FALSE)+
    scale_colour_manual(values=color[1])+
    scale_fill_manual(values=color[1])+
    labs(title = "Marginal Effect of Percent Opposite-Party Ideology",
         subtitle = "By Percent Opposite-Party",
         x = "Percent Opposite-Party",
         y = "Estimated marginal effect")+
    guides(col=FALSE) + 
    guides(fill=FALSE)
  ggsave("drafts/marginals/me5-6.pdf")
  print("me5-6 is Saved")
}