# Author:       David Simpson
# Title:        
# Class:        
# Created:      18 November 2018
# Edited:       18 November 2018
# Adapted From:  https://cfss.uchicago.edu/persp013_interaction_terms.html#objectives # by: Benjamin Soltoff

# Source URL:   
# Citation:     
# Input Files:  (1)
#               (2)
# Output Files: (1) 
#               (2)

#rm(list=ls())

# Interaction Effect
marginal_effect <- function(model, eff_var, mod_var){
  choice <- c(model$coefficients)[which(str_detect(names(model$coefficients), ":"))] # Identify possible interaction terms
  option <- c(paste(eff_var,mod_var,sep=":"),paste(mod_var,eff_var,sep=":")) # Identify possible ordering of your terms of interest
  
  # Loop to identify correct interaction term
  for (i in 1:length(choice)){
    for(j in 1:length(option)){
      if(option[j]==names(choice)[[i]]){
        interaction <- names(choice)[[i]]
      }
    }
  }
  print(paste("The interaction term is:",interaction, sep = " "))
  # store coefficients and covariance matrix
  beta.hat <- coef(model)
  covmat <- vcov(model)
  
  # possible set of values for mod_var
  if(class(model)[[1]] == "lm"){
    z <- seq(min(model$model[[mod_var]]), max(model$model[[mod_var]]),by=.001)
  } else {
    z <- seq(min(model$data[[mod_var]]), max(model$data[[mod_var]]))
  }
  
  # calculate instantaneous effect
  dy.dx <- beta.hat[[eff_var]] + beta.hat[[interaction]] * z
  
  # calculate standard errors for instantaeous effect
  se.dy.dx <- sqrt(covmat[eff_var, eff_var] +
                     z^2 * covmat[interaction, interaction] +
                     2 * z * covmat[eff_var, interaction])
  
  # combine into data frame
  data_frame(z = z,
             dy.dx = dy.dx,
             se = se.dy.dx)
}


######
# Original Function
#####

# function to get point estimates and standard errors
# model - lm object
# mod_var - name of moderating variable in the interaction
#instant_effect <- function(model, mod_var){
  # get interaction term name
#  int.name <- names(model$coefficients)[[which(str_detect(names(model$coefficients), ":"))]]
  
#  marg_var <- str_split(int.name, ":")[[1]][[which(str_split(int.name, ":")[[1]] != mod_var)]]
  
  # store coefficients and covariance matrix
#  beta.hat <- coef(model)
#  cov <- vcov(model)
  
  # possible set of values for mod_var
#  if(class(model)[[1]] == "lm"){
#    z <- seq(min(-0), max(model$model[[mod_var]]))
#  } else {
#    z <- seq(min(model$data[[mod_var]]), max(model$data[[mod_var]]))
#  }
  
  # calculate instantaneous effect
#  dy.dx <- beta.hat[[marg_var]] + beta.hat[[int.name]] * z
  
  # calculate standard errors for instantaeous effect
#  se.dy.dx <- sqrt(cov[marg_var, marg_var] +
#                     z^2 * cov[int.name, int.name] +
#                     2 * z * cov[marg_var, int.name])
  
  # combine into data frame
#  data_frame(z = z,
#             dy.dx = dy.dx,
#             se = se.dy.dx)
#}




