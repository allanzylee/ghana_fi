###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: 3/2/2024
# Purpose: VA OLS Regressions with Mechanisms as Outcomes

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

library(tidyverse)
library(stargazer)
library(AER)
library(dataCompareR)
library(broom)
library(xtable)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

##########################################################################################
######################################## Regression Functions ############################
##########################################################################################

# Define base OLS functions
reg_func <- function(category, model){
  m_category_str<-paste0("m_",category)
  e_category_str<-paste0("e_",category)
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  
  if(category=='enroll_ch'|category=='private_school'){
    reg<-glm(fm,
        data=for_reg,
        family='binomial')
  }else{
    reg <- lm(fm,
              data=for_reg)
  }
  
  reg_robust <- coeftest(reg, vcovCL, cluster=full_data_w$careid)
  
  return(reg_robust)
}

# # Define function for standard errors
# cluster_robust_func <- function(category, results_str){
#   
#   results<-get(results_str)
#   reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)
#   
#   return(reg_robust)
# }

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

##########################################################################################
####################################### Regress #######################################
##########################################################################################

# Define list of relevant mechanisms
mechs<-c('ch_health','ch_health_rel',
         'enroll_ch','private_school',
         'hh_engagement',
         'ch_motiv','ch_esteem',
         'attend')

# Define reg_input
reg_input <- expand.grid(category=mechs,
                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regress (with derive cluster robust errors)
base_mech_results<- pmap(reg_input,
                         reg_func) %>% 
  set_names(mechs)

stargazer(base_mech_results,
          column.labels = chartr("_"," ",mechs),
          star.cutoffs = c(.05, .01, NA),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/01_base_mech_results.html")

####################################### Add effects of child age and sex #######################################

# Define reg_input
reg_input_age_sex <- expand.grid(category=mechs,
                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regress (with derive cluster robust errors)
base_mech_results_age_sex<- pmap(reg_input_age_sex,
                         reg_func) %>% 
  set_names(mechs)

stargazer(base_mech_results_age_sex,
          column.labels = chartr("_"," ",mechs),
          star.cutoffs = c(.05, .01, NA),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/02_base_mech_age_sex_results.html")



#
# 
# # Base OLS
# base_enrollment_reg <- glm(enrolled_in_school ~ e_ch_fs_dummy+e_cg_fs_dummy,
#                            data=full_data_w, family='binomial')
# 
# summary(base_enrollment_reg)
# 
# # Multivariate OLS
# multivar_enrollment_reg <- glm(enrolled_in_school ~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language,
#                                data=full_data_w, family='binomial')
# 
# summary(multivar_enrollment_reg)

# ####################################### Export #######################################
# enrollment_regs <- list(base_enrollment_reg, multivar_enrollment_reg)
# 
# stargazer(enrollment_regs,
#           title="Enrollment Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           omit=c('female','age','cg_age','cg_female','marital_status','cg_edu','poverty','num_kids','pe_pc1','pe_pc2','pe_pc3','pe_pc4','treatment','language'),
#           column.labels = c("Base",'Multivariate'),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/1.6.24_enrollment_reg/enrolled_reg.html")


