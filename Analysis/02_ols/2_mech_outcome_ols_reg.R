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
ols_func <- function(category, model){
  m_category_str<-paste0("m_",category)
  e_category_str<-paste0("e_",category)
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  reg <- lm(fm,
            data=for_reg)
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){
  
  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)
  
  return(reg_robust)
}

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

##########################################################################################
####################################### Enrollment #######################################
##########################################################################################

# Define base OLS input
base_ols_input_region_treatment <- expand.grid(category=c('ch_health','ch_health_rel',
                                                          'enroll_ch','private_school',
                                                          'hh_engagement',
                                                          'ch_motiv','ch_esteem',
                                                          'attend'),
                                               model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))


# Base OLS
base_enrollment_reg <- glm(enrolled_in_school ~ e_ch_fs_dummy+e_cg_fs_dummy,
                           data=full_data_w, family='binomial')

summary(base_enrollment_reg)

# Multivariate OLS
multivar_enrollment_reg <- glm(enrolled_in_school ~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language,
                               data=full_data_w, family='binomial')

summary(multivar_enrollment_reg)

####################################### Export #######################################
enrollment_regs <- list(base_enrollment_reg, multivar_enrollment_reg)

stargazer(enrollment_regs,
          title="Enrollment Regression",
          dep.var.caption = "Endline Dependent Variable:",
          omit=c('female','age','cg_age','cg_female','marital_status','cg_edu','poverty','num_kids','pe_pc1','pe_pc2','pe_pc3','pe_pc4','treatment','language'),
          column.labels = c("Base",'Multivariate'),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/1.6.24_enrollment_reg/enrolled_reg.html")


