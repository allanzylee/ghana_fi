###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: 3/17/24
# Purpose: OLS Regressions with different sets of mechanisms

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
  m_category_str<-paste0("m_",category,"_per")
  e_category_str<-paste0("e_",category,"_per")
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  reg <- lm(fm,
            data=for_reg)
  # reg_robust <- coeftest(reg, vcovCL, cluster=full_data_w$careid)
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)

  return(reg_robust[,2])
}

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

####################################################################################################################
########################## Base OLS Model: Include Region and Treatment dummies ###############################
#########################################################################################################################

# Define base OLS input
base_ols_input<- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regression results
base_ols_results<- pmap(base_ols_input,
                                reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
base_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                             results_str='base_ols_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Errors
base_ols_robust_errors <- pmap(base_ols_robust_input,
                                       cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

############################## Exporting Results ###############################

stargazer(base_ols_results,
          title="Base OLS Regression wit Region and Treatment FE",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
          se=base_ols_robust_errors,
          # p=list(base_ols_robust_errors[['lit']][,4],base_ols_robust_errors[['num']][,4],base_ols_robust_errors[['ef']][,4],base_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/05_mech_controls_ols_reg/01_base_ols.html")

#######################################################################################################################
############################## Multivariate OLS Regression w/ Educational Investments Mechanisms ##############################
#######################################################################################################################

# Define base OLS input
ols_input_edu <- expand.grid(category=c('lit','num','ef','sel'),
                                          model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+e_enroll_ch+e_private_school+'))

# Regression results
ols_edu_results<- pmap(ols_input_edu,
                                    reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
ols_edu_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                 results_str='ols_edu_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
ols_edu_robust_errors <- pmap(ols_edu_results_robust_input,
                                           cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

# # Export tidy results
# reduced_multivar_ols_region_df <-pmap_dfr(reduced_multivar_ols_robust_region_input %>% dplyr::select(category),
#                                    tidy_func)

############################## Exporting Results ###############################

stargazer(ols_edu_results,
          title="Multivariate OLS Regression: Educational Investment Mechanism",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          se=ols_edu_robust_errors,
          # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/05_mech_controls_ols_reg/02_ols_edu.html")

