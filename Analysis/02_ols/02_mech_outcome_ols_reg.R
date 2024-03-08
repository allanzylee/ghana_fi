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
  
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)

  return(reg_robust[,2])
}

# Define function for creating tidy results
# tidy_func <- function(category, results_str){
#   results<-get(results_str)
#   out<-tidy(results[[category]]) %>% 
#     mutate(category=category)
#   return(out)
# }

##########################################################################################
####################################### Regress #######################################
##########################################################################################

####################################### Base #######################################

# Define list of relevant mechanisms
mechs<-c('ch_health','ch_health_rel',
         'enroll_ch','private_school',
         'hh_engagement',
         'ch_motiv','ch_esteem',
         'attend')

# Define reg_input
base_reg_input <- expand.grid(category=mechs,
                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regress (with derive cluster robust errors)
base_mech_results<- pmap(base_reg_input,
                         reg_func) %>% 
  set_names(mechs)

# Define base OLS Robust input
base_mech_cluster_input <- expand.grid(category=mechs,
                                       results_str='base_mech_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
base_mech_cluster_results <- pmap(base_mech_cluster_input,
                                  cluster_robust_func) %>%
  set_names(mechs)

stargazer(base_mech_results,
          column.labels = chartr("_"," ",mechs),
          se=base_mech_cluster_results,
          # p=list(base_ols_robust_errors_region_treatment[['lit']][,4],base_ols_robust_errors_region_treatment[['num']][,4],base_ols_robust_errors_region_treatment[['ef']][,4],base_ols_robust_errors_region_treatment[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/01_base_mech_results.html")

####################################### Multivariate #######################################

# Define reg_input
multi_reg_input<- expand.grid(category=mechs,
                                 model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regress (with derive cluster robust errors)
multi_mech_results<- pmap(multi_reg_input,
                          reg_func) %>% 
  set_names(mechs)

# Define base OLS Robust input
multi_mech_cluster_input <- expand.grid(category=mechs,
                                       results_str='multi_mech_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
multi_mech_cluster_results <- pmap(multi_mech_cluster_input,
                                  cluster_robust_func) %>%
  set_names(mechs)

stargazer(multi_mech_results, 
          column.labels = chartr("_"," ",mechs),
          star.cutoffs = c(.05, .01, NA),
          se=multi_mech_cluster_results,
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/02_multi_mech_results.html")



