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
reg_func <- function(category, model, only_enrolled=F){
  m_category_str<-paste0("m_",category)
  e_category_str<-paste0("e_",category)
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  
  if(category=='private_school'){
    
    if(only_enrolled==T){
    
      for_reg_only_enrolled<-for_reg %>% 
      filter(e_enroll_ch==1)
    } else{
      for_reg_only_enrolled<-for_reg
    }
    
    reg<-glm(fm,
        data=for_reg_only_enrolled,
        family='binomial')
  }else if(category=='enroll_ch'){
    reg<-glm(fm,
             data=for_reg,
             family='binomial')
    
  } else {
    reg <- lm(fm,
              data=for_reg)
  }
  
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str, only_enrolled=F){

  results<-get(results_str)
  if(category=='private_school'){
    
    if(only_enrolled==T){
      
      for_robust<-full_data_w %>% 
        filter(e_enroll_ch==1)
    } else{
      for_robust<-full_data_w
    }
    
    reg_robust <- coeftest(results[[category]], vcovCL, cluster=for_robust$careid)
  } else{
    reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)
  }

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

# Define flag for whether to only have children who are enrolled in school for private school regression
only_enrolled_flag<-T
private_school_label <-case_when(only_enrolled_flag==T~"Child is Enrolled in Private School (Subset)",
                                 T~"Child is Enrolled in Private School") 

# Define list of relevant mechanisms
mechs<-c('ch_health','ch_health_rel',
         'enroll_ch','private_school',
         'hh_engagement',
         'ch_motiv','ch_esteem',
         'attend')

# Define reg_input
base_reg_input <- expand.grid(category=mechs,
                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'),
                         only_enrolled=only_enrolled_flag)

# Regress (with derive cluster robust errors)
base_mech_results<- pmap(base_reg_input,
                         reg_func) %>% 
  set_names(mechs)

# Define base OLS Robust input
base_mech_cluster_input <- expand.grid(category=mechs,
                                       results_str='base_mech_results',
                                       only_enrolled=only_enrolled_flag) %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
base_mech_cluster_results <- pmap(base_mech_cluster_input,
                                  cluster_robust_func) %>%
  set_names(mechs)

# Define Stargazer labels
mechs_label<-c('Child-Reported Health','Child-Reported Relative Health',
               'Child is Enrolled in School',private_school_label,
               'Household Engagement',
               'Child Motivation','Child Self-Esteem',
               'Child Attended School')

cov_labels <-c("Child-Reported FI",
               "Caregiver-Reported FI",
               "Child Female",
               "Child is 10–17",
               "Region: North East",
               "Region: Northern",
               "Region: Upper East",
               "Region: Upper West",
               "PNP Treatment",
               "Lagged Outcome",
               "Constant")

stargazer(base_mech_results,
          column.labels = chartr("_"," ",mechs_label),
          se=base_mech_cluster_results,
          covariate.labels=cov_labels,
          # p=list(base_ols_robust_errors_region_treatment[['lit']][,4],base_ols_robust_errors_region_treatment[['num']][,4],base_ols_robust_errors_region_treatment[['ef']][,4],base_ols_robust_errors_region_treatment[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/01_base_mech_results.html")

####################################### Multivariate #######################################

# Define reg_input
multi_reg_input<- expand.grid(category=mechs,
                                 model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'),
                              only_enrolled=only_enrolled_flag)

# Regress (with derive cluster robust errors)
multi_mech_results<- pmap(multi_reg_input,
                          reg_func) %>% 
  set_names(mechs)

# Define base OLS Robust input
multi_mech_cluster_input <- expand.grid(category=mechs,
                                       results_str='multi_mech_results',
                                       only_enrolled=only_enrolled_flag) %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
multi_mech_cluster_results <- pmap(multi_mech_cluster_input,
                                  cluster_robust_func) %>%
  set_names(mechs)

# Define Stargazer labels
multi_cov_labels <-c("Child-Reported FI",
               "Caregiver-Reported FI",
               "Child Female",
               "Child is 10–17",
               "Caregiver Age",
               "Caregiver Female",
               "Caregiver has a Partner",
               "Caregiver Completed Primary School",
               "Household Size",
               "Language: Dagbani",
               "Language: Gruni",
               "Language: Other",
               "Language: Sissali",
               "Region: North East",
               "Region: Northern",
               "Region: Upper East",
               "Region: Upper West",
               "PNP Treatment",
               "Lagged Outcome",
               "Constant")

stargazer(multi_mech_results, 
          column.labels = chartr("_"," ",mechs_label),
          star.cutoffs = c(.05, .01, NA),
          se=multi_mech_cluster_results,
          covariate.labels=multi_cov_labels,
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/02_mech_outcome_ols_reg/02_multi_mech_results.html")



