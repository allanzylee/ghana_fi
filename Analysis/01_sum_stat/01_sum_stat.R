###################################### Introduction ############################################

# Author: Allan Lee
# Date: December 30th, 2023
# Purpose: Calculate relevant summary statistics

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environments
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(dplyr)
library(stargazer)
library(glue)
library(ltm)
library(xtable)
library(writexl)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

###################################### Load relevant functions ################################

# Define base OLS functions
reg_func <- function(category, model){
  m_category_str<-paste0("m_",category,"_per")
  e_category_str<-paste0("e_",category,"_per")
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  reg <- lm(fm,
            data=for_reg)
  return(reg)
}

#########################################################################################
######################################## Overall Summary Statistics ##############################
##########################################################################################

# High level summary of all relevant variables
summary_stat<-full_data_w %>% 
  dplyr::select(m_ch_fs_dummy,
                m_cg_fs_dummy,
                e_ch_fs_dummy,
                e_cg_fs_dummy,
                female,
                age_num,
                # enrolled_in_school,
                # private_school,
                cg_age,
                cg_female,
                cg_schooling,
                marital_status,
                # poverty,
                hh_size) %>% 
  as.data.frame()

# Export the summary statistic table
stargazer(summary_stat,
          header=FALSE, 
          type='latex',
          title = "Child Demographics Summary Statistics",
          covariate.labels=c("Midline Child-Reported FI",
                             "Midline Caregiver-Reported FI",
                             "Endline Child-Reported FI",
                             "Endline Caregiver-Reported FI",
                             "Child Sex",
                             "Child Age",
                             # "Enrolled in School", 
                             # "Attends Private School",
                             "Caregiver Age",
                             "Caregiver Sex",
                             "Caregiver Has Completed Primary Schooling",
                             "Caregiver Marital Status",
                             # "Poverty",
                             "Household Size")
          )

############################# Child and CG Reports of FI: Correlation ###############################

# Define function
ch_cg_cor_func <- function(var_group_str, str_1, str_0) {
  
  if (is.na(var_group_str)) {
    cor <- full_data_w %>%
      summarize(cor = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[4]),
                p_val = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[3])) %>%
      mutate(category = 'Overall')
  } else {
    var_group <- ensym(var_group_str)
    cor <- full_data_w %>% 
      group_by(category = !!var_group) %>% 
      summarize(cor = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[4]),
                p_val = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[3])) %>% 
      mutate(category = case_when(category == 1 ~ str_1,
                                  TRUE ~ str_0),
             .before = 1)
  }
  return(cor)
}

# Define Input
ch_cg_cor_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  NA,            "",     "",
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  
)  

# Manually calculate correlation by age/female
ch_cg_fi_cor_age_female <- full_data_w %>% 
  group_by(age, female) %>% 
  summarize(cor = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[4]),
            p_val = as.double(cor.test(e_ch_fs_dummy, e_cg_fs_dummy)[3])) %>% 
  mutate(category=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
ch_cg_fi_cor<- pmap_dfr(ch_cg_cor_input,
                ch_cg_cor_func) %>% 
  bind_rows(ch_cg_fi_cor_age_female) %>% 
  rename(ch_cg_reports_cor=cor)

############################# Child Reports of FI: Mean by Group/Round ###############################

# Define function
ch_sum_stat_func <- function(var_group_str, str_1, str_0) {

 var_group <- ensym(var_group_str)
 sum_stat <- full_data_w %>% 
   dplyr::select(!!var_group,
                 contains('fs')) %>% 
    group_by(group=!!var_group) %>% 
    summarize(across(matches('ch_fs_dummy$'),
                     ~mean(., na.rm=T),
                     .names = "mean.{col}")
    ) %>%
    mutate(group=case_when(group==1 ~ str_1,
                           T~str_0))
  return(sum_stat)
}

# Define Input
ch_sum_stat_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  
)

# Overall
ch_fi_sum_stat_overall <- full_data_w %>% 
  dplyr::select(contains('fs')) %>% 
  summarize(across(matches('ch_fs_dummy$'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}")
  ) %>% 
  mutate(group='Overall')

# By Gender and Age
ch_fi_sum_stat_female_age <- full_data_w %>% 
  dplyr::select(contains('fs'),
                female,
                age) %>% 
  group_by(female, age) %>% 
  summarize(across(matches('ch_fs_dummy$'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}")
  ) %>%
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
ch_fi_sum_stat <- ch_fi_sum_stat_overall %>% 
  bind_rows(pmap_dfr(ch_sum_stat_input,ch_sum_stat_func),
            ch_fi_sum_stat_female_age) %>% 
  rename(e_fs=mean.e_ch_fs_dummy,
         m_fs=mean.m_ch_fs_dummy) %>% 
  dplyr::select(group,e_fs,m_fs)
  
xtable(ch_fi_sum_stat)

############################# Midline and Endline Child and CG Reports of FI: Correlation ###############################

# Define function
m_e_ch_cg_corr_func <- function(var_group_str, str_1, str_0) {
  
  if (is.na(var_group_str)) {
    m_e_cor <- full_data_w %>% 
      summarize(ch_cor=as.double(cor.test(e_ch_fs_dummy,
                                          m_ch_fs_dummy)[4]),
                ch_cor_pval=as.double(cor.test(e_ch_fs_dummy,
                                               m_ch_fs_dummy)[3]),
                cg_cor=as.double(cor.test(e_cg_fs_dummy,
                                          m_cg_fs_dummy)[4]),
                cg_cor_pval=as.double(cor.test(e_cg_fs_dummy,
                                               m_cg_fs_dummy)[3])
                
      ) 
  } else {
    var_group <- ensym(var_group_str)
    m_e_cor <- full_data_w %>% 
      filter(!is.na(!!var_group)) %>% 
      group_by(!!var_group) %>% 
      summarize(ch_cor=as.double(cor.test(e_ch_fs_dummy,
                                m_ch_fs_dummy)[4]),
                ch_cor_pval=as.double(cor.test(e_ch_fs_dummy,
                                     m_ch_fs_dummy)[3]),
                cg_cor=as.double(cor.test(e_cg_fs_dummy,
                                m_cg_fs_dummy)[4]),
                cg_cor_pval=as.double(cor.test(e_cg_fs_dummy,
                                     m_cg_fs_dummy)[3])
                
      ) %>% 
      mutate(group=case_when(!!var_group==1 ~ str_1,
                             T~str_0))
    return(m_e_cor)
  }
  
}

# Define Input
m_e_ch_cg_corr_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  NA, '', '',
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9'
  
) 

# By Female and Age
m_e_ch_cg_fi_cor_female_age <- full_data_w %>% 
  group_by(age, female) %>% 
  summarize(ch_cor=as.double(cor.test(e_ch_fs_dummy,
                                      m_ch_fs_dummy)[4]),
            ch_cor_pval=as.double(cor.test(e_ch_fs_dummy,
                                           m_ch_fs_dummy)[3]),
            cg_cor=as.double(cor.test(e_cg_fs_dummy,
                                      m_cg_fs_dummy)[4]),
            cg_cor_pval=as.double(cor.test(e_cg_fs_dummy,
                                           m_cg_fs_dummy)[3])
            
  ) %>% 
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                         age==1 & female==0 ~ 'Male (10-17)',
                         age==0 & female==1 ~ 'Female (5-9)',
                         T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
m_e_ch_cg_fi_cor <-pmap_dfr(m_e_ch_cg_corr_input,
                            m_e_ch_cg_corr_func) %>% 
  bind_rows(m_e_ch_cg_fi_cor_female_age
            #,m_e_ch_cg_fi_cor_current_class
            ) %>% 
  dplyr::select(-female,
                -age)

############################# Midline and Endline Child Outcomes: Correlation ###############################

# Define function
m_e_ch_outcome_cor_func <- function(var_group_str) {
  midline<-paste0('m_',var_group_str,'_per')
  endline<-paste0('e_',var_group_str,'_per')
  m_var_group <- ensym(midline)
  e_var_group <- ensym(endline)
  
  m_e_cor <- full_data_w %>% 
    mutate(across(everything(),~as.numeric(.))) %>% 
    filter(!is.na(!!m_var_group),
           !is.na(!!e_var_group)) %>% 
    summarize(cor=as.double(cor.test(!!m_var_group,
                      !!e_var_group)[4] ),
              cor_pval=as.double(cor.test(!!m_var_group,
                                     !!e_var_group)[3]))%>% 
    mutate(cat=var_group_str)
  # %>% 
  #   pivot_longer(-group, 
  #                names_to = c("category", ".value"), 
  #                names_sep="_" ) %>% 
  #   mutate(group=case_when(group==1 ~ str_1,
  #                          T~str_0))
  return(m_e_cor)
  
}

# Define Input
m_e_ch_outcome_cor_input <- tribble(
  ~var_group_str,
  "lit",
  "num",
  "ef",
  "sel"
  
) 
m_e_ch_outcome_cor_output <-pmap_dfr(m_e_ch_outcome_cor_input,
                                     m_e_ch_outcome_cor_func)

########################################## Export ########################################################
# Make list of summary statistic results
sum_stat_export <- list('child_cg_reports_cor' = ch_cg_fi_cor, 
                      'child_fi_reports_sum_stat' = ch_fi_sum_stat,
                      'm_e_fi_cor'=m_e_ch_cg_fi_cor)

# Export
write_xlsx(sum_stat_export, '/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/01_sum_stat.xlsx')

##################################################################################################################################
############################## Test the Statistical Significance of Child-Reported Food Insecurity ###############################
##################################################################################################################################

# Define successive input
successive_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                    model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+cg_age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+cg_female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+marital_status+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+cg_schooling +region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+hh_size+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            # '~ e_ch_fs_dummy+e_cg_fs_dummy+pe_pc1+',
                                            # '~ e_ch_fs_dummy+e_cg_fs_dummy+pe_pc2+',
                                            # '~ e_ch_fs_dummy+e_cg_fs_dummy+pe_pc3+',
                                            # '~ e_ch_fs_dummy+e_cg_fs_dummy+pe_pc4+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+',
                                            '~ e_ch_fs_dummy+e_cg_fs_dummy+language+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regression results
successive_ols_results<- pmap(successive_ols_input,
                              reg_func)

# Extract child_food_insecurity and note the instances when it is not all significant
ch_fi_sig_func <- function(num,model,category){
  out<-broom::tidy(successive_ols_results[[num]]) %>% 
    janitor::clean_names() %>% 
    mutate(cov=model,
           outcome=category) %>% 
    filter(term=='e_ch_fs_dummy') %>% 
    mutate(sig_flag=case_when(p_value<=0.05~1,
                              T~0))
  return(out)
}

# Regression results
successive_ch_fi_sig<- pmap_dfr(successive_ols_input %>% mutate(num=seq(1,40,1)),
                                ch_fi_sig_func) %>% 
  dplyr::select(cov,outcome,sig_flag) %>% 
  pivot_wider(names_from = outcome,
              values_from = sig_flag) %>% 
  mutate(cov=case_when(cov=='~ e_ch_fs_dummy+e_cg_fs_dummy+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'~"Baseline Mode",
                       T~gsub(".*~ e_ch_fs_dummy\\+e_cg_fs_dummy\\+(.*?)\\+region_north_east\\+region_northern\\+region_upper_east\\+region_upper_west\\+treatment\\+.*", "\\1", cov)),
         across(-cov,~case_when(.==1~"X",
                                T~"")))

# Export
write.csv(successive_ch_fi_sig,
          '/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/01b_stat_sig.csv')

# Create Latex Table
xtable(successive_ch_fi_sig, type = "latex")


