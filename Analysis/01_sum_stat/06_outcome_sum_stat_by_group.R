###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 6th, 2024
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

############################# Child Outcomes: Mean by Group/Round ###############################

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
