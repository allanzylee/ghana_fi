###################################### Introduction ############################################

# Author: Allan Lee
# Date: March 16th, 2024
# Purpose: Calculate correlation between child- and caregiver-reported FI

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

### Calculate simple correlation between both reports
cor(full_data_w$e_ch_fs_dummy,
    full_data_w$e_cg_fs_dummy)

### Create a table and see the pattern of extreme FI
hh_fi_table <-full_data_w %>% 
  summarise(both_fi=sum(case_when(e_ch_fs_dummy==1 & e_cg_fs_dummy==1~1,
                           T~0)),
         ch_only_fi=sum(case_when(e_ch_fs_dummy==1 & e_cg_fs_dummy==0~1,
                            T~0)),
         cg_only_fi=sum(case_when(e_ch_fs_dummy==0 & e_cg_fs_dummy==1~1,
                            T~0)),
         no_fi=sum(case_when(e_ch_fs_dummy==0 & e_cg_fs_dummy==0~1,
                              T~0)),
         )




