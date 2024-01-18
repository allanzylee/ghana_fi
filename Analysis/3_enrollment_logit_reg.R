###################################### Introduction ############################################

# Author: Allan Lee
# Date: January 3rd, 2024
# Purpose: Logit Regressions on Enrollment

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(stargazer)
library(AER)
library(dataCompareR)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

##########################################################################################
####################################### Enrollment #######################################
##########################################################################################

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


