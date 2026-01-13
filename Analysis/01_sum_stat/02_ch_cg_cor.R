###################################### Introduction ############################################

# Author: Allan Lee
# Date: March 16th, 2024
# Purpose: Calculate correlation between child- and caregiver-reported FI. Detect extreme patterns of FI.

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
cor.test(full_data_w$e_cfies_indicator,
    full_data_w$e_fies_indicator)

### Create a table and see the pattern of extreme FI
hh_fi_table <-full_data_w %>% 
  summarise(both_fi=sum(case_when(e_cfies_indicator==1 & e_fies_indicator==1~1,
                           T~0)),
         ch_only_fi=sum(case_when(e_cfies_indicator==1 & e_fies_indicator==0~1,
                            T~0)),
         cg_only_fi=sum(case_when(e_cfies_indicator==0 & e_fies_indicator==1~1,
                            T~0)),
         no_fi=sum(case_when(e_cfies_indicator==0 & e_fies_indicator==0~1,
                              T~0)),
         )


# Test within household child-level FI correlation

sib_pairs <- full_data_w %>%
  group_by(careid) %>%
  filter(n() == 2) %>%      
  arrange(careid) %>%      
  mutate(sib = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = careid,
    names_from = sib,
    values_from = e_cfies_indicator
  )

cor.test(sib_pairs$`1`, sib_pairs$`2`, use = "complete.obs")


