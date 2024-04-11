###################################### Introduction ############################################

# Author: Allan Lee
# Date: April 10th, 2024
# Purpose: Clean outcome data shared by SW and EA

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(haven)

# Load relevant data
outcome <- read_dta("import/outcomes_zscore.dta")

# Pivot the data wider and rename columns
out<-outcome %>% 
  filter(!is.na(round)) %>% 
  group_by(childid,careid) %>% 
  pivot_wider(names_from='round',
              values_from=c('zf_per_se',
                            'zf_per_lit',
                            'zf_per_num',
                            'zf_per_ef')) %>% 
  select(childid,
         careid,
         contains('2'),
         contains('3')) %>% 
  ungroup()

# Define outcome column names
outcome_cat <- c('sel',
                 'lit',
                 'num',
                 'ef'
                 )

# Midline
outcome_columns<- c('childid',
                    'careid',
                    paste("m_", outcome_cat,"_per", sep = ""),
                    paste("e_", outcome_cat,"_per", sep = ""))
  
names(out)<-outcome_columns

# Export
saveRDS(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_zscore.rds")
  
