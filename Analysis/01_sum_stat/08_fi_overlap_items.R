###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 5th, 2025
# Purpose: Calculate the percentage of FI for overlapping items

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
  
  
stat<-full_data_w %>% 
  dplyr::select(childid,
         careid,
         matches("^fs\\d+")) %>% 
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>% 
  dplyr::select(childid,
                careid,
                # Worry about lack of food
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                # Size of meal cut
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                # Skipped meal
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                # Hungry but didn't eat
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
                ) %>% 
  pivot_longer(cols=-c(childid,
                       careid),
               names_to = c(".value", "category"), 
               names_sep ="_") %>% 
  group_by(category) %>% 
  summarise(across(c("worry",
                     'cut',
                     'skip',
                     'hungry'),
                   ~mean(.,
                         na.rm=T)))




