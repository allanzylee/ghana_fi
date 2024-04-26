###################################### Introduction ############################################

# Author: Allan Lee
# Date: Apr 25rd, 2024
# Purpose: Calculate the average length of time between midline and endline interviews

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(psych)
library(haven)
library(writexl)
library(janitor)

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  rename(careid=caseid) %>% 
  filter(io2==1)
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid))

################################## Calculate difference in interview times ###############################
# Child
child_interview<-m_child %>% 
  select(childid,
         enddate) %>% 
  left_join(e_child %>% 
              select(childid,
                     enddate),
            by=c('childid'),
            suffix=c('_m',
                     '_e')) %>% 
  mutate(diff=enddate_e-enddate_m) %>% 
  filter(diff>=0)

# Caregiver
cg_interview<-m_cg %>% 
  select(childid,
         enddate) %>% 
  left_join(e_cg %>% 
              select(childid,
                     enddate),
            by=c('childid'),
            suffix=c('_m',
                     '_e')) %>% 
  mutate(diff=enddate_e-enddate_m) %>% 
  filter(diff>=0)

################################## Calculate average, min, and max interview time difference ###############################
min_child_interview_diff<-as.double(min(child_interview$diff))/30

max_child_interview_diff<-as.double(max(child_interview$diff))/30

min_cg_interview_diff<-as.double(min(cg_interview$diff))/30

max_cg_interview_diff<-as.double(max(cg_interview$diff))/30

mean_child_interview_diff<-as.double(mean(child_interview$diff))/30

mean_cg_interview_diff<-as.double(mean(cg_interview$diff))/30

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

out<-tribble(~mean_child,~mean_cg,~min_child,~min_cg,~max_child,~max_cg,
             mean_child_interview_diff,mean_cg_interview_diff,min_child_interview_diff,min_cg_interview_diff,max_child_interview_diff,max_cg_interview_diff)

write_xlsx(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/05_interview_length_diff.xlsx")





