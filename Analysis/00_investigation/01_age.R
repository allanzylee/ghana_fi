###################################### Introduction ############################################

# Author: Allan Lee
# Date: April 5th, 2024
# Purpose: Investigate which age variable to use

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
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)

e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)

e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid))

m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))

############## Age Investigation

# Endline 
e_age<-e_child %>% 
  mutate(under_10=case_when((is.na(sm6)&is.na(sm7)&is.na(re8)&is.na(re9)&is.na(re10)&is.na(re11)&is.na(wm7)&is.na(wm8)&is.na(wm9)&is.na(wm10))~1,T~0)) %>% 
  select(childid,childage,under_10) %>% 
  mutate(age_under_10=case_when(as.double(childage)<10~1,T~0)) %>% 
  left_join(e_cg %>% 
              select(childid, cr6),
            by=c('childid')) %>% 
  mutate(cg_age_under_10=case_when(as.double(cr6)<10~1,T~0))

# Midline
m_age<-m_child %>% 
  mutate(under_10=case_when((is.na(sm6)&is.na(sm7)&is.na(re8)&is.na(re9)&is.na(re10)&is.na(re11)&is.na(wm7)&is.na(wm8)&is.na(wm9)&is.na(wm10))~1,T~0)) %>% 
  select(childid,cr6,paa1,under_10) %>% 
  mutate(age_under_10=case_when(as.double(cr6)<10~1,T~0)) %>% 
  mutate(paa_age_under_10=case_when(as.double(paa1)<10~1,T~0)) %>% 
  left_join(e_age %>% 
              select(childid,e_age_under_10=age_under_10),
            by=c('childid'))
# Test if midline and endline questions are consistently asked
e_m_age<-e_age %>% 
  select(childid,under_10) %>% 
  left_join(m_age %>% 
              select(childid,under_10),
            by=c('childid')
            ) %>% 
  filter(under_10.x!=under_10.y)

