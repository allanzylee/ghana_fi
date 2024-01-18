###################################### Introduction ############################################

# Author: Allan Lee
# Date: September 16th, 2022
# Purpose: Analyze all data and denote relevant variables.

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Set working directory
setwd("/Users/AllanLee/Desktop/Penn/ECON4900/Data")

# Load packages
library(foreign)
library(haven)
library(tidyverse)
library(stargazer)
library(psych)
library(corrr)
library(tibble)
library(writexl)
library(timechange)
library(rnoaa)
library(base)
library(arsenal)
library(labelled)
library(zoo)

###################################### PNP Data Exploration #############################
# The purpose of this section is to analyze all relevant data and extract important variables

### Child specific data at BL

child_caregiver <- read_dta("child_caregiver.dta")
child_complete <- read_dta("child_complete.dta")
child_outcome <- read_dta("child_outcome.dta")
child_outcomes <- read_dta("child_outcomes.dta")
child_total <- read_dta("child_totals.dta")
child <- read_dta("child.dta")

### household/cg specific data at BL

enrol_cg <- read_dta("Enrolment & Caregiver Survey_depii.dta")
household <- read_dta("household.dta")

### Implementation 
cg_idi <- read_dta("implementation/Clean_Caregiver_IDI_nopii.dta")

### Midline
m_house<- read_dta("Midline/data/01_PNP_Midline_HouseholdSurvey.dta")
m_cg <- read_dta("Midline/data/02_PNP_Midline_CaregiverSurvey.dta")
m_child <- read_dta("Midline/data/03_PNP_Midline_ChildSurvey.dta")
m_cg_child <- read_dta("Midline/data/04_PNP_Midline_CaregiverChildSurvey.dta")
m_bl_cg_child_sms <- read_dta("Midline/data/05_PNP_BaseMid_CaregiverChildSurvey_smsrecords.dta")
m_bl_cg_child_survey <- read_dta("Midline/data/05_PNP_BaseMid_CaregiverChildSurvey.dta")

# Midline Constructed 
m_fs_merged <- read_dta("Midline/data/constructed/merged_midline_fs.dta")
m_fs <- read_dta("Midline/data/constructed/midline_fs.dta")

# Midline Archive
m_house_archive <- read_dta("Midline/data/archive/01_PNP_Midline_HouseholdSurvey.dta")
m_cg_archive <- read_dta("Midline/data/archive/02_PNP_Midline_CaregiverSurvey.dta")
m_child_archive <- read_dta("Midline/data/archive/03_PNP_Midline_ChildSurvey.dta")


# Midline Raw data
m_imp <- read_dta("Midline/data/Raw data/00_implementationdata.dta")
m_house_raw <- read_dta("Midline/data/Raw data/01_PNP_Midline_HouseholdSurvey.dta")
m_cg_raw <- read_dta("Midline/data/Raw data/02_PNP_Midline_CaregiverSurvey.dta")
m_child_raw <- read_dta("Midline/data/Raw data/03_PNP_Midline_ChildSurvey.dta")

# Endline
e_house<- read_dta("04 Endline Survey/01_PNP_Endline_HouseholdSurvey.dta")
e_cg <- read_dta("04 Endline Survey/02_PNP_Endline_CaregiverSurvey.dta")
e_child <- read_dta("04 Endline Survey/03_PNP_Endline_ChildSurvey.dta")
e_cg_child_survey <- read_dta("04 Endline Survey/04_PNP_Endline_CaregiverChildSurvey.dta")
all_cg_child_survey <- read_dta("04 Endline Survey/05_PNP_BaseMidEd_CaregiverChildSurvey.dta")
e_merged <- read_dta("04 Endline Survey/merged_dataset_end.dta")
samplechild <- read_dta("04 Endline Survey/samplechild.dta")


