###################################### Introduction ############################################

# Author: Allan Lee
# Date: April 10th, 2024
# Purpose: Investigate Outcome Data Shared by SW and EA

# Methodology of calculating outcomes involve first creating factor vars, then removing interview effects, and then standardizing based on control

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
outcome <- read_dta("import/outcomes.dta") 

