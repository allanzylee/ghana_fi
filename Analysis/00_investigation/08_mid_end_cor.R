###################################### Introduction ############################################

# Author: Allan Lee
# Date: July 20, 2025
# Purpose: Calculate midline vs endline FI correlation

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Load header
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

# Mid vs End: child
cor.test(full_data_w$e_ch_fs_dummy,full_data_w$m_ch_fs_dummy)

# Mid vs End: caregiver
cor.test(full_data_w$e_cg_fs_dummy,full_data_w$m_cg_fs_dummy)

# Mid vs End: outcomes
cor.test(full_data_w$e_lit_per,full_data_w$m_lit_per)
cor.test(full_data_w$e_num_per,full_data_w$m_num_per)
cor.test(full_data_w$e_ef_per,full_data_w$m_ef_per)
cor.test(full_data_w$e_sel_per,full_data_w$m_sel_per)


