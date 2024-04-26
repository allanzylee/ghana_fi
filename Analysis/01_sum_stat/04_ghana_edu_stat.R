###################################### Introduction ############################################

# Author: Allan Lee
# Date: Apr 25rd, 2024
# Purpose: Calculate the average education statistics for our regions of interest in Northern Ghana

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
ghana_edu <- read_csv("import/2020_ghana_factsheet.csv") %>% 
  clean_names() %>% 
  mutate(across(c(-region),~as.double(./100)))

################################## Define regions of interest ###############################
regions_of_interest <-c('Northern',
                        'Upper East',
                        'Upper West')

################################## Calculate average dropout rate ###############################
avg_drop_out_north<-ghana_edu %>% 
  filter(region %in% regions_of_interest) %>%
  summarise(across(contains('out_of'),~mean(.)))

################################## Calculate average percentage of children who did not achieve foundational skill ###############################
avg_found_north<-ghana_edu %>% 
  filter(region %in% regions_of_interest) %>%
  summarise(across(contains('perc'),~mean(.)))

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

out<-bind_cols(avg_drop_out_north,
               avg_found_north)

write_xlsx(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/04_ghana_edu_stat.xlsx")





