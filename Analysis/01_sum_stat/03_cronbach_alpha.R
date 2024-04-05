###################################### Introduction ############################################

# Author: Allan Lee
# Date: Apr 3rd, 2024
# Purpose: Calculate Cronbach's Alpha for each of the questions related to outcome categories

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

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.)))

###################################### Create function to calculate Cronbach's Alpha #############################
get_alpha <- function(df){
  
  for_alpha<-get(df) %>% 
    mutate(across(everything(),
                  ~as.double(.)))
  out<-as.double(alpha(for_alpha)[['total']][['raw_alpha']]) %>% 
    as_tibble() %>% 
    mutate(test=df)
  return(out)
}

##########################################################################################
###################################### Outcome Data: Cronbach's Alpha #############################
##########################################################################################

############################################### Midline: SEL ######################################

m_sel <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(cr1:re11) %>%
  dplyr::select(-re5,-re8)

############################################### Midline Literacy ######################################

m_lit <- m_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) %>%
  dplyr::select(-oc5) 

############################################### Midline: Numeracy ######################################

m_num <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"))

############################################### Midline: EF ######################################

m_ef <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(starts_with("wm"),
                starts_with("sm"))

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(cr1:re11) %>%
  # Dedplyr::select friends question
  dplyr::select(-re5,-re8)

############################################### Endline Literacy ######################################

e_lit <- e_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) 

############################################### Endline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

e_num <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"))

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  dplyr::select(starts_with("wm"),
                starts_with("sm"))

############################################### Putting outcome data together ######################################
categories<-c('m_lit','m_num','m_lit','m_sel',
              'e_lit','e_num','e_lit','e_sel') %>% 
  as_tibble() %>% 
  rename('df'=value)

alpha<-pmap_dfr(categories,
           get_alpha)

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

write_xlsx(alpha, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/03_cronbach_alpha/cronbach_alpha.xlsx")





