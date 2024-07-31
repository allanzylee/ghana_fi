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
  out<-as.double(alpha(for_alpha,check.keys=T)[['total']][['raw_alpha']]) %>% 
    as_tibble() %>% 
    mutate(test=df)
  return(out)
}

##########################################################################################
###################################### Outcome Data: Cronbach's Alpha #############################
##########################################################################################

############################################### Midline: SEL ######################################
# 
# m_sel <- m_child %>% 
#   # dplyr::select relevant numeracy questions
#   dplyr::select(cr1:re11) %>%
#   dplyr::select(-re5,-re8)
# 
# ############################################### Midline Literacy ######################################
# 
# m_lit <- m_child %>% 
#   # dplyr::select relevant literacy questions
#   dplyr::select(starts_with("nr"),
#                 starts_with("sp"),
#                 starts_with("or"),
#                 starts_with("oc"),
#                 matches("pa[0-9]")) %>%
#   dplyr::select(-oc5) 
# 
# ############################################### Midline: Numeracy ######################################
# 
# m_num <- m_child %>% 
#   # dplyr::select relevant numeracy questions
#   dplyr::select(matches("co[0-9]"),
#                 starts_with("nd"),
#                 starts_with("mn"),
#                 starts_with("nu"),
#                 starts_with("wp"),
#                 starts_with("ad"),
#                 matches("su[0-9]"),
#                 starts_with("mu"),
#                 matches("di[0-9]"))
# 
# ############################################### Midline: EF ######################################
# 
# m_ef <- m_child %>% 
#   # dplyr::select relevant numeracy questions
#   dplyr::select(starts_with("wm"),
#                 starts_with("sm"))

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                cr1:re11,
                child_age=childage) %>%
  # Dedplyr::select friends question
  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Change CR SEL questions to be 1/0 binary
  mutate(across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>% 
  summarise(cr_perc=rowSums(across(contains('cr')),na.rm=T)/length(colnames(e_child %>% select(contains('cr')))),
         re_perc=case_when(child_age<10~rowSums(across(matches("re[0-9]")),na.rm=T)/6,
                           T~rowSums(across(matches("re[0-9]")),na.rm=T)/9))

cor(e_sel$cr_perc,
    e_sel$re_perc)

############################################### Endline Literacy ######################################

e_lit <- e_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
                childid,
                # starts_with("ov"),
                starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]"),
                child_age=childage) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  summarise(nr_perc=rowSums(across(starts_with("nr")),na.rm=T)/length(colnames(e_child %>% select(starts_with("nr")))),
            sp_perc=rowSums(across(starts_with("sp")),na.rm=T)/length(colnames(e_child %>% select(starts_with("sp")))),
            or_perc=rowSums(across(starts_with("or")),na.rm=T)/length(colnames(e_child %>% select(starts_with("or")))),
            oc_perc=rowSums(across(starts_with("oc")),na.rm=T)/length(colnames(e_child %>% select(starts_with("oc")))),
            pa_perc=rowSums(across(matches("pa[0-9]")),na.rm=T)/length(colnames(e_child %>% select(matches("pa[0-9]")))))

############################################### Endline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

e_num <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"),
                child_age=childage) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  summarise(co_perc=rowSums(across(matches("co[0-9]")),na.rm=T)/length(colnames(e_child %>% select(matches("co[0-9]")))),
            nd_perc=rowSums(across(starts_with("nd")),na.rm=T)/length(colnames(e_child %>% select(starts_with("nd")))),
            mn_perc=rowSums(across(starts_with("mn")),na.rm=T)/length(colnames(e_child %>% select(starts_with("mn")))),
            nu_perc=rowSums(across(starts_with("nu")),na.rm=T)/length(colnames(e_child %>% select(starts_with("nu")))),
            wp_perc=rowSums(across(starts_with("wp")),na.rm=T)/length(colnames(e_child %>% select(starts_with("wp")))),
            ad_perc=rowSums(across(starts_with("ad")),na.rm=T)/length(colnames(e_child %>% select(starts_with("ad")))),
            su_perc=rowSums(across(matches("su[0-9]")),na.rm=T)/length(colnames(e_child %>% select(matches("su[0-9]")))),
            mu_perc=rowSums(across(starts_with("mu")),na.rm=T)/length(colnames(e_child %>% select(starts_with("mu")))),
            di_perc=rowSums(across(matches("di[0-9]")),na.rm=T)/length(colnames(e_child %>% select(matches("di[0-9]")))))

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  dplyr::select(childid,
                starts_with("wm"),
                starts_with("sm"),
                child_age=childage) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  summarise(wm_perc=rowSums(across(starts_with('wm')),na.rm=T)/length(colnames(e_child %>% select(starts_with('wm')))),
            sm_perc=case_when(child_age<10~rowSums(across(starts_with("sm")),na.rm=T)/5,
                              T~rowSums(across(starts_with("sm")),na.rm=T)/7)
            )

cor(e_ef$wm_perc,
    e_ef$sm_perc)

############################################### Putting outcome data together ######################################
categories<-c('e_lit','e_num','e_sel','e_ef') %>% 
  as_tibble() %>% 
  rename('df'=value)

alpha<-pmap_dfr(categories,
           get_alpha)

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

write_xlsx(alpha, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/03_cronbach_alpha/cronbach_alpha.xlsx")





