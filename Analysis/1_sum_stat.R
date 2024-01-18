###################################### Introduction ############################################

# Author: Allan Lee
# Date: December 30th, 2023
# Purpose: Calculate relevant summary statistics

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

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')
outcomes <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome.rds') %>% 
  select(childid,careid,contains('_per'))

#########################################################################################
######################################## Overall Summary Statistics ##############################
##########################################################################################

# High level summary of all relevant variables
summary_stat<-full_data_w %>% 
  dplyr::select(e_ch_fs_dummy, e_cg_fs_dummy, female,age,enrolled_in_school,private_school,num_books,cg_age,cg_female,cg_edu,marital_status,poverty,num_kids, treatment) %>% 
  as.data.frame()

# Export the summary statistic table
stargazer(summary_stat,
          header=FALSE, 
          type='latex',
          title = "Child Demographics Summary Statistics",
          covariate.labels=c("Child Food Insecurity","Caregiver Food Insecurity","Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School","Number of Books Child Owns",
                             "Caregiver Age","Caregiver Female","Caregiver Education","Caregiver Marital Status","Poverty","Household Size","Participated in PNP Treatment Group")
          )

############################# Language Summary Statistics ###############################
lan_break_down<-as.data.frame(table(full_data_w$language)/nrow(full_data_w)) %>% 
  clean_names() %>% 
  rename(language=var1,frequency=freq)
# write_xlsx(lan_break_down, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/lan_break_down.xlsx")

############################# "Extreme" Weather Deviance Summary Statistics ###############################
extreme_weather_deviation <- full_data_w %>%
  dplyr::select(contains('extreme')) %>% 
  summarise(across(starts_with("extreme"), ~mean(.,na.rm=T))) %>% 
  rename("Optimal Precipitation Deviance Indicator"=extreme_o_p_dev,
         "Optimal Temperature Deviance Indicator"=extreme_o_t_dev,
         "Mean Precipitation Deviance Indicator"=extreme_m_p_dev,
         "Mean Temperature Deviance Indicator"=extreme_m_t_dev,
         "Regional Precipitation Deviance Indicator"=extreme_s_p_dev,
         "Regional Temperature Deviance Indicator"=extreme_s_t_dev) %>% 
  pivot_longer(cols=c(everything()),
               names_to="Variable",
               values_to="Larger than One SD") %>% 
  mutate(`Larger than One SD`=round(`Larger than One SD`,digits=2),
         `Smaller than One SD`=1-`Larger than One SD`)
  
stargazer(extreme_weather_deviation,
          summary=FALSE,
          rownames=FALSE)

############################# Gender Bias Summary Statistics ###############################
gender_bias <- full_data_w %>% 
  dplyr::select(childid,careid,contains("gb")) %>% 
  dplyr::mutate(across(contains('gb'),~case_when(.==1~0,
                                               T~1))) %>% 
  dplyr::select(-childid,-careid) %>% 
  rename_with(toupper) %>% 
  as.data.frame()

stargazer(gender_bias)

############################# Child and CG Reports of FI: Correlation ###############################

# Define function
ch_cg_cor_func <- function(var_group_str, str_1, str_0) {
  
  if (is.na(var_group_str)) {
    cor <- full_data_w %>%
      summarize(cor = cor(e_ch_fs_dummy, e_cg_fs_dummy, use = "everything")) %>%
      mutate(category = 'Overall')
  } else {
    var_group <- ensym(var_group_str)
    cor <- full_data_w %>% 
      group_by(category = !!var_group) %>% 
      summarize(cor = cor(e_ch_fs_dummy, e_cg_fs_dummy, use = "everything")) %>% 
      mutate(category = case_when(category == 1 ~ str_1,
                                  TRUE ~ str_0),
             .before = 1)
  }
  return(cor)
}

# Define Input
ch_cg_cor_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  NA,            "",     "",
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  
)  

# Manually calculate correlation by age/female
ch_cg_fi_cor_age_female <- full_data_w %>% 
  group_by(age, female) %>% 
  summarize(cor=cor(e_ch_fs_dummy, e_cg_fs_dummy, use = "everything")) %>% 
  mutate(category=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
ch_cg_fi_cor<- pmap_dfr(ch_cg_cor_input,
                ch_cg_cor_func) %>% 
  bind_rows(ch_cg_fi_cor_age_female) %>% 
  rename(ch_cg_reports_cor=cor)

############################# Child and CG Reports of FI: Mean and SD ###############################

# Define function
ch_cg_sum_stat_func <- function(var_group_str, str_1, str_0) {

 var_group <- ensym(var_group_str)
 sum_stat <- full_data_w %>% 
    group_by(group=!!var_group) %>% 
    summarize(across(matches('^e_.*dummy$'),
                     ~mean(., na.rm=T),
                     .names = "mean.{col}"),
              across(matches('^e_.*dummy$'),
                     ~sd(., na.rm=T),
                     .names = "sd.{col}")
    ) %>% 
    pivot_longer(-group, 
                 names_to = c("category", ".value"), 
                 names_sep="_" ) %>% 
    mutate(group=case_when(group==1 ~ str_1,
                           T~str_0))
  return(sum_stat)
}

# Define Input
ch_cg_sum_stat_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  
)  

# By round
ch_cg_fi_sum_stat_overall <- full_data_w %>% 
  summarize(across(contains('_dummy'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}"),
            across(contains('_dummy'),
                   ~sd(., na.rm=T),
                   .names = "sd.{col}")
            ) %>% 
  dplyr::select(-contains('sd.mean')) %>% 
  pivot_longer(everything(), 
               names_to = c("category", ".value"), 
               names_sep="_" ) %>% 
  mutate(group=case_when(str_sub(category,start=-1)=='m'~ "Midline Reports",
                         T~'Endline Reports'))

# Endline by Gender and Age
e_ch_cg_fi_sum_stat_female_age <- full_data_w %>% 
  group_by(female, age) %>% 
  summarize(across(matches('^e_.*dummy$'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}"),
            across(matches('^e_.*dummy$'),
                   ~sd(., na.rm=T),
                   .names = "sd.{col}")
  ) %>% 
  pivot_longer(-c(female,age), 
               names_to = c("category", ".value"), 
               names_sep="_" ) %>% 
  # pivot_wider(names_from = category,
  #             values_from=c(ch,cg)) %>% 
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
ch_cg_fi_sum_stat <- pmap_dfr(ch_cg_sum_stat_input,
                              ch_cg_sum_stat_func) %>% 
  bind_rows(ch_cg_fi_sum_stat_overall,
            e_ch_cg_fi_sum_stat_female_age) %>% 
  rename(ch_reports=ch,
         cg_reports=cg) %>% 
  mutate(category=sub(".[^.]*$", "",category)) %>% 
  pivot_wider(names_from=category,
              values_from=c(ch_reports,cg_reports))

############################# Midline and Endline Child and CG Reports of FI: Correlation ###############################

# Define function
m_e_ch_cg_corr_func <- function(var_group_str, str_1, str_0) {
  
  if (is.na(var_group_str)) {
    m_e_cor <- full_data_w %>% 
      summarize(ch_cor=cor(e_ch_fs_dummy,
                           m_ch_fs_dummy),
                cg_cor=cor(e_cg_fs_dummy,
                           m_cg_fs_dummy),
                
      ) %>% 
      pivot_longer(everything(), 
                   names_to = c("category", ".value"), 
                   names_sep="_" ) %>% 
      mutate(group='Overall')
  } else {
  var_group <- ensym(var_group_str)
  m_e_cor <- full_data_w %>% 
    filter(!is.na(!!var_group)) %>% 
    group_by(group=!!var_group) %>% 
    summarize(ch_cor=cor(e_ch_fs_dummy,
                         m_ch_fs_dummy),
              cg_cor=cor(e_cg_fs_dummy,
                         m_cg_fs_dummy),
              
    ) %>% 
    pivot_longer(-group, 
                 names_to = c("category", ".value"), 
                 names_sep="_" ) %>% 
    mutate(group=case_when(group==1 ~ str_1,
                           T~str_0))
  return(m_e_cor)
  }
  
}

# Define Input
m_e_ch_cg_corr_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  NA, '', '',
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  'private_school', 'Child attends private school', 'Child does not attend private school',
  
) 

# By Female and Age
m_e_ch_cg_fi_cor_female_age <- full_data_w %>% 
  group_by(age, female) %>% 
  summarize(ch_cor=cor(e_ch_fs_dummy,
                       m_ch_fs_dummy),
            cg_cor=cor(e_cg_fs_dummy,
                       m_cg_fs_dummy),
            
  ) %>% 
  pivot_longer(-c(age,female), 
               names_to = c("category", ".value"), 
               names_sep="_" ) %>% 
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# By class
m_e_ch_cg_fi_cor_current_class <- full_data_w %>% 
  filter(!is.na(current_class)) %>% 
  mutate(group=as.double(current_class)) %>% 
  group_by(group) %>% 
  summarize(ch_cor=cor(e_ch_fs_dummy,
                       m_ch_fs_dummy),
            cg_cor=cor(e_cg_fs_dummy,
                       m_cg_fs_dummy),
            
  ) %>% 
  pivot_longer(-group, 
               names_to = c("category", ".value"), 
               names_sep="_" ) %>% 
  mutate(group=case_when(group <=2 ~ glue('Kindergarten {group}'),
                         (group >2 & group <=8) ~ glue('Class {group-2}'),
                         (group >8 & group <=11) ~ glue('JHS {group-8}'),
                         T~glue('SHS {group-11}'),)) %>% 
  ungroup()

# Combine results
m_e_ch_cg_fi_cor <-pmap_dfr(m_e_ch_cg_corr_input,
                            m_e_ch_cg_corr_func) %>% 
  bind_rows(m_e_ch_cg_fi_cor_female_age,
            m_e_ch_cg_fi_cor_current_class) %>% 
  pivot_wider(names_from=category,
              values_from = cor) %>% 
  rename(mid_end_child_reports_cor=ch,
         mid_end_cg_reports_cor=cg)

############################# Outcome: Summary Statistics ###############################

outcome_data<-full_data_w %>% 
  select(-contains('_per')) %>% 
  left_join(outcomes, by=c('childid','careid'))

# Define function
outcome_func <- function(var_group_str, str_1, str_0) {
  
  if (is.na(var_group_str)) {
    outcome <- outcome_data %>%
      summarize(across(matches('_per'),
                       ~mean(., na.rm=T),
                       .names = "mean.{col}"),
                across(matches('_per'),
                       ~sd(., na.rm=T),
                       .names = "sd.{col}")
      ) %>% 
      select(-contains('sd.mean')) %>% 
      pivot_longer(everything(), 
                   names_to = c("category", ".value"), 
                   names_sep="_" ) %>% 
      mutate(group='Overall')
  } else {
    var_group <- ensym(var_group_str)
    outcome <- outcome_data %>%
      group_by(group=!!var_group) %>% 
      summarize(across(matches('_per'),
                       ~mean(., na.rm=T),
                       .names = "mean.{col}"),
                across(matches('_per'),
                       ~sd(., na.rm=T),
                       .names = "sd.{col}")
      ) %>% 
      select(-contains('sd.mean')) %>% 
      pivot_longer(-group, 
                   names_to = c("category", ".value"), 
                   names_sep="_" ) %>% 
      mutate(group=case_when(group==1 ~ str_1,
                             T~str_0))
  }
  
  return(outcome)
}

# Define Input
outcome_sum_stat_input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  NA, '', '',
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  # 'private_school', 'Child attends private school', 'Child does not attend private school',
  
)

# Female * Age
outcome_overall_female_age <- outcome_data %>%
  group_by(female, age) %>% 
  summarize(across(matches('_per'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}"),
            across(matches('_per'),
                   ~sd(., na.rm=T),
                   .names = "sd.{col}")
  ) %>% 
  select(-contains('sd.mean')) %>% 
  pivot_longer(-c(female,age), 
               names_to = c("category", ".value"), 
               names_sep="_" ) %>% 
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                            age==1 & female==0 ~ 'Male (10-17)',
                            age==0 & female==1 ~ 'Female (5-9)',
                            T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
outcome_sum_stat <-pmap_dfr(outcome_sum_stat_input,
               outcome_func) %>% 
  bind_rows(outcome_overall_female_age) %>% 
  mutate(round=case_when(str_sub(category,start=-1)=='m'~ "midline",
                         T~'endline'),
         category=sub(".[^.]*$", "",category)) %>% 
  pivot_wider(names_from=category,
              values_from=c(lit,sel,ef,num))
  

########################################## Export ########################################################
# Make list of summary statistic results
sum_stat_export <- list('child_cg_reports_cor' = ch_cg_fi_cor, 
                      'child_cg_reports_sum_stat' = ch_cg_fi_sum_stat, 
                      'mid_end_child_cg_reports_corr' = m_e_ch_cg_fi_cor,
                      'outcome_sum_stat'=outcome_sum_stat)

# Export
write.xlsx(sum_stat_export, file = '/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.31.2023_sum_stat/sum_stat.xlsx')

########################################################################################################
########################################## Outcome Cronbach's Alpha ########################################################
#########################################################################################################

x<-cronbach.alpha(outcome_data %>% dplyr::select(matches('^m_.*per$')), CI=TRUE)



