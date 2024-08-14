###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 13th, 2024
# Purpose: Calculate outcome summary static by food insecurity group. X variable is FI status with two panels to represent child and caregiver.

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
outcome_raw <- read_rds("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_raw.rds")

############################# Construct Data to Calculate raw outcome % ###############################

data<-full_data_w %>% 
  dplyr::select(careid,
                childid,
                age,
                female,
                e_ch_fs_dummy,
                e_cg_fs_dummy) %>% 
  left_join(outcome_raw,
            by=c('childid',
                 'careid'))

############################# Child Outcomes: Mean by Group/Round ###############################

# Define function
outcome_sum_stat_func <- function(var_group_str, str_1, str_0) {
  
  var_group <- ensym(var_group_str)
  sum_stat <- data %>% 
    dplyr::select(!!var_group,
                  matches('^e_.*per$')) %>% 
    group_by(group=!!var_group) %>% 
    summarize(across(matches('per'),
                     ~mean(., na.rm=T),
                     .names = "mean.{col}")
    ) %>%
    mutate(group=case_when(group==1 ~ str_1,
                           T~str_0))
  return(sum_stat)
}

# Define Input
input <- tribble(
  ~var_group_str, ~str_1, ~str_0,
  'e_ch_fs_dummy',      "Child is FI", "Child is Not FI",
  'e_cg_fs_dummy',      "Household is FI", "Household is Not FI"
  
)
# 
# # Overall
# outcome_sum_stat_overall <- data %>% 
#   dplyr::select(matches('^e_.*per$')) %>% 
#   summarize(across(matches('^e_.*per$'),
#                    ~mean(., na.rm=T),
#                    .names = "mean.{col}")
#   ) %>% 
#   mutate(group='Overall')
# 
# # By Gender and Age
# outcome_sum_stat_female_age <- data %>% 
#   dplyr::select(matches('^e_.*per$'),
#                 female,
#                 age) %>% 
#   group_by(female, age) %>% 
#   summarize(across(matches('^e_.*per$'),
#                    ~mean(., na.rm=T),
#                    .names = "mean.{col}")
#   ) %>%
#   mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
#                          age==1 & female==0 ~ 'Male (10-17)',
#                          age==0 & female==1 ~ 'Female (5-9)',
#                          T~'Male (5-9)')) %>% 
#   ungroup() %>% 
#   dplyr::select(-age,-female)

# Combine results
for_ex <- pmap_dfr(input,outcome_sum_stat_func) %>% 
  # bind_rows(pmap_dfr(input,outcome_sum_stat_func),
  #           outcome_sum_stat_female_age) %>% 
  pivot_longer(c(-group),
               names_to = 'category',
               values_to='value')

############################# Create Exhibit ###############################
plot<-for_ex %>% 
  mutate(category=factor(category,
                         levels=c('mean.e_lit_per',
                                  'mean.e_num_per',
                                  'mean.e_ef_per',
                                  'mean.e_sel_per')),
         group=factor(group,
                      levels=c('Child is FI',
                               'Child is Not FI',
                               'Household is FI',
                               'Household is Not FI'))
  ) %>% 
  mutate(stakeholder=case_when(str_detect(group,
                                          "Child")==T~"Child-Reported Child Food Insecurity",
                               T~"Caregiver-Reported Household Food Insecurity")) %>% 
  mutate(group=case_when(str_detect(group,
                                    "Not FI")==T~"Food Secure",
                         T~"Food Insecure")) %>% 
  ggplot(aes(x=group,
             y=value,
             fill = category)) +
  geom_col(position='dodge') +
  facet_wrap(~stakeholder) +
  geom_text(aes(label=glue('{round(value,3)*100}%')),
            position=position_dodge(0.9),
            vjust=-0.5)+
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     labels = scales::percent) +
  labs(
    # title="Endline Child Cognitive and Socioemotional Outcomes by Group",
    y='',
    x='') +
  scale_x_discrete(breaks=c('Food Secure',
                            'Food Insecure'))+
  scale_fill_manual(values=c('#c5c6d0',
                             '#828282',
                             '#333333',
                             'black'),
                    c('mean.e_lit_per',
                      'mean.e_num_per',
                      'mean.e_ef_per',
                      'mean.e_sel_per'),
                    labels=c('Literacy',
                             'Numeracy',
                             'Executive Function',
                             'Socioemotional Learning')) +
  labs(title='Mean Endline Child Outcomes by Food Security Status and Report')+
  theme_classic()+
  theme(
    axis.text = element_text(color='black',
                             size=10),
    axis.ticks = element_line(color='black'),
    axis.line = element_line(color='black'),
    legend.position = 'bottom',
    legend.title=element_blank(),
    plot.title = element_text(hjust = 0.5,
                              color='black',
                              size=18,
                              face="bold")
  )

plot

# Export as PDF
ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/06c_outcome_sum_stat_by_fi.png",
       width=25,
       height=25,
       units='cm')
