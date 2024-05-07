###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 6th, 2024
# Purpose: Calculate outcome summary static by group

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
                female) %>% 
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
  'female',      "Female", "Male",
  'age', 'Child is 10-17', 'Child is 5-9',
  
)

# Overall
outcome_sum_stat_overall <- data %>% 
  dplyr::select(matches('^e_.*per$')) %>% 
  summarize(across(matches('^e_.*per$'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}")
  ) %>% 
  mutate(group='Overall')

# By Gender and Age
outcome_sum_stat_female_age <- data %>% 
  dplyr::select(matches('^e_.*per$'),
                female,
                age) %>% 
  group_by(female, age) %>% 
  summarize(across(matches('^e_.*per$'),
                   ~mean(., na.rm=T),
                   .names = "mean.{col}")
  ) %>%
  mutate(group=case_when(age==1 & female==1 ~ 'Female (10-17)',
                         age==1 & female==0 ~ 'Male (10-17)',
                         age==0 & female==1 ~ 'Female (5-9)',
                         T~'Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,-female)

# Combine results
for_ex <- outcome_sum_stat_overall %>% 
  bind_rows(pmap_dfr(input,outcome_sum_stat_func),
            outcome_sum_stat_female_age) %>% 
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
                         levels=c('Overall',
                                  'Male',
                                  'Female',
                                  'Child is 5-9',
                                  'Child is 10-17',
                                  'Male (5-9)',
                                  'Male (10-17)',
                                  'Female (5-9)',
                                  'Female (10-17)'))) %>% 
  ggplot(aes(x=group,
             y=value,
             fill = category))+
  geom_col(position='dodge') +
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
  scale_x_discrete(breaks=c('Overall',
                            'Male',
                            'Female',
                            'Child is 5-9',
                            'Child is 10-17',
                            'Male (5-9)',
                            'Male (10-17)',
                            'Female (5-9)',
                            'Female (10-17)'))+
  scale_fill_manual(values=c('#C00000',
                             '#EE6363',
                             '#0070c1',
                             '#559bf0'),
                    breaks=c('mean.e_lit_per',
                             'mean.e_num_per',
                             'mean.e_ef_per',
                             'mean.e_sel_per'),
                    labels=c('Literacy',
                             'Numeracy',
                             'Executive Function',
                             'Socioemotional Learning')) +
  theme_classic()+
  theme(
    axis.text = element_text(color='black',
                             size=10),
    axis.ticks = element_line(color='black'),
    axis.line = element_line(color='black'),
    legend.position = 'bottom',
    legend.title=element_blank()
  )

plot

# Export as PDF
ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/06_outcome_sum_stat_by_group.png",
       width=25,
       height=25,
       units='cm')





