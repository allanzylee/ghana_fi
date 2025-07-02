###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 5th, 2025
# Purpose: Calculate the percentage of FI for overlapping items

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
  
# Calculate overlap percentage ------------------------------------------------------
overlap_perc_raw<-full_data_w %>% 
  dplyr::select(childid,
         careid,
         matches("^fs\\d+"),
         female,
         age) %>% 
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>% 
  dplyr::select(childid,
                careid,
                female,
                age,
                # Worry about lack of food
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                # Size of meal cut
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                # Skipped meal
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                # Hungry but didn't eat
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
                ) %>% 
  pivot_longer(cols=-c(childid,
                       careid,
                       female,
                       age),
               names_to = c(".value", "category"), 
               names_sep ="_") 

perc_overall<-overlap_perc_raw%>% 
  group_by(category) %>% 
  summarise(across(c("worry",
                     'cut',
                     'skip',
                     'hungry'),
                   ~mean(.,
                         na.rm=T))) %>% 
  mutate(type='Overall')

perc_gender<-overlap_perc_raw%>% 
  group_by(category,
           female)%>% 
  summarise(across(c("worry",
                     'cut',
                     'skip',
                     'hungry'),
                   ~mean(.,
                         na.rm=T))) %>% 
  mutate(type=case_when(female==1 ~ 'Child is Female',
                         T~'Child is Male')) %>% 
  ungroup() %>% 
  dplyr::select(-female) %>% 
  arrange(type)

perc_age<-overlap_perc_raw%>% 
  group_by(category,
           age)%>% 
  summarise(across(c("worry",
                     'cut',
                     'skip',
                     'hungry'),
                   ~mean(.,
                         na.rm=T))) %>% 
  mutate(type=case_when(age==1 ~ 'Child is 10-17',
                        T~'Child is 5-9')) %>% 
  ungroup() %>% 
  dplyr::select(-age)%>% 
  arrange(type)

perc_gender_age<-overlap_perc_raw%>% 
  group_by(category,
           age,
           female)%>% 
  summarise(across(c("worry",
                     'cut',
                     'skip',
                     'hungry'),
                   ~mean(.,
                         na.rm=T))) %>% 
  mutate(type=case_when(age==1 & female==1 ~ 'Child is Female (10-17)',
                        age==1 & female==0 ~ 'Child is Male (10-17)',
                        age==0 & female==1 ~ 'Child is Female (5-9)',
                        T~'Child is Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,
                -female) %>% 
  arrange(type)

overlap_perc=bind_rows(perc_overall,
                       perc_gender,
                       perc_age,
                       perc_gender_age)

# Create unction for overlap cor ------------------------------------------------------

# Create data cor correlation 
overlap_cor_df<-full_data_w %>% 
  dplyr::select(childid,
                careid,
                age,
                female,
                matches("^fs\\d+")) %>% 
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>% 
  dplyr::select(childid,
                careid,
                age,
                female,
                # Worry about lack of food
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                # Size of meal cut
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                # Skipped meal
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                # Hungry but didn't eat
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
  )

cor_func<-function(by_var=NULL){
  
  out<-overlap_cor_df %>% 
    group_by(across({{by_var}})) %>% 
    summarise(worry=list(cor.test(worry_child, worry_cg)),
              cut=list(cor.test(cut_child, cut_cg)),
              skip=list(cor.test(skip_child, skip_cg)),
              hungry=list(cor.test(hungry_child, hungry_cg)))
  
  out$worry[[1]]  
}


overall <- overlap_cor_input %>% 
  summarise(worry=cor(worry_child,worry_cg),
            cut=cor(cut_child,cut_cg),
            skip=cor(skip_child,skip_cg),
            hungry=cor(hungry_child,hungry_cg)) %>% 
  mutate(category='Overall')

gender<- overlap_cor_input %>% 
  group_by(female) %>% 
  summarise(worry=cor(worry_child,worry_cg),
            cut=cor(cut_child,cut_cg),
            skip=cor(skip_child,skip_cg),
            hungry=cor(hungry_child,hungry_cg)) %>% 
  mutate(category=case_when(female==1 ~ 'Child is Female',
                            T~'Child is Male')) %>% 
  ungroup() %>% 
  dplyr::select(-female)

age<- overlap_cor_input %>% 
  group_by(age) %>% 
  summarise(worry=cor(worry_child,worry_cg),
            cut=cor(cut_child,cut_cg),
            skip=cor(skip_child,skip_cg),
            hungry=cor(hungry_child,hungry_cg)) %>% 
  mutate(category=case_when(age==1 ~ 'Child is 10-17',
                            T~'Child is 5-9')) %>% 
  ungroup() %>% 
  dplyr::select(-age)


gender_age <- overlap_cor_input %>% 
  group_by(age,
           female) %>% 
  summarise(worry=cor(worry_child,worry_cg),
            cut=cor(cut_child,cut_cg),
            skip=cor(skip_child,skip_cg),
            hungry=cor(hungry_child,hungry_cg)) %>% 
  mutate(category=case_when(age==1 & female==1 ~ 'Child is Female (10-17)',
                            age==1 & female==0 ~ 'Child is Male (10-17)',
                            age==0 & female==1 ~ 'Child is Female (5-9)',
                            T~'Child is Male (5-9)')) %>% 
  ungroup() %>% 
  dplyr::select(-age,
         -female)

overlap_cor<-bind_rows(overall,
                       gender,
                       age,
                       gender_age)

# Export
to_export<-list('cor'=as_tibble(overlap_cor),
             'perc'=as_tibble(overlap_perc))

write_xlsx(to_export,
          "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/08_fi_overlap_items.xlsx")

# Create latex friendly version of table
latex_cor<-xtable(overlap_cor %>% 
         dplyr::select('Category'=category,
                'Worry'=worry,
                'Cut'=cut,
                'Skip'=skip,
                'Hungry'=hungry))

print(latex_cor,
      include.rownames=FALSE)

latex_perc=xtable(overlap_perc %>% 
         dplyr::select('Group'=type,
                       'Worry'=worry,
                       'Cut'=cut,
                       'Skip'=skip,
                       'Hungry'=hungry,
                       'Reported by'=category) %>% 
  mutate(`Reported by`=case_when(`Reported by`=='cg'~"Caregiver",
                                 T~'Child')))

print(latex_perc,
      include.rownames=FALSE)

# Calculate percentage difference mean
perc_diff_mean<-overlap_perc %>% 
  group_by(type) %>% 
  mutate(across(c(worry,
                  cut,
                  skip,
                  hungry),~.[category=='cg']-.[category=='child'])) %>% 
  ungroup() %>% 
  distinct(worry,
           cut,
           skip,
           hungry) %>% 
  summarise(across(c(worry,
                     cut,
                     skip,
                     hungry),~mean(.,na.rm=T))) %>% 
  rowwise() %>% 
  summarise(
            mean = mean(c_across(worry:hungry)),
            median = median(c_across(worry:hungry))) 
