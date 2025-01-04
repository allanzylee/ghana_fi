###################################### Introduction ############################################

# Author: Allan Lee
# Date: April 10th, 2024
# Purpose: Clean outcome data shared by SW and EA

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
outcome <- read_dta("import/outcomes_zscore.dta")

# Pivot the data wider and rename columns
out<-outcome %>% 
  filter(!is.na(round)) %>% 
  group_by(childid,careid) %>% 
  pivot_wider(names_from='round',
              values_from=c('zf_per_se',
                            'zf_per_lit',
                            'zf_per_num',
                            'zf_per_ef')) %>% 
  select(childid,
         careid,
         contains('2'),
         contains('3')) %>% 
  ungroup()

# Define outcome column names
outcome_cat <- c('sel',
                 'lit',
                 'num',
                 'ef'
                 )

# Midline
outcome_columns<- c('childid',
                    'careid',
                    paste("m_", outcome_cat,"_per", sep = ""),
                    paste("e_", outcome_cat,"_per", sep = ""))
  
names(out)<-outcome_columns

# Export
saveRDS(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_zscore.rds")

########################################################################################  
### Replicate outcome z-score
########################################################################################
outcome_raw<-read_rds("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_raw.rds")

# create function to calculate z_score
z_score_func<-function(round,
                       outcome){
  
  # Create relevant variables and define data
  enum_id=paste0(round,'_enum_id') 
  treatment=paste0(round,'_treatment')
  outcome_round=paste0(round,
                 '_',
                 outcome,
                 '_per')
  data=outcome_raw %>% 
    filter(!is.na(!!sym(outcome_round)),
           !is.na(!!sym(enum_id))) %>% 
    mutate(across(contains('enum_id'),
                  ~as.factor(.)))

  
  # Get residual net of interviewer effects
  model <- lm(as.formula(paste0(outcome_round, "~",enum_id)), data = outcome_raw)
  data[['netted_per']] <- resid(model)
  
  # Calculate group means
  data_w_zscore<-data %>% 
    mutate(mean_var=case_when(!!sym(treatment) == 0 ~ mean(netted_per,
                                                    na.rm=T),
                              T~NA_real_),
           sd_var=case_when(!!sym(treatment) == 0 ~ sd(netted_per,
                                                    na.rm=T),
                              T~NA_real_)) %>%
    mutate(mean_var=max(mean_var,
                        na.rm=T),
           sd_var=max(sd_var,
                      na.rm=T)) %>% 
    mutate(z_score=(netted_per-mean_var)/sd_var) %>% 
    select(childid,
           z_score) %>% 
    mutate(round=round,
           outcome=outcome)
  
}

# Create input
input<-expand_grid(round=c('m','e'),
                   outcome=outcome_cat)

# Run function
zscore<-pmap_dfr(input,
                 z_score_func)

# Format checker version
checker<-zscore %>% 
  mutate(cat=paste(round,
                   outcome,
                   'per',
                   sep='_')) %>% 
  dplyr::select(childid,
                z_score,
                cat) %>% 
  pivot_wider(values_from = z_score,
              names_from = cat)

comp<-dataCompareR::rCompare(out %>% filter(childid!=''),
                             checker,
                             keys=c('childid'))

