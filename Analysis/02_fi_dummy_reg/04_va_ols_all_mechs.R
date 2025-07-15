###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 24, 2025
# Purpose: Run Baseline OLS Regression with All Mechanisms

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

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>% 
  # Filter age group to 10--17 year old since many of the investment mechanisms are NA for younger children
  filter(age==1,
         !is.na(e_ch_health),
         !is.na(e_private_school),
         !is.na(e_cg_edu_engagement),
         !is.na(e_ch_motiv),
         !is.na(e_ch_edu_asp),
         !is.na(e_cg_emotional_engagement),
         !is.na(e_attend),
         !is.na(cg_mh_scale)) %>% 
  mutate(e_attend=case_when(as.double(e_attend)>3~1,
                            T~0))

##########################################################################################
############################## Multivariate OLS Regression w/ Region and household randomized treatment + Age and Gender ##############################
##########################################################################################

# Define mechanisms of investments
health_input='e_ch_health'
edu_input='e_attend+e_private_school+e_cg_edu_engagement'
# Child self esteem is not included due to 54% of respondents missing data
child_psyc_input='e_ch_motiv+e_ch_edu_asp'
cg_psyc_input='e_cg_emotional_engagement+cg_mh_scale'

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num'),
                                   model=c(glue('~ e_ch_fs_dummy+e_cg_fs_dummy+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'),
                                           glue('~ e_ch_fs_dummy+e_cg_fs_dummy+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{edu_input}+{health_input}+{child_psyc_input}+{cg_psyc_input}+')
                                           ))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                             reg_func) %>% 
  set_names('Literacy: Base',
            'Numeracy: Base',
            'Literacy: All Mechs',
            'Numeracy: All Mechs')

# Export results
modelsummary(va_ols_region_results,
             title='Extended Value-Added Model: Base and All Mechanisms',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*dummy|.*outcome|.*health|.*attend|.*school|.*engagement|.*motiv|.*asp|.*scale)",
             coef_rename=c('e_ch_fs_dummy'="Child-Reported Food Insecurity",
                           'e_cg_fs_dummy'="Caregiver-Reported Food Insecurity",
                           'female'='Child is Female',
                           'lagged_outcome'="Lagged Outcome",
                           'e_ch_health2'="Poor Health",
                           'e_ch_health3'="Average Health",
                           'e_ch_health4'="Good Health",
                           'e_ch_health5'="Very Good Health",
                           'e_attend'="Attended School",
                           'e_private_school'="Private Shool",
                           'e_cg_edu_engagement'="Caregiver Edu. Engagement",
                           'e_ch_motiv'='Child Motivation',
                           'e_ch_edu_asp'='Child Edu. Aspiration',
                           'e_cg_emotional_engagement'='Caregiver Emo. Engagement',
                           'cg_mh_scale'='Caregiver Mental Health'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment. The child-reported health covariates are measured relative to children who reported very poor health. Attended School is a binary variable indicating whether the child attended school most of the time. Private school is a binary variable indicating whether the child attended private school. Caregiver Edu. Engagement is the sum of caregivers’ degree of agreement with statements related to whether they engage with their children’s education. Child Motivation is the sum of a child’s degree of agreement with statements related to whether they are motivated. Child Edu. Aspiration is a binary variable of whether a child aspires to complete high school. Caregiver Emo. Engagement is the sum of caregivers’ degree of agreement with statements related to whether they engage with their children’s emotional well-being. Caregiver Mental Health is the sum of caregivers’ degree of experience with poor mental health.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/04_va_ols_all_mechs.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{extended_all_mechs}Extended Value-Added Model: Base and All Mechanisms',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*dummy|.*outcome|.*health|.*attend|.*school|.*engagement|.*motiv|.*asp|.*scale)",
             coef_rename=c('e_ch_fs_dummy'="Child-Reported Food Insecurity",
                           'e_cg_fs_dummy'="Caregiver-Reported Food Insecurity",
                           'female'='Child is Female',
                           'lagged_outcome'="Lagged Outcome",
                           'e_ch_health2'="Poor Health",
                           'e_ch_health3'="Average Health",
                           'e_ch_health4'="Good Health",
                           'e_ch_health5'="Very Good Health",
                           'e_attend'="Attended School",
                           'e_private_school'="Private School",
                           'e_cg_edu_engagement'="Caregiver Edu. Engagement",
                           'e_ch_motiv'='Child Motivation',
                           'e_ch_edu_asp'='Child Edu. Aspiration',
                           'e_cg_emotional_engagement'='Caregiver Emo. Engagement',
                           'cg_mh_scale'='Caregiver Mental Health'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment. The child-reported health covariates are measured relative to children who reported very poor health. Attended School is a binary variable indicating whether the child attended school most of the time. Private school is a binary variable indicating whether the child attended private school. Caregiver Edu. Engagement is the sum of caregivers’ degree of agreement with statements related to whether they engage with their children’s education. Child Motivation is the sum of a child’s degree of agreement with statements related to whether they are motivated. Child Edu. Aspiration is a binary variable of whether a child aspires to complete high school. Caregiver Emo. Engagement is the sum of caregivers’ degree of agreement with statements related to whether they engage with their children’s emotional well-being. Caregiver Mental Health is the sum of caregivers’ degree of experience with poor mental health.",
             out='latex',
             latex_options = c("booktabs", "scale_down"),
             escape = FALSE)

