###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 23, 2025
# Purpose: Run Baseline OLS Regression with Child Age Interaction Effects

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

##########################################################################################
############################## Multivariate OLS Regression w/ Region and household randomized treatment + Age and Gender ##############################
##########################################################################################

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                                 model=c('~ e_cfies_scale+e_fies_scale+e_cfies_scale*age+e_fies_scale*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                                           reg_func) %>% 
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

# Export results
modelsummary(va_ols_region_results,
             title='Value-Added Model: Heterogeneity by Child Age Group',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*[0-9])",
             coef_map=c('e_cfies_scale1'="CFIES: Few Experiences",
                           'e_cfies_scale2'="CFIES: Several Experiences",
                           'e_cfies_scale3'="CFIES: Many Experiences",
                           'e_fies_scale1'="FIES: Moderate",
                           'e_fies_scale2'="FIES: Severe",
                           'age'='Child is 10–17',
                           'lagged_outcome'="Lagged Outcome",
                           'e_cfies_scale1:age'='CFIES: Few Experiences:Child is 10–17',
                           'e_cfies_scale2:age'='CFIES: Several Experiences:Child is 10–17',
                           'e_cfies_scale3:age'='CFIES: Many Experiences:Child is 10–17',
                           'e_fies_scale1:age'='FIES: Moderate:Child is 10–17',
                           'e_fies_scale2:age'='FIES: Severe:Child is 10–17',
                           '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/03_cfies_fies_reg/03_va_ols_child_age_group.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{reg:age}Value-Added Model: Heterogeneity by Child Age Group',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*[0-9])",
             coef_map=c('e_cfies_scale1'="CFIES: Few Experiences",
                           'e_cfies_scale2'="CFIES: Several Experiences",
                           'e_cfies_scale3'="CFIES: Many Experiences",
                           'e_fies_scale1'="FIES: Moderate",
                           'e_fies_scale2'="FIES: Severe",
                           'age'='Child is 10–17',
                           'lagged_outcome'="Lagged Outcome",
                           'e_cfies_scale1:age'='CFIES: Few Experiences:Child is 10–17',
                           'e_cfies_scale2:age'='CFIES: Several Experiences:Child is 10–17',
                           'e_cfies_scale3:age'='CFIES: Many Experiences:Child is 10–17',
                           'e_fies_scale1:age'='FIES: Moderate:Child is 10–17',
                           'e_fies_scale2:age'='FIES: Severe:Child is 10–17',
                           '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment.",
             out='latex',
             escape = FALSE)

