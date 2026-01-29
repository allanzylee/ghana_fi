###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 23, 2025
# Purpose: Run Baseline OLS Regression with Child Sex Interaction Effects

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
                                                 model=c('~ e_cfies_scale+e_fies_scale+e_cfies_scale*female+e_fies_scale*female+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                                           reg_func) %>% 
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

# Export results
modelsummary(va_ols_region_results,
             title='Value-Added Model: Heterogeneity by Child Sex',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*[0-9]|.*treatment)",
             coef_map=c('e_cfies_scale1'="CFIES: Few Experiences",
                        'e_cfies_scale2'="CFIES: Several Experiences",
                        'e_cfies_scale3'="CFIES: Many Experiences",
                        'e_fies_scale1'="FIES: Moderate",
                        'e_fies_scale2'="FIES: Severe",
                        'female'='Child is Female',
                        'lagged_outcome'="Lagged Outcome",
                        'e_cfies_scale1:female'='CFIES: Few Experiences:Child is Female',
                        'e_cfies_scale2:female'='CFIES: Several Experiences:Child is Female',
                        'e_cfies_scale3:female'='CFIES: Many Experiences:Child is Female',
                        'e_fies_scale1:female'='FIES: Moderate:Child is Female',
                        'e_fies_scale2:female'='FIES: Severe:Child is Female',
                        'treatment'='Treatment',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child age group, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/03_cfies_fies_reg/02_va_ols_child_sex.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{reg:sex}Value-Added Model: Heterogeneity by Child Sex',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*[0-9]|.*treatment)",
             coef_map=c('e_cfies_scale1'="CFIES: Few Experiences",
                        'e_cfies_scale2'="CFIES: Several Experiences",
                        'e_cfies_scale3'="CFIES: Many Experiences",
                        'e_fies_scale1'="FIES: Moderate",
                        'e_fies_scale2'="FIES: Severe",
                        'female'='Child is Female',
                        'lagged_outcome'="Lagged Outcome",
                        'e_cfies_scale1:female'='CFIES: Few Experiences:Child is Female',
                        'e_cfies_scale2:female'='CFIES: Several Experiences:Child is Female',
                        'e_cfies_scale3:female'='CFIES: Many Experiences:Child is Female',
                        'e_fies_scale1:female'='FIES: Moderate:Child is Female',
                        'e_fies_scale2:female'='FIES: Severe:Child is Female',
                        'treatment'='Treatment',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child age group, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/03_cfies_fies_reg/02_va_ols_child_sex.tex",
             escape = FALSE)

