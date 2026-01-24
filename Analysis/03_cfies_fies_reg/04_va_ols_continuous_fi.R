###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: Jan 22, 2026
# Purpose: Run Baseline OLS Regression with raw child cognitive skill percent outcomes 

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

# Define functions

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ e_cfies_sum+e_fies_sum+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                             reg_func) %>% 
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

# Export results
modelsummary(va_ols_region_results,
             title='Value-Added Model',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*sum|.*outcome|.*reatment)",
             coef_map=c('e_cfies_sum'="CFIES Raw Score",
                        'e_fies_sum'="FIES Raw Score",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Raw CFIES and FIES are reported. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/03_cfies_fies_reg/04_va_ols_continuous_fi.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{reg:multi}Value-Added Model',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*sum|.*outcome|.*reatment)",
             coef_map=c('e_cfies_sum'="Child-Reported FI",
                        'e_fies_sum'="Caregiver-Reported FI",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Raw CFIES and FIES are reported. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, and household randomized treatment.",
             out='latex',
             escape = FALSE)

