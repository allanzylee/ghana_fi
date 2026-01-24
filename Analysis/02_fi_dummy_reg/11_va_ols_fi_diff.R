###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: Jan 22, 2026
# Purpose: Run Baseline OLS Regression with difference in FI as relevant variable

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
  mutate(cfies_diff=e_cfies_indicator-m_cfies_indicator,
         fies_diff=e_fies_indicator-m_fies_indicator)

##########################################################################################
############################## Multivariate OLS Regression w/ Region and household randomized treatment + Age and Gender ##############################
##########################################################################################

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ cfies_diff+fies_diff+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+'))

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
             coef_omit = "^(?!.*tercept|.*diff|.*outcome|.*reatment)",
             coef_map=c('cfies_diff'="Child Becomes FI",
                        'fies_diff'="Caregiver Becomes FI",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively, with the difference between endline and midline FI used in this specification. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/11_va_ols_fi_diff.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{reg:multi}Value-Added Model',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*diff|.*outcome|.*reatment)",
             coef_map=c('cfies_diff'="Child Becomes FI",
                        'fies_diff'="Caregiver Becomes FI",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively, with the difference between endline and midline FI used in this specification. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, and household randomized treatment.",
             out='latex',
             escape = FALSE)

