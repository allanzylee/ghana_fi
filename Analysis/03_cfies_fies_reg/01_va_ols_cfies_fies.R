###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 23, 2025
# Purpose: Run Baseline OLS Regression

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
  mutate(e_fies_scale=as.factor(e_fies_scale))

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ e_cfies_scale+e_fies_scale+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+'))

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
             coef_omit = "^(?!.*tercept|.*[0-9]|.*outcome)",
             coef_rename=c(e_cfies_scale1="CFIES: Few Experiences",
                           e_cfies_scale2="CFIES: Several Experiences",
                           e_cfies_scale3="CFIES: Many Experiences",
                           e_fies_scale0="FIES: Mild",
                           e_fies_scale1="FIES: Moderate",
                           e_fies_scale2="FIES: Severe",
                           'lagged_outcome'="Lagged Outcome"),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/03_cfies_fies_reg/01_va_ols_cfies_fies.html",
             escape = FALSE)

modelsummary(va_ols_region_results,
             title='\\label{reg:multi}Value-Added Model',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*[0-9]|.*outcome)",
             coef_rename=c(e_cfies_scale1="Child: Few Exp. (CFIES=1-6)",
                           e_cfies_scale2="Child: Several Exp. (CFIES=7-10)",
                           e_cfies_scale3="Child: Many Exp. (CFIES=11-20)",
                           e_fies_scale0="FIES: Mild",
                           e_fies_scale1="Caregiver: Moderately Food Insecure (FIES=4-6)",
                           e_fies_scale2="Caregiver: Severely Food Insecure (FIES=7-8)",
                           'lagged_outcome'="Lagged Outcome"),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, region, and household randomized treatment.",
             out='latex',
             escape = FALSE)

