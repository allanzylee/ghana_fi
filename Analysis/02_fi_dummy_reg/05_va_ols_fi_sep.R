###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 27, 2025
# Purpose: Run Baseline OLS Regression with separate FI

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
ch <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ e_cfies_indicator+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

# Regression results
ch_results<- pmap(ch,
                             reg_func) %>% 
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

# Define base OLS input
cg <- expand.grid(category=c('lit','num','ef','sel'),
                  model=c('~ e_fies_indicator+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

# Regression results
cg_results<- pmap(cg,
                  reg_func) %>% 
  set_names(
            'Literacy',
            'Numeracy',
            'EF',
            'SEL')

# Combine
to_export<-list('Child-Reports'=ch_results,
                'Caregiver-Reports'=cg_results)

# Export results
modelsummary(to_export,
             shape='rbind',
             title='Value-Added Model',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*treatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported Food Insecurity",
                           'e_fies_indicator'="Caregiver-Reported Food Insecurity",
                           'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/05_va_ols_fi_sep.html",
             escape = FALSE)

modelsummary(to_export,
             shape='rbind',
             title='\\label{reg:multi}Value-Added Model',
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*treatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported Food Insecurity",
                           'e_fies_indicator'="Caregiver-Reported Food Insecurity",
                           'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, region, and household randomized treatment.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/05_va_ols_fi_sep.tex",
             escape = FALSE)

