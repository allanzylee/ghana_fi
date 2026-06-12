###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Run Value-Added OLS with 3-way interactions:
#          Child Age x FI x Poverty (14a) and Child Age x FI x Caregiver Education (14b)

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>%
  mutate(poverty_std = as.numeric(scale(poverty)))

##########################################################################################
########### 14a: FI x Child Age x Poverty (3-way) ######################################
##########################################################################################

va_ols_input_14a <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*age + e_fies_indicator*age +
                  e_cfies_indicator*poverty_std + e_fies_indicator*poverty_std +
                  e_cfies_indicator*age*poverty_std + e_fies_indicator*age*poverty_std +
                  female + age + poverty_std + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_14a <- pmap(va_ols_input_14a, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_14a <- c(
  'e_cfies_indicator'                  = "Child-Reported FI",
  'e_fies_indicator'                   = "Caregiver-Reported FI",
  'age'                                = "Child is 10--17",
  'poverty_std'                        = "Poverty (Std.)",
  'lagged_outcome'                     = "Lagged Outcome",
  'e_cfies_indicator:age'              = "Child-Reported FI $\\times$ Child is 10--17",
  'e_fies_indicator:age'               = "Caregiver-Reported FI $\\times$ Child is 10--17",
  'e_cfies_indicator:poverty_std'      = "Child-Reported FI $\\times$ Poverty",
  'e_fies_indicator:poverty_std'       = "Caregiver-Reported FI $\\times$ Poverty",
  'e_cfies_indicator:age:poverty_std'  = "Child-Reported FI $\\times$ Child is 10--17 $\\times$ Poverty",
  'e_fies_indicator:age:poverty_std'   = "Caregiver-Reported FI $\\times$ Child is 10--17 $\\times$ Poverty",
  'treatment'                          = "Treatment",
  '(Intercept)'                        = "(Intercept)"
)

notes_14a <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Poverty is standardized to have mean zero and standard deviation one. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child sex, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_14a,
             title     = 'Value-Added Model: Heterogeneity by Child Age and Poverty',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*poverty)",
             coef_map  = coef_map_14a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14a_va_ols_child_age_group_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_14a,
             title     = '\\label{reg:age_poverty}Value-Added Model: Heterogeneity by Child Age and Poverty',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*poverty)",
             coef_map  = coef_map_14a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14a_va_ols_child_age_group_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
########### 14b: FI x Child Age x Caregiver Primary Education (3-way) ##################
##########################################################################################

va_ols_input_14b <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*age + e_fies_indicator*age +
                  e_cfies_indicator*cg_primary + e_fies_indicator*cg_primary +
                  e_cfies_indicator*age*cg_primary + e_fies_indicator*age*cg_primary +
                  female + age + cg_primary + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_14b <- pmap(va_ols_input_14b, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_14b <- c(
  'e_cfies_indicator'                   = "Child-Reported FI",
  'e_fies_indicator'                    = "Caregiver-Reported FI",
  'age'                                 = "Child is 10--17",
  'cg_primary'                          = "Caregiver Completed Primary",
  'lagged_outcome'                      = "Lagged Outcome",
  'e_cfies_indicator:age'               = "Child-Reported FI $\\times$ Child is 10--17",
  'e_fies_indicator:age'                = "Caregiver-Reported FI $\\times$ Child is 10--17",
  'e_cfies_indicator:cg_primary'        = "Child-Reported FI $\\times$ Caregiver Completed Primary",
  'e_fies_indicator:cg_primary'         = "Caregiver-Reported FI $\\times$ Caregiver Completed Primary",
  'e_cfies_indicator:age:cg_primary'    = "Child-Reported FI $\\times$ Child is 10--17 $\\times$ Caregiver Completed Primary",
  'e_fies_indicator:age:cg_primary'     = "Caregiver-Reported FI $\\times$ Child is 10--17 $\\times$ Caregiver Completed Primary",
  'treatment'                           = "Treatment",
  '(Intercept)'                         = "(Intercept)"
)

notes_14b <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child sex, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_14b,
             title     = 'Value-Added Model: Heterogeneity by Child Age and Caregiver Education',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*cg_primary)",
             coef_map  = coef_map_14b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14b_va_ols_child_age_group_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_14b,
             title     = '\\label{reg:age_edu}Value-Added Model: Heterogeneity by Child Age and Caregiver Education',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*cg_primary)",
             coef_map  = coef_map_14b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14b_va_ols_child_age_group_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)