###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Run Value-Added OLS with 3-way interactions:
#          Child Sex x FI x Poverty (13a) and Child Sex x FI x Caregiver Education (13b)

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
########### 13a: FI x Child Sex x Poverty (3-way) ######################################
##########################################################################################

va_ols_input_13a <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*female + e_fies_indicator*female +
                  e_cfies_indicator*poverty_std + e_fies_indicator*poverty_std +
                  e_cfies_indicator*female*poverty_std + e_fies_indicator*female*poverty_std +
                  female + poverty_std + age + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_13a <- pmap(va_ols_input_13a, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_13a <- c(
  'e_cfies_indicator'                    = "Child-Reported FI",
  'e_fies_indicator'                     = "Caregiver-Reported FI",
  'female'                               = "Child is Female",
  'poverty_std'                          = "Poverty (Std.)",
  'lagged_outcome'                       = "Lagged Outcome",
  'e_cfies_indicator:female'             = "Child-Reported FI $\\times$ Female",
  'e_fies_indicator:female'              = "Caregiver-Reported FI $\\times$ Female",
  'e_cfies_indicator:poverty_std'        = "Child-Reported FI $\\times$ Poverty",
  'e_fies_indicator:poverty_std'         = "Caregiver-Reported FI $\\times$ Poverty",
  'e_cfies_indicator:female:poverty_std' = "Child-Reported FI $\\times$ Female $\\times$ Poverty",
  'e_fies_indicator:female:poverty_std'  = "Caregiver-Reported FI $\\times$ Female $\\times$ Poverty",
  'treatment'                            = "Treatment",
  '(Intercept)'                          = "(Intercept)"
)

notes_13a <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Poverty is standardized to have mean zero and standard deviation one. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child age group, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_13a,
             title     = 'Value-Added Model: Heterogeneity by Child Sex and Poverty',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*poverty)",
             coef_map  = coef_map_13a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13a_va_ols_child_sex_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_13a,
             title     = '\\label{reg:sex_poverty}Value-Added Model: Heterogeneity by Child Sex and Poverty',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*poverty)",
             coef_map  = coef_map_13a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13a_va_ols_child_sex_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
########### 13b: FI x Child Sex x Caregiver Primary Education (3-way) ##################
##########################################################################################

va_ols_input_13b <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*female + e_fies_indicator*female +
                  e_cfies_indicator*cg_primary + e_fies_indicator*cg_primary +
                  e_cfies_indicator*female*cg_primary + e_fies_indicator*female*cg_primary +
                  female + cg_primary + age + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_13b <- pmap(va_ols_input_13b, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_13b <- c(
  'e_cfies_indicator'                    = "Child-Reported FI",
  'e_fies_indicator'                     = "Caregiver-Reported FI",
  'female'                               = "Child is Female",
  'cg_primary'                           = "Caregiver Completed Primary",
  'lagged_outcome'                       = "Lagged Outcome",
  'e_cfies_indicator:female'             = "Child-Reported FI $\\times$ Female",
  'e_fies_indicator:female'              = "Caregiver-Reported FI $\\times$ Female",
  'e_cfies_indicator:cg_primary'         = "Child-Reported FI $\\times$ Caregiver Completed Primary",
  'e_fies_indicator:cg_primary'          = "Caregiver-Reported FI $\\times$ Caregiver Completed Primary",
  'e_cfies_indicator:female:cg_primary'  = "Child-Reported FI $\\times$ Female $\\times$ Caregiver Completed Primary",
  'e_fies_indicator:female:cg_primary'   = "Caregiver-Reported FI $\\times$ Female $\\times$ Caregiver Completed Primary",
  'treatment'                            = "Treatment",
  '(Intercept)'                          = "(Intercept)"
)

notes_13b <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child age group, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_13b,
             title     = 'Value-Added Model: Heterogeneity by Child Sex and Caregiver Education',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*cg_primary)",
             coef_map  = coef_map_13b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13b_va_ols_child_sex_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_13b,
             title     = '\\label{reg:sex_edu}Value-Added Model: Heterogeneity by Child Sex and Caregiver Education',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*cg_primary)",
             coef_map  = coef_map_13b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13b_va_ols_child_sex_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)