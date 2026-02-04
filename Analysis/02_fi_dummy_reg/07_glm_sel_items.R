###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: July 4, 2025
# Purpose: Run Baseline OLS Regression

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())
options(modelsummary_factory_default = 'kableExtra')
options(modelsummary_factory_latex = 'kableExtra')
options(modelsummary_factory_html = 'kableExtra')
# Load header
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  mutate(across(contains('fs'),~as.double(.))) %>% 
  rename(careid=caseid) %>% 
  filter(io2==1)

m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  mutate(across(contains('fs'),~as.double(.))) %>% 
  filter(io2==1)

##########################################################################################
############################## Multivariate OLS Regression w/ Region and household randomized treatment + Age and Gender ##############################
##########################################################################################

# Define regression function
reg_func_sel <- function(category, model){
  
  outcome=paste0('e_',
                 category)
  
  m_category_str=paste0('m_',
                category)
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(outcome, model,'+','lagged_outcome'))  
  reg <- glm(fm,
               data=for_reg,
               family = 'binomial')
  return(reg)
}

# Define sel outcomes
e_sel_outcomes<-e_child %>% 
  select(childid,
         matches('re[1-7]')) %>% 
  select(-re10,
         -re11,
         -re5) %>% 
  mutate(across(-childid,~case_when(.==1~1,
                                        .==0~0,
                                        T~NA_real_))) %>% 
  rename_with(~ paste0("e_",.), .cols = matches("re"))

m_sel_outcomes<-m_child %>% 
  select(childid,
         matches('re[1-7]')) %>% 
  select(-re10,
         -re11,
         -re5) %>% 
  mutate(across(-childid,~case_when(.==1~1,
                                    .==0~0,
                                    T~NA_real_))) %>% 
  rename_with(~ paste0("m_",.), .cols = matches("re"))

full_data_w<-full_data_w %>% 
  left_join(e_sel_outcomes,
            by=c('childid')) %>% 
  left_join(m_sel_outcomes,
            by=c('childid'))

# Define base OLS input
va_ols_input_region <- expand.grid(category=paste0('re',c(1:4,6:7)),
                                                 model=c('~ e_cfies_indicator+e_fies_indicator+female+age+treatment+age_pct_rank+factor(month)+region_north_east+region_northern+region_upper_east+region_upper_west'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                                           reg_func_sel) %>% 
  set_names('Talk to Someone When Sad',
            'Ask for Help with Problem with Friend',
            'Ask for Help at Home with Something Difficult',
            'Ask for Help at Home with Homework',
            "Ask for Friends' Help with Something Difficult",
            "Ask for Friends' Help with Homework")

# Export results
modelsummary(va_ols_region_results,
             title='SEL Item Model',
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*treatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported Food Insecurity",
                           'e_fies_indicator'="Caregiver-Reported Food Insecurity",
                           'lagged_outcome'="Lagged Outcome",
                        'treatment'='Treatment',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Generalized Linear Models are used for this analysis to regress binary variables of whether a child reached out for help on food insecurity and other covariates. Covariates in the regression that are not shown include child sex, child age group, region, child rank in percentile by age, and month fixed effects.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/07_glm_sel_items.html",
             escape = FALSE)

raw<-modelsummary(va_ols_region_results,
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*treatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported Food Insecurity",
                        'e_fies_indicator'="Caregiver-Reported Food Insecurity",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Generalized Linear Models are used for this analysis to regress binary variables of whether a child reached out for help on food insecurity and other covariates. Covariates in the regression that are not shown include child sex, child age group, region, child rank in percentile by age, and month fixed effects.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/07_glm_sel_items.tex",
             latex_options = "scale_down",
             booktabs=T, threeparttable = TRUE)

formatted=raw %>% 
  column_spec(1,width="1in") %>% 
  column_spec(2:7,width="0.5in")

writeLines(formatted,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/07_glm_sel_items.tex")
# 
# ex=kbl(formatted)
# cat(ex)
# 
# kableExtra::save_kable(
#   formatted,
#   format='latex',
#   file = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/07_glm_sel_items.tex",
#   float=F
# )
