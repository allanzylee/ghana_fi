###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: December 29th, 2023
# Purpose: OLS Regressions

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

library(tidyverse)
library(stargazer)
library(AER)
library(dataCompareR)
library(broom)
library(xtable)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

##########################################################################################
######################################## Regression Functions ############################
##########################################################################################

# Define base OLS functions
reg_func <- function(category, model){
  m_category_str<-paste0("m_",category,"_per")
  e_category_str<-paste0("e_",category,"_per")
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  reg <- lm(fm,
            data=for_reg)
  # reg_robust <- coeftest(reg, vcovCL, cluster=full_data_w$careid)
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)
  
  out<-list(se=reg_robust[,2],
       p=reg_robust[,4])
  
  return(out)
}

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

####################################################################################################################
########################## Base OLS Model: Include Region and Treatment dummies ###############################
#########################################################################################################################

# Define base OLS input
base_ols_input_region_treatment <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'))

# Regression results
base_ols_results_region_treatment<- pmap(base_ols_input_region_treatment,
                                reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
base_ols_robust_input_region_treatment <- expand.grid(category=c('lit','num','ef','sel'),
                                             results_str='base_ols_results_region_treatment') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Errors
base_ols_robust_errors_region_treatment <- pmap(base_ols_robust_input_region_treatment,
                                       cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

############################## Exporting Results ###############################

stargazer(base_ols_results_region_treatment,
          title="Base OLS Regression wit Region and Treatment FE",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          se=lapply(base_ols_robust_errors_region_treatment, function(x) x$se),
          p=lapply(base_ols_robust_errors_region_treatment, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/01_base_ols.html")

##########################################################################################
############################## Multivariate OLS Regression w/ Region and PNP Treatment ##############################
##########################################################################################

# Define base OLS input
reduced_multivar_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                          model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+treatment+language+region_north_east+region_northern+region_upper_east+region_upper_west+'))

# Regression results
reduced_multivar_ols_region_results<- pmap(reduced_multivar_ols_input_region,
                                    reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
reduced_multivar_ols_robust_region_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                 results_str='reduced_multivar_ols_region_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
reduced_multivar_ols_region_robust_errors <- pmap(reduced_multivar_ols_robust_region_input,
                                           cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

# # Export tidy results
# reduced_multivar_ols_region_df <-pmap_dfr(reduced_multivar_ols_robust_region_input %>% dplyr::select(category),
#                                    tidy_func)

############################## Exporting Results ###############################

stargazer(reduced_multivar_ols_region_results,
          title="Multivariate OLS Regression",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=lapply(reduced_multivar_ols_region_robust_errors, function(x) x$se),
          p=lapply(reduced_multivar_ols_region_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/02_multivar_ols.html")

########################################################################################################################
############################## OLS Regression w/ Covariates and GENDER INTERACTION ##############################
########################################################################################################################

# This version only keeps child sex, age, treatment, and region as controls

# Define base OLS input
gender_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*female+e_cg_fs_dummy*female+female+age+treatment+language+region_northern+region_upper_east+region_upper_west+'))

# Regression results
gender_multivar_ols_results<- pmap(gender_multivar_ols_input,
                                   reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
gender_multivar_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                results_str='gender_multivar_ols_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
gender_multivar_ols_robust_errors <- pmap(gender_multivar_ols_robust_input,
                                          cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

# # Export tidy results
# gender_multivar_ols_df <-pmap_dfr(gender_multivar_ols_robust_input %>% dplyr::select(category),
#                                   tidy_func)

############################## Exporting Results ###############################

stargazer(gender_multivar_ols_results,
          title="Multivariate OLS Regression with Gender Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Child is Female","Lagged Outcome","Child-Reported FI x Child is Female","Caregiver-Reported FI x Child is Female","Constant"),
          omit=c('age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther','languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=lapply(gender_multivar_ols_robust_errors, function(x) x$se),
          p=lapply(gender_multivar_ols_robust_errors, function(x) x$p),
          # p=list(gender_multivar_ols_robust_errors[['lit']][,4],gender_multivar_ols_robust_errors[['num']][,4],gender_multivar_ols_robust_errors[['ef']][,4],gender_multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/03_gender_multivar_ols.html")

########################################################################################################################
############################## OLS Regression w/ Covariates and AGE INTERACTION ##############################
########################################################################################################################

# This version only keeps child sex, age, treatment, and region as controls 

# Define base OLS input
age_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+language+region_northern+region_upper_east+region_upper_west+'))

# Regression results
age_multivar_ols_results<- pmap(age_multivar_ols_input,
                                reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
age_multivar_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                             results_str='age_multivar_ols_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
age_multivar_ols_robust_errors <- pmap(age_multivar_ols_robust_input,
                                       cluster_robust_func) %>%
  set_names('lit','num','ef','sel')
# 
# # Export tidy results
# age_multivar_ols_df <-pmap_dfr(age_multivar_ols_robust_input %>% dplyr::select(category),
#                                tidy_func)

############################## Exporting Results ###############################

stargazer(age_multivar_ols_results,
          title="Multivariate OLS Regression with Age Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Child is 10–17","Lagged Outcome","Child-Reported FI x Child is 10–17","Caregiver-Reported FI x Child is 10–17","Constant"),
          omit=c('female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther',                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=lapply(age_multivar_ols_robust_errors, function(x) x$se),
          p=lapply(age_multivar_ols_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/04_age_multivar_ols.html")

########################################################################################################################
############################## OLS Regression w/ Covariates and Caregiver Education INTERACTION ##############################
########################################################################################################################

# This version removes enrollment, poverty, number of books, and private school as covariates 

# Define base OLS input
cg_schooling_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*cg_schooling+e_cg_fs_dummy*cg_schooling+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+treatment+language+region_northern+region_upper_east+region_upper_west+'))

# Regression results
cg_schooling_multivar_ols_results<- pmap(cg_schooling_multivar_ols_input,
                                reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
cg_schooling_multivar_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                             results_str='cg_schooling_multivar_ols_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
cg_schooling_multivar_ols_robust_errors <- pmap(cg_schooling_multivar_ols_robust_input,
                                       cluster_robust_func) %>%
  set_names('lit','num','ef','sel')
# 
# # Export tidy results
# age_multivar_ols_df <-pmap_dfr(age_multivar_ols_robust_input %>% dplyr::select(category),
#                                tidy_func)

############################## Exporting Results ###############################

stargazer(cg_schooling_multivar_ols_results,
          title="Multivariate OLS Regression with Caregiver Schooling Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Caregiver Schooling","Lagged Outcome","Child-Reported FI x Caregiver Schooling","Caregiver-Reported FI x Caregiver Schooling","Constant"),
          omit=c('age','female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther',                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=lapply(cg_schooling_multivar_ols_robust_errors, function(x) x$se),
          p=lapply(cg_schooling_multivar_ols_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/05_cg_schooling_multivar_ols.html")

##########################################################################################
############################## Multivariate OLS Regression w/ Region and PNP Treatment + Age and Gender ##############################
##########################################################################################

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                                 model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                                           reg_func) %>% 
  set_names('lit','num','ef','sel')

# Define base OLS Robust input
va_ols_robust_region_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                        results_str='va_ols_region_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
va_ols_region_robust_errors <- pmap(va_ols_robust_region_input,
                                                  cluster_robust_func) %>%
  set_names('lit','num','ef','sel')

# Export results
stargazer(va_ols_region_results,
          title="Multivariate OLS Regression",
          #dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=lapply(va_ols_region_robust_errors, function(x) x$se),
          p=lapply(va_ols_region_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/01_ols_regs/02_baseline_region_fe/06_va_ols.html")

