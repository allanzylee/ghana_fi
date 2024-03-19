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

  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)

  return(reg_robust[,2])
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

# Define whether to use FIES or FIES Scale (FAO)

fies_scale_indicator=T

if(fies_scale_indicator==T){
  fies<-"e_fies_scale"
  folder<-"02_fies_scale"
} else {
  fies<-"e_fies"
  folder<-"01_fies"
}

# Define base OLS input
base_ols_input_region_treatment <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c(glue('~ e_ch_fies+{fies}+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))

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

# Define Stargazer Labels
outcome_lables<-c("Literacy","Numeracy","Executive Function","SEL")

cov_labels <-c("CFIES: Few Experiences",
               "CFIES: Several Experiences",
               "CFIES: Many Experiences",
               "FIES: Moderate",
               "FIES: Severe",
               # "Child Female",
               # "Child is 10–17",
               "Region: North East",
               "Region: Northern",
               "Region: Upper East",
               "Region: Upper West",
               "PNP Treatment",
               "Lagged Outcome",
               "Constant")

stargazer(base_ols_results_region_treatment,
          title="Base OLS Regression with Region and Treatment FE",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = outcome_lables,
          covariate.labels=cov_labels,
          se=base_ols_robust_errors_region_treatment,
          # p=list(base_ols_robust_errors_region_treatment[['lit']][,4],base_ols_robust_errors_region_treatment[['num']][,4],base_ols_robust_errors_region_treatment[['ef']][,4],base_ols_robust_errors_region_treatment[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          # omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
          out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/03_ols_regs_fies/{folder}//01_base_ols.html"))

##########################################################################################
############################## Multivariate OLS Regression w/ Region and PNP Treatment ##############################
##########################################################################################

# Define base OLS input
reduced_multivar_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                          model=c(glue('~ e_ch_fies+{fies}+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))

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

multi_cov_labels <-c("CFIES: Few Experiences",
                     "CFIES: Several Experiences",
                     "CFIES: Many Experiences",
                     "FIES: Moderate",
                     "FIES: Severe",
                     "Child Female",
                     "Child is 10–17",
                     "Caregiver Age",
                     "Caregiver Female",
                     "Caregiver has a Partner",
                     "Caregiver Completed Primary School",
                     "Household Size",
                     "Language: Dagbani",
                     "Language: Gruni",
                     "Language: Other",
                     "Language: Sissali",
                     "Region: North East",
                     "Region: Northern",
                     "Region: Upper East",
                     "Region: Upper West",
                     "PNP Treatment",
                     "Lagged Outcome",
                     "Constant")

stargazer(reduced_multivar_ols_region_results,
          title="Multivariate OLS Regression",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=multi_cov_labels,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          # omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=reduced_multivar_ols_region_robust_errors,
          # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/03_ols_regs_fies/{folder}/02_multivar_ols.html"))

########################################################################################################################
############################## OLS Regression w/ Covariates and GENDER INTERACTION ##############################
########################################################################################################################

# This version removes enrollment, poverty, number of books, and private school as covariates 

# Define base OLS input
gender_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                         model=c(glue('~ e_ch_fies+{fies}+e_ch_fies*female+{fies}*female+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))

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

gender_multi_cov_labels <-c("CFIES: Few Experiences",
                     "CFIES: Several Experiences",
                     "CFIES: Many Experiences",
                     "FIES: Moderate",
                     "FIES: Severe",
                     "Child Female",
                     "Child is 10–17",
                     "Caregiver Age",
                     "Caregiver Female",
                     "Caregiver has a Partner",
                     "Caregiver Completed Primary School",
                     "Household Size",
                     "Language: Dagbani",
                     "Language: Gruni",
                     "Language: Other",
                     "Language: Sissali",
                     "Region: North East",
                     "Region: Northern",
                     "Region: Upper East",
                     "Region: Upper West",
                     "PNP Treatment",
                     "Lagged Outcome",
                     "CFIES: Few Experiences x Child Female",
                     "CFIES: Several Experiences x Child Female",
                     "CFIES: Many Experiences x Child Female",
                     "FIES: Moderate x Child Female",
                     "FIES: Severe x Child Female",
                     "Constant")

stargazer(gender_multivar_ols_results,
          title="Multivariate OLS Regression with Gender Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=gender_multi_cov_labels,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          # omit=c('age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther','languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=gender_multivar_ols_robust_errors,
          # p=list(gender_multivar_ols_robust_errors[['lit']][,4],gender_multivar_ols_robust_errors[['num']][,4],gender_multivar_ols_robust_errors[['ef']][,4],gender_multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/03_ols_regs_fies/{folder}/03_gender_multivar_ols.html"))

########################################################################################################################
############################## OLS Regression w/ Covariates and AGE INTERACTION ##############################
########################################################################################################################

# This version removes enrollment, poverty, number of books, and private school as covariates 

# Define base OLS input
age_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c(glue('~ e_ch_fies+{fies}+e_ch_fies*age+{fies}*age+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+language+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))

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

age_multi_cov_labels <-c("CFIES: Few Experiences",
                            "CFIES: Several Experiences",
                            "CFIES: Many Experiences",
                            "FIES: Moderate",
                            "FIES: Severe",
                         "Child is 10–17",
                            "Child Female",
                            "Caregiver Age",
                            "Caregiver Female",
                            "Caregiver has a Partner",
                            "Caregiver Completed Primary School",
                            "Household Size",
                            "Language: Dagbani",
                            "Language: Gruni",
                            "Language: Other",
                            "Language: Sissali",
                            "Region: North East",
                            "Region: Northern",
                            "Region: Upper East",
                            "Region: Upper West",
                            "PNP Treatment",
                            "Lagged Outcome",
                            "CFIES: Few Experiences x Child is 10–17",
                            "CFIES: Several Experiences x Child is 10–17",
                            "CFIES: Many Experiences x Child is 10–17",
                            "FIES: Moderate x Child is 10–17",
                            "FIES: Severe x Child is 10–17",
                            "Constant")

stargazer(age_multivar_ols_results,
          title="Multivariate OLS Regression with Age Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=age_multi_cov_labels,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          # omit=c('female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther',                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=age_multivar_ols_robust_errors,
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_ols/03_ols_regs_fies/{folder}/04_age_multivar_ols.html"))

