###################################### Ghana FI Introduction ############################################

# Author: Allan Lee (xiao bao)
# Date: December 29th, 2023
# Purpose: OLS Regressions

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
# library(foreign)
# library(haven)
library(tidyverse)
library(stargazer)
# library(psych)
# library(corrr)
# library(tibble)
# library(writexl)
# library(timechange)
# library(rnoaa)
# library(base)
# library(arsenal)
# library(labelled)
# library(zoo)
library(AER)
# library(GGally)
# library(broom.helpers)
# library(jtools)
# library(janitor)
library(dataCompareR)
library(broom)
library(xtable)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')
#full_data_l <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_l.rds')

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

  # # Compute cluster-robust standard errors
  # cluster_robust_se <- vcovCL(results[[category]], cluster = full_data_w)
  # 
  # # Extract coefficients and standard errors
  # coefficients <- coef(model)
  # cluster_robust_standard_errors <- sqrt(diag(cluster_robust_se))
  
  return(reg_robust)
}

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

# ##########################################################################################
# ######################################## Base OLS Regression ############################
# ##########################################################################################
# 
# # Define base OLS input
# base_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
#                               model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+'))
# 
# # Regression results
# base_ols_results<- pmap(base_ols_input,
#                             reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# base_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                               results_str='base_ols_results') %>% 
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# base_ols_robust_errors <- pmap(base_ols_robust_input,
#                                cluster_robust_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(base_ols_results,
#           title="Base OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
#           se=list(base_ols_robust_errors[['lit']][,2],base_ols_robust_errors[['num']][,2],base_ols_robust_errors[['ef']][,2],base_ols_robust_errors[['sel']][,2]),
#           p=list(base_ols_robust_errors[['lit']][,4],base_ols_robust_errors[['num']][,4],base_ols_robust_errors[['ef']][,4],base_ols_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_results/base_ols.html"
#           )
# 
# ##########################################################################################
# ############################## OLS Regression w/ Covariates ##############################
# ##########################################################################################
# 
# # Define base OLS input
# multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
#                               model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+cg_age +cg_female +marital_status+cg_schooling +poverty+hh_size+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language+'))
# 
# # Regression results
# multivar_ols_results<- pmap(multivar_ols_input,
#                         reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# multivar_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                      results_str='multivar_ols_results') %>% 
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# multivar_ols_robust_errors <- pmap(multivar_ols_robust_input,
#                                cluster_robust_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Export tidy results
# multivar_ols_df <-pmap_dfr(multivar_ols_robust_input %>% select(category),
#                      tidy_func)
# 
# ############################## Exporting Results ###############################
# 
# stargazer(multivar_ols_results,
#           title="Multivariate OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           # omit=omitted_variables,
#           se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
#           p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_results/multivar_ols.html")
# 
# # Shortened table for paper
# stargazer(multivar_ols_results,
#           title="Multivariate OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=c(variables[1:3],variables[38]),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           omit=omitted_variables,
#           se=list(lit_reg_full_robust[,2],num_reg_full_robust[,2],ef_reg_full_robust[,2],sel_reg_full_robust[,2]),
#           p=list(lit_reg_full_robust[,4],num_reg_full_robust[,4],ef_reg_full_robust[,4],sel_reg_full_robust[,4]))

# ###### Export regression coefficients as bar charts. First, create data frame of all regression coefficients #####
# multivar_for_chart<-multivar_ols_df %>% 
#   mutate(category=as.factor(category)) %>% 
#   filter(str_detect(term,'dummy'))
# 
# # Create regression bar chart
# multivar_full_coef_plot<-ggplot(multivar_for_chart,
#        aes(x=term, y=estimate)) +
#   geom_bar(stat = "identity") +
#   xlab("Variables") + 
#   ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error,
#                     ymax=estimate + 1.96 * std.error),
#                 size=.75, width=.3) +
#   facet_grid(~category) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/ols_full_bar_chart.png",
#        width=9.26,
#        height=5.5,
#        units="in")

# ####################################################################################################################
# ########################## Base OLS Model: Weather Deviance as X variable ###############################
# #########################################################################################################################
# 
# # Define base OLS input
# base_weather_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
#                               model=c('~ optimal_t_dev+optimal_p_dev+'))
# 
# # Regression results
# base_weather_ols_results<- pmap(base_weather_ols_input,
#                         reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# base_weather_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                      results_str='base_weather_ols_results') %>% 
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# base_weather_ols_robust_errors <- pmap(base_weather_ols_robust_input,
#                                robust_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(base_weather_ols_results,
#           title="Base OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
#           se=list(base_weather_ols_robust_errors[['lit']][,2],base_weather_ols_robust_errors[['num']][,2],base_weather_ols_robust_errors[['ef']][,2],base_weather_ols_robust_errors[['sel']][,2]),
#           p=list(base_weather_ols_robust_errors[['lit']][,4],base_weather_ols_robust_errors[['num']][,4],base_weather_ols_robust_errors[['ef']][,4],base_weather_ols_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/1.10.2024_weather_dev_ols/base_ols.html")
# 
# ####################################################################################################################
# ########################## Multivar OLS Model: Weather Deviance as X variable ###############################
# #########################################################################################################################
# 
# # Define base OLS input
# reduced_multivar_weather_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                           model=c('~ optimal_t_dev+optimal_p_dev+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language+'))
# 
# # Regression results
# reduced_multivar_weather_ols_results<- pmap(reduced_multivar_weather_ols_input,
#                                     reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# reduced_multivar_weather_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                                  results_str='reduced_multivar_weather_ols_results') %>% 
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# multivar_weather_ols_robust_errors <- pmap(reduced_multivar_weather_ols_robust_input,
#                                            robust_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Export tidy results
# reduced_multivar_weather_ols_df <-pmap_dfr(reduced_multivar_weather_ols_robust_input %>% dplyr::select(category),
#                                    tidy_func)
# 
# ############################## Exporting Results ###############################
# 
# stargazer(reduced_multivar_weather_ols_results,
#           title="Multivariate OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
#                  'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
#                  'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
#                  'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
#                  'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
#                  'languageSissali','languageOther'),
#           se=list(multivar_weather_ols_robust_errors[['lit']][,2],multivar_weather_ols_robust_errors[['num']][,2],multivar_weather_ols_robust_errors[['ef']][,2],multivar_weather_ols_robust_errors[['sel']][,2]),
#           p=list(multivar_weather_ols_robust_errors[['lit']][,4],multivar_weather_ols_robust_errors[['num']][,4],multivar_weather_ols_robust_errors[['ef']][,4],multivar_weather_ols_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/1.10.2024_weather_dev_ols/multivar_ols.html")

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

# Cluster Robust Standard Error results
base_ols_robust_errors_region_treatment <- pmap(base_ols_robust_input_region_treatment,
                                       robust_func) %>% 
  set_names('lit','num','ef','sel')

############################## Exporting Results ###############################

stargazer(base_ols_results_region_treatment,
          title="Base OLS Regression wit Region and Treatment FE",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
          # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
          se=list(base_ols_robust_errors_region_treatment[['lit']][,2],base_ols_robust_errors_region_treatment[['num']][,2],base_ols_robust_errors_region_treatment[['ef']][,2],base_ols_robust_errors_region_treatment[['sel']][,2]),
          p=list(base_ols_robust_errors_region_treatment[['lit']][,4],base_ols_robust_errors_region_treatment[['num']][,4],base_ols_robust_errors_region_treatment[['ef']][,4],base_ols_robust_errors_region_treatment[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_region_treatment_fe/base_ols_with_region_treatment_fe.html")

##########################################################################################
############################## Multivariate OLS Regression w/ Region ##############################
##########################################################################################

# Add region FE

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
                                           robust_func) %>% 
  set_names('lit','num','ef','sel')

# # Export tidy results
# reduced_multivar_ols_region_df <-pmap_dfr(reduced_multivar_ols_robust_region_input %>% dplyr::select(category),
#                                    tidy_func)

############################## Exporting Results ###############################

stargazer(reduced_multivar_ols_region_results,
          title="Multivariate OLS Regression",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=list(reduced_multivar_ols_region_robust_errors[['lit']][,2],reduced_multivar_ols_region_robust_errors[['num']][,2],reduced_multivar_ols_region_robust_errors[['ef']][,2],reduced_multivar_ols_region_robust_errors[['sel']][,2]),
          p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_region_treatment_fe/multivar_ols.html")

########################################################################################################################
############################## OLS Regression w/ Covariates and GENDER INTERACTION ##############################
########################################################################################################################

# This version removes enrollment, poverty, number of books, and private school as covariates 

# Define base OLS input
gender_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                         model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*female+e_cg_fs_dummy*female+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+treatment+language+region_northern+region_upper_east+region_upper_west+'))

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
                                          robust_func) %>% 
  set_names('lit','num','ef','sel')

# Export tidy results
gender_multivar_ols_df <-pmap_dfr(gender_multivar_ols_robust_input %>% dplyr::select(category),
                                  tidy_func)

############################## Exporting Results ###############################

stargazer(gender_multivar_ols_results,
          title="Multivariate OLS Regression with Gender Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther','languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=list(gender_multivar_ols_robust_errors[['lit']][,2],gender_multivar_ols_robust_errors[['num']][,2],gender_multivar_ols_robust_errors[['ef']][,2],gender_multivar_ols_robust_errors[['sel']][,2]),
          p=list(gender_multivar_ols_robust_errors[['lit']][,4],gender_multivar_ols_robust_errors[['num']][,4],gender_multivar_ols_robust_errors[['ef']][,4],gender_multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_region_treatment_fe/gender_multivar_ols.html")

########################################################################################################################
############################## OLS Regression w/ Covariates and AGE INTERACTION ##############################
########################################################################################################################

# This version removes enrollment, poverty, number of books, and private school as covariates 

# Define base OLS input
age_multivar_ols_input <- expand.grid(category=c('lit','num','ef','sel'),
                                      model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+cg_age +cg_female +marital_status+cg_schooling +hh_size+treatment+language+region_northern+region_upper_east+region_upper_west+'))

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
                                       robust_func) %>% 
  set_names('lit','num','ef','sel')

# Export tidy results
age_multivar_ols_df <-pmap_dfr(age_multivar_ols_robust_input %>% dplyr::select(category),
                               tidy_func)

############################## Exporting Results ###############################

stargazer(age_multivar_ols_results,
          title="Multivariate OLS Regression with Age Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther',                 'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          se=list(age_multivar_ols_robust_errors[['lit']][,2],age_multivar_ols_robust_errors[['num']][,2],age_multivar_ols_robust_errors[['ef']][,2],age_multivar_ols_robust_errors[['sel']][,2]),
          p=list(age_multivar_ols_robust_errors[['lit']][,4],age_multivar_ols_robust_errors[['num']][,4],age_multivar_ols_robust_errors[['ef']][,4],age_multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/ols_region_treatment_fe/age_multivar_ols.html")
