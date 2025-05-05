###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: 3/17/24
# Purpose: OLS Regressions with different sets of mechanisms

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
library(glue)
library(stringr)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>% 
  # Filter age group to 10--17 year old since many of the investment mechanisms are NA for younger children
  filter(age==1,
         !is.na(e_ch_health),
         !is.na(e_private_school),
         !is.na(e_cg_edu_engagement),
         !is.na(e_ch_motiv),
         !is.na(e_ch_edu_asp),
         !is.na(e_cg_emotional_engagement),
         !is.na(e_attend),
         !is.na(cg_mh_scale)) %>% 
  mutate(e_attend=case_when(as.double(e_attend)>3~1,
                            T~0))

##########################################################################################
######################################## Regression Functions ############################
##########################################################################################

mech_reg_func<-function(category){
  
  # Define terms
  fi<-'e_ch_fs_dummy+e_cg_fs_dummy'
  category_text<-case_when(category=='lit'~'Literacy',
                           category=='num'~'Numeracy',
                           category=='ef'~'EF',
                           T~'SEL')
  
  # Define mechanisms of investments
  health_input='e_ch_health'
  edu_input='e_attend+e_private_school+e_cg_edu_engagement'
  # Child self esteem is not included due to 54% of respondents missing data
  child_psyc_input='e_ch_motiv+e_ch_edu_asp'
  cg_psyc_input='e_cg_emotional_engagement+cg_mh_scale'
  
  # Define covariate albels
  health_lab<-c("Poor Health",
                "Average Health",
                "Good Health",
                "Very Good Health")
  edu_lab<-c("Attended School",
             "Private Shool",
             "Caregiver Edu. Engagement")
  child_psyc_lab<-c('Child Motivation',
                      'Child Edu. Aspiration')
  cg_psyc_lab<-c('Caregiver Emo. Engagement',
                 "Caregiver Mental Health")
  
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
    
    out <-list(reg_robust[,2],
               reg_robust[,4])
    
    return(out)
  }
  
  # Define function for showing F-stat DF in two lines
  show_F_in_two_lines <- function(stargazer) {
    # `Stringr` works better than base's regex 
    require(stringr)
    
    # If you remove `capture.output()`, not only the modified LaTeX code 
    # but also the original code would show up
    stargazer <- stargazer |>
      capture.output()
    
    # Reuse the index in which F-statistics are displayed
    position_F <- str_which(stargazer, "F Statistic")
    
    # Extract only F-statistics
    Fs <- stargazer[position_F] |>
      str_replace_all("\\(.*?\\)", "")
    
    # Extract only df values and make a new line for them
    dfs <- stargazer[position_F] |>
      str_extract_all("\\(.*?\\)") |>
      unlist() |>
      (
        \(dfs)
        paste0(" & ", dfs, collapse = "")
      )() |>
      paste0(" \\\\")
    
    # Reuse table elements that are specified
    # after the index of F-statistics
    after_Fs <- stargazer[-seq_len(position_F)]
    
    c(
      stargazer[seq_len(position_F - 1)],
      Fs,
      dfs,
      after_Fs
    ) |>
      cat(sep = "\n")
  }
  
  # Define function for showing Res. SE DF in two lines
  show_res_se_in_two_lines <- function(stargazer) {
    # `Stringr` works better than base's regex 
    require(stringr)
    
    # If you remove `capture.output()`, not only the modified LaTeX code 
    # but also the original code would show up
    stargazer <- stargazer |>
      capture.output()
    
    # Reuse the index in which F-statistics are displayed
    position_res_se <- str_which(stargazer, "Residual Std. Error")
    
    # Extract only F-statistics
    res_ses <- stargazer[position_res_se] |>
      str_replace_all("\\(.*?\\)", "")
    
    # Extract only df values and make a new line for them
    dfs <- stargazer[position_res_se] |>
      str_extract_all("\\(.*?\\)") |>
      unlist() |>
      (
        \(dfs)
        paste0(" & ", dfs, collapse = "")
      )() |>
      paste0(" \\\\")
    
    # Reuse table elements that are specified
    # after the index of F-statistics
    after_res_ses <- stargazer[-seq_len(position_res_se)]
    
    c(
      stargazer[seq_len(position_res_se - 1)],
      res_ses,
      dfs,
      after_res_ses
    ) |>
      cat(sep = "\n")
  }
  
  # Define all regression inputs
  input<- expand.grid(category=c(category),
                               model=c(glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{health_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{edu_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{child_psyc_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{cg_psyc_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+{edu_input}+{health_input}+{child_psyc_input}+{cg_psyc_input}+')))
  
  # Regression results
  ols_results<- pmap(input,
                          reg_func) %>% 
    set_names('base',
              'health_input',
              'edu_input',
              'child_psyc_input',
              'cg_psyc_input',
              'all_input')
  
   # Define base OLS Robust input
  ols_robust_input <- expand.grid(category=c('base',
                                             'health_input',
                                                          'edu_input',
                                                          'child_psyc_input',
                                                          'cg_psyc_input',
                                                          'all_input'),
                                       results_str='ols_results') %>%
    mutate(across(everything(),~as.character(.)))
  
  # Cluster Robust Standard Errors
  ols_robust_errors <- pmap(ols_robust_input,
                                 cluster_robust_func) %>%
    set_names('base',
              'health_input',
              'edu_input',
              'child_psyc_input',
              'cg_psyc_input',
              'all_input')
  
  # Export formatted results
  stargazer(ols_results,
            title=glue("Extended Value-Added Model: {category_text}"),
            dep.var.caption = "Endline Dependent Variable:",
            column.labels = c("\\shortstack{Base\\\\ Model}",
                              "\\shortstack{Health\\\\ Inputs}",
                              "\\shortstack{Educational\\\\ Inputs}",
                              "\\shortstack{Child Psyc.\\\\ Inputs}",
                              "\\shortstack{Caregiver Psyc.\\\\ Inputs}", 
                              "\\shortstack{All\\\\ Inputs}"),
            covariate.labels=c("Child-Reported FI",
                               "Caregiver-Reported FI",
                               'Female',
                               health_lab,
                               edu_lab,
                               child_psyc_lab,
                               cg_psyc_lab,
                               "Lagged Outcome",
                               "Constant"),
            se=lapply(ols_robust_errors, function(x) x$se),
            p=lapply(ols_robust_errors, function(x) x$p),
            star.cutoffs = c(.05, .01, NA),
            notes.append     = FALSE,
            # notes            = "*$p<0.05$; **$p<0.01$",
            font.size = 'footnotesize',
            column.sep.width = "-10pt",
            omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment')
           ,out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/01_dummy/{category}.html")
           )|>
    show_F_in_two_lines() %>% 
    show_res_se_in_two_lines
}

# Run function
mech_reg_func('lit')
mech_reg_func('num')
mech_reg_func('ef')
mech_reg_func('sel')

# ####################################################################################################################
# ########################## Define terms to use and label ###############################
# #########################################################################################################################
# 
# # Define whether to use FIES or FIES Scale (FAO)
# 
# dummy_indicator<-T
# # FIES Scale Indicator is only relevant if dummy indicator is false
# fies_scale_indicator<-F
# 
# 
# if(dummy_indicator==T){
#   
#   fi<-"e_ch_fs_dummy+e_cg_fs_dummy"
#   folder<-"01_dummy"
#   # FI labels
#   fi_labels<-c("Child-Reported FI",
#                "Caregiver-Reported FI")
#   
# } else {
#   if(fies_scale_indicator==T){
#     fi<-"e_ch_fies+e_fies_scale"
#     folder<-"03_fies_scale"
#     
#     # FI labels
#     fi_labels<-c("CFIES: Few Experiences",
#                  "CFIES: Several Experiences",
#                  "CFIES: Many Experiences",
#                  "FIES: Mild",
#                  "FIES: Moderate",
#                  "FIES: Severe")
#     
#   } else {
#     fi<-"e_ch_fies+e_fies_sum"
#     folder<-"02_fies_sum"
#     
#     
#     fi_labels <-c("CFIES: Few Experiences",
#                   "CFIES: Several Experiences",
#                   "CFIES: Many Experiences",
#                   "FIES")
#   }
# }
# 
# # Define Stargazer Labels
# outcome_lables<-c("Literacy","Numeracy","Executive Function","SEL")
# 
# # Base labels
# cov_labels <-c(fi_labels,
               # "Region: North East",
               # "Region: Northern",
               # "Region: Upper East",
               # "Region: Upper West",
#                "PNP Treatment",
#                "Lagged Outcome",
#                "Constant")
# 
# # Multi labels
# edu_labels <-c(fi_labels,
#                "Child Female",
#                "Child Age",
#                "Child is Enrolled in School",
#                "Child Attends Private Shool",
#                "Lagged Outcome",
#                "Constant")
# 
# health_labels <-c(fi_labels,
#                   "Child Female",
#                   "Child Age",
#                   "Child Reported Poor Health",
#                   "Child Reported Average Health",
#                   "Child Reported Good Health",
#                   "Child Reported Very Good Health",
#                   "Child Reported Worse Relative Health",
#                   "Child Reported Same Relative Health",
#                   "Child Reported Better Relative Health",
#                   "Child Reported Much Better Relative Health",
#                   "Lagged Outcome",
#                   "Constant")
# 
# psyc_labels <-c(fi_labels,
#                 "Child Female",
#                 "Child Age",
#                 "Child Self-Esteem",
#                 "Child Education Aspiration",
#                 "Caregiver Education Aspiration for Child",
#                 "Lagged Outcome",
#                 "Constant")
# 
# all_labels <-c(fi_labels,
#                 "Child Female",
#                 "Child Age",
#                "Child is Enrolled in School",
#                "Child Attends Private Shool",
#                "Child Reported Poor Health",
#                "Child Reported Average Health",
#                "Child Reported Good Health",
#                "Child Reported Very Good Health",
#                "Child Reported Worse Relative Health",
#                "Child Reported Same Relative Health",
#                "Child Reported Better Relative Health",
#                "Child Reported Much Better Relative Health",
#                 "Child Self-Esteem",
#                 "Child Education Aspiration",
#                 "Caregiver Education Aspiration for Child",
#                 "Lagged Outcome",
#                 "Constant")
# 
# # Define Stargazer Labels
# outcome_lables<-c("Literacy","Numeracy","Executive Function","SEL")
# 
# ####################################################################################################################
# ########################## Base OLS Model: Include Region and Treatment dummies ###############################
# #########################################################################################################################
# 
# # Define base OLS input
# base_ols_input<- expand.grid(category=c('lit','num','ef','sel'),
#                                       model=c(glue('~ {fi} +female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))
# 
# # Regression results
# base_ols_results<- pmap(base_ols_input,
#                                 reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# base_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                              results_str='base_ols_results') %>%
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Errors
# base_ols_robust_errors <- pmap(base_ols_robust_input,
#                                        cluster_robust_func) %>%
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(base_ols_results,
#           title="Base OLS Regression wit Region and Treatment FE",
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = cov_labels,
#           # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
#           se=lapply(base_ols_robust_errors, function(x) x$se),
#           p=lapply(base_ols_robust_errors, function(x) x$p),
#           # p=list(base_ols_robust_errors[['lit']][,4],base_ols_robust_errors[['num']][,4],base_ols_robust_errors[['ef']][,4],base_ols_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
#           out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/01_base_ols.html"))
# 
# #######################################################################################################################
# ############################## Multivariate OLS Regression w/ Educational Investments Mechanisms ##############################
# #######################################################################################################################
# 
# # Define base OLS input
# ols_input_edu <- expand.grid(category=c('lit','num','ef','sel'),
#                                           model=c(glue('~ {fi}+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_enroll_ch+e_private_school+')))
# 
# # Regression results
# ols_edu_results<- pmap(ols_input_edu,
#                                     reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# ols_edu_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                                  results_str='ols_edu_results') %>%
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# ols_edu_robust_errors <- pmap(ols_edu_results_robust_input,
#                                            cluster_robust_func) %>%
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(ols_edu_results,
#           title="Multivariate OLS Regression: Educational Investment Mechanism",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           se=lapply(ols_edu_robust_errors, function(x) x$se),
#           p=lapply(ols_edu_robust_errors, function(x) x$p),
#           # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
#           out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/02_reg_edu.html"))
# 
# #######################################################################################################################
# ############################## Multivariate OLS Regression w/ Health Investment Mechanism ##############################
# #######################################################################################################################
# 
# # Define base OLS input
# ols_input_health <- expand.grid(category=c('lit','num','ef','sel'),
#                              model=c(glue('~ {fi}+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_ch_health+e_ch_health_rel+')))
# 
# # Regression results
# ols_health_results<- pmap(ols_input_health,
#                        reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# ols_health_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                             results_str='ols_health_results') %>%
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# ols_health_robust_errors <- pmap(ols_health_results_robust_input,
#                               cluster_robust_func) %>%
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(ols_health_results,
#           title="Multivariate OLS Regression: Health Investment Mechanism",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           se=lapply(ols_health_robust_errors, function(x) x$se),
#           p=lapply(ols_health_robust_errors, function(x) x$p),
#           # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
#           out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/03_reg_health.html"))
# 
# 
# #######################################################################################################################
# ############################## Multivariate OLS Regression w/ Psychological Investment Mechanism ##############################
# #######################################################################################################################
# 
# # Define base OLS input
# ols_input_psyc <- expand.grid(category=c('lit','num','ef','sel'),
#                                 model=c(glue('~ {fi}+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_ch_esteem+e_ch_edu_asp+e_cg_edu_asp+')))
# 
# # Regression results
# ols_psyc_results<- pmap(ols_input_psyc,
#                           reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# ols_psyc_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                                results_str='ols_psyc_results') %>%
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# ols_psyc_robust_errors <- pmap(ols_psyc_results_robust_input,
#                                  cluster_robust_func) %>%
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(ols_psyc_results,
#           title="Multivariate OLS Regression: Psychological Investment Mechanism",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           se=lapply(ols_psyc_robust_errors, function(x) x$se),
#           p=lapply(ols_psyc_robust_errors, function(x) x$p),
#           # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
#           out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/04_reg_psyc.html"))
# 
# #######################################################################################################################
# ############################## Multivariate OLS Regression w/ All Investment Mechanism ##############################
# #######################################################################################################################
# 
# # Define base OLS input
# ols_input_all <- expand.grid(category=c('lit','num','ef','sel'),
#                               model=c(glue('~ {fi}+female+age+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_enroll_ch+e_private_school+e_ch_esteem+e_ch_edu_asp+e_cg_edu_asp+e_ch_esteem+e_ch_edu_asp+e_cg_edu_asp+')))
# 
# # Regression results
# ols_all_results<- pmap(ols_input_all,
#                         reg_func) %>% 
#   set_names('lit','num','ef','sel')
# 
# # Define base OLS Robust input
# ols_all_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
#                                              results_str='ols_all_results') %>%
#   mutate(across(everything(),~as.character(.)))
# 
# # Cluster Robust Standard Error results
# ols_all_robust_errors <- pmap(ols_all_results_robust_input,
#                                cluster_robust_func) %>%
#   set_names('lit','num','ef','sel')
# 
# ############################## Exporting Results ###############################
# 
# stargazer(ols_all_results,
#           title="Multivariate OLS Regression: All Investment Mechanisms",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=all_labels,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           se=lapply(ols_all_robust_errors, function(x) x$se),
#           p=lapply(ols_all_robust_errors, function(x) x$p),
#           # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
#           star.cutoffs = c(.05, .01, NA),
#           notes.append     = FALSE,
#           notes            = "*$p<0.05$; **$p<0.01$",
#           omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
#           out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/04_reg_psyc.html"))
