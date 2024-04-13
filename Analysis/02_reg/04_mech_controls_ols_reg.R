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
  
    out <-list(reg_robust[,2],
               reg_robust[,4])
    
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
  ########################## Define terms to use and label ###############################
  #########################################################################################################################
  
  # Define whether to use FIES or FIES Scale (FAO)
  
  dummy_indicator<-F
  # FIES Scale Indicator is only relevant if dummy indicator is false
  fies_scale_indicator<-F
  
  
  if(dummy_indicator==T){
    
    fi<-"e_ch_fs_dummy+e_cg_fs_dummy"
    folder<-"01_dummy"
    # FI labels
    fi_labels<-c("Child-Reported FI",
                 "Caregiver-Reported FI")
    
  } else {
    if(fies_scale_indicator==T){
      fi<-"e_ch_fies+e_fies_scale"
      folder<-"03_fies_scale"
      
      # FI labels
      fi_labels<-c("CFIES: Few Experiences",
                   "CFIES: Several Experiences",
                   "CFIES: Many Experiences",
                   "FIES: Mild",
                   "FIES: Moderate",
                   "FIES: Severe")
      
    } else {
      fi<-"e_ch_fies+e_fies_sum"
      folder<-"02_fies_sum"
      
      
      fi_labels <-c("CFIES: Few Experiences",
                    "CFIES: Several Experiences",
                    "CFIES: Many Experiences",
                    "FIES")
    }
  }
  
  # Define Stargazer Labels
  outcome_lables<-c("Literacy","Numeracy","Executive Function","SEL")
  
  # Base labels
  cov_labels <-c(fi_labels,
                 "Region: North East",
                 "Region: Northern",
                 "Region: Upper East",
                 "Region: Upper West",
                 "PNP Treatment",
                 "Lagged Outcome",
                 "Constant")
  
  # Multi labels
  edu_labels <-c(fi_labels,
                 "Child Female",
                 "Child Age",
                 "Child is Enrolled in School",
                 "Child Attends Private Shool",
                 "Lagged Outcome",
                 "Constant")
  
  health_labels <-c(fi_labels,
                    "Child Female",
                    "Child Age",
                    "Child Reported Poor Health",
                    "Child Reported Average Health",
                    "Child Reported Good Health",
                    "Child Reported Very Good Health",
                    "Child Reported Worse Relative Health",
                    "Child Reported Same Relative Health",
                    "Child Reported Better Relative Health",
                    "Child Reported Much Better Relative Health",
                    "Lagged Outcome",
                    "Constant")
  
  psyc_labels <-c(fi_labels,
                  "Child Female",
                  "Child Age",
                  "Child Self-Esteem",
                  "Child Education Aspiration",
                  "Caregiver Education Aspiration for Child",
                  "Lagged Outcome",
                  "Constant")
  
  # Define Stargazer Labels
  outcome_lables<-c("Literacy","Numeracy","Executive Function","SEL")
  
  ####################################################################################################################
  ########################## Base OLS Model: Include Region and Treatment dummies ###############################
  #########################################################################################################################
  
  # Define base OLS input
  base_ols_input<- expand.grid(category=c('lit','num','ef','sel'),
                                        model=c(glue('~ {fi} +female+age_num+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+')))
  
  # Regression results
  base_ols_results<- pmap(base_ols_input,
                                  reg_func) %>% 
    set_names('lit','num','ef','sel')
  
  # Define base OLS Robust input
  base_ols_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                               results_str='base_ols_results') %>%
    mutate(across(everything(),~as.character(.)))
  
  # Cluster Robust Standard Errors
  base_ols_robust_errors <- pmap(base_ols_robust_input,
                                         cluster_robust_func) %>%
    set_names('lit','num','ef','sel')
  
  ############################## Exporting Results ###############################
  
  stargazer(base_ols_results,
            title="Base OLS Regression wit Region and Treatment FE",
            dep.var.caption = "Endline Dependent Variable:",
            column.labels = cov_labels,
            # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
            se=lapply(base_ols_robust_errors, function(x) x$se),
            p=lapply(base_ols_robust_errors, function(x) x$p),
            # p=list(base_ols_robust_errors[['lit']][,4],base_ols_robust_errors[['num']][,4],base_ols_robust_errors[['ef']][,4],base_ols_robust_errors[['sel']][,4]),
            star.cutoffs = c(.05, .01, NA),
            notes.append     = FALSE,
            notes            = "*$p<0.05$; **$p<0.01$",
            omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
            out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/01_base_ols.html"))
  
  #######################################################################################################################
  ############################## Multivariate OLS Regression w/ Educational Investments Mechanisms ##############################
  #######################################################################################################################
  
  # Define base OLS input
  ols_input_edu <- expand.grid(category=c('lit','num','ef','sel'),
                                            model=c(glue('~ {fi}+female+age_num+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_enroll_ch+e_private_school+')))
  
  # Regression results
  ols_edu_results<- pmap(ols_input_edu,
                                      reg_func) %>% 
    set_names('lit','num','ef','sel')
  
  # Define base OLS Robust input
  ols_edu_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                   results_str='ols_edu_results') %>%
    mutate(across(everything(),~as.character(.)))
  
  # Cluster Robust Standard Error results
  ols_edu_robust_errors <- pmap(ols_edu_results_robust_input,
                                             cluster_robust_func) %>%
    set_names('lit','num','ef','sel')
  
  ############################## Exporting Results ###############################
  
  stargazer(ols_edu_results,
            title="Multivariate OLS Regression: Educational Investment Mechanism",
            dep.var.caption = "Endline Dependent Variable:",
            # covariate.labels=variables,
            column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
            se=lapply(ols_edu_robust_errors, function(x) x$se),
            p=lapply(ols_edu_robust_errors, function(x) x$p),
            # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
            star.cutoffs = c(.05, .01, NA),
            notes.append     = FALSE,
            notes            = "*$p<0.05$; **$p<0.01$",
            omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
            out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/02_reg_edu.html"))
  
  #######################################################################################################################
  ############################## Multivariate OLS Regression w/ Health Investment Mechanism ##############################
  #######################################################################################################################
  
  # Define base OLS input
  ols_input_health <- expand.grid(category=c('lit','num','ef','sel'),
                               model=c(glue('~ {fi}+female+age_num+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_ch_health+e_ch_health_rel+')))
  
  # Regression results
  ols_health_results<- pmap(ols_input_health,
                         reg_func) %>% 
    set_names('lit','num','ef','sel')
  
  # Define base OLS Robust input
  ols_health_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                              results_str='ols_health_results') %>%
    mutate(across(everything(),~as.character(.)))
  
  # Cluster Robust Standard Error results
  ols_health_robust_errors <- pmap(ols_health_results_robust_input,
                                cluster_robust_func) %>%
    set_names('lit','num','ef','sel')
  
  ############################## Exporting Results ###############################
  
  stargazer(ols_health_results,
            title="Multivariate OLS Regression: Health Investment Mechanism",
            dep.var.caption = "Endline Dependent Variable:",
            # covariate.labels=variables,
            column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
            se=lapply(ols_health_robust_errors, function(x) x$se),
            p=lapply(ols_health_robust_errors, function(x) x$p),
            # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
            star.cutoffs = c(.05, .01, NA),
            notes.append     = FALSE,
            notes            = "*$p<0.05$; **$p<0.01$",
            omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
            out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/03_reg_health.html"))
  
  
  #######################################################################################################################
  ############################## Multivariate OLS Regression w/ Psychological Investment Mechanism ##############################
  #######################################################################################################################
  
  # Define base OLS input
  ols_input_psyc <- expand.grid(category=c('lit','num','ef','sel'),
                                  model=c(glue('~ {fi}+female+age_num_region_north_east+region_northern+region_upper_east+region_upper_west+treatment+e_ch_esteem+e_ch_edu_asp+e_cg_edu_asp+')))
  
  # Regression results
  ols_psyc_results<- pmap(ols_input_psyc,
                            reg_func) %>% 
    set_names('lit','num','ef','sel')
  
  # Define base OLS Robust input
  ols_psyc_results_robust_input <- expand.grid(category=c('lit','num','ef','sel'),
                                                 results_str='ols_psyc_results') %>%
    mutate(across(everything(),~as.character(.)))
  
  # Cluster Robust Standard Error results
  ols_psyc_robust_errors <- pmap(ols_psyc_results_robust_input,
                                   cluster_robust_func) %>%
    set_names('lit','num','ef','sel')
  
  ############################## Exporting Results ###############################
  
  stargazer(ols_psyc_results,
            title="Multivariate OLS Regression: Psychological Investment Mechanism",
            dep.var.caption = "Endline Dependent Variable:",
            # covariate.labels=variables,
            column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
            se=lapply(ols_health_robust_errors, function(x) x$se),
            p=lapply(ols_health_robust_errors, function(x) x$p),
            # p=list(reduced_multivar_ols_region_robust_errors[['lit']][,4],reduced_multivar_ols_region_robust_errors[['num']][,4],reduced_multivar_ols_region_robust_errors[['ef']][,4],reduced_multivar_ols_region_robust_errors[['sel']][,4]),
            star.cutoffs = c(.05, .01, NA),
            notes.append     = FALSE,
            notes            = "*$p<0.05$; **$p<0.01$",
            omit=c('region_north_east','region_northern','region_upper_east','region_upper_west','treatment'),
            out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_reg/04_mech_controls/{folder}/04_reg_psyc.html"))
