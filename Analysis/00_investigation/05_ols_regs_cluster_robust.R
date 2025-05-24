###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 23, 2025
# Purpose: OLS Regressions Test

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

library(fixest)
library(dplyr)
library(stargazer)
library(AER)
library(dataCompareR)
library(broom)
library(xtable)
library(lfe)
library(purrr)
library(modelsummary)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

##########################################################################################
######################################## Regression Functions ############################
##########################################################################################

# Define base OLS functions
reg_func <- function(category, model){
  e_category_str<-paste0("e_",category,"_per")
  m_category_str<-paste0("m_",category,"_per")
  
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

##########################################################################################
############################## Multivariate OLS Regression w/ Region and PNP Treatment + Age and Gender ##############################
##########################################################################################

# Define base OLS input
va_ols_input_region <- expand.grid(category=c('lit'),
                                                 model=c('~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+'))

# Regression results
va_ols_region_results<- pmap(va_ols_input_region,
                                           reg_func) %>% 
  set_names('lit')


summary(va_ols_region_results[['lit']],
        cluster='careid')

# Define base OLS Robust input
va_ols_robust_region_input <- expand.grid(category=c('lit'),
                                                        results_str='va_ols_region_results') %>%
  mutate(across(everything(),~as.character(.)))

# Cluster Robust Standard Error results
va_ols_region_robust_errors <- pmap(va_ols_robust_region_input,
                                                  cluster_robust_func) %>%
  set_names('lit')

# Export results
stargazer(va_ols_region_results,
          title="Multivariate OLS Regression",
          #dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy"),
          # covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          # omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          # se=lapply(va_ols_region_robust_errors, function(x) x$se),
          p=lapply(va_ols_region_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/05_ols_regs_cluster_robust.html")

# Try FELM
felm_reg=felm(e_lit_per ~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+m_lit_per |0|0|careid,data=full_data_w)
summary(felm_reg)

stargazer(felm_reg,
          title="Multivariate OLS Regression",
          #dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy"),
          # covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          # omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          # # se=lapply(va_ols_region_robust_errors, function(x) x$se),
          # p=lapply(va_ols_region_robust_errors, function(x) x$p),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/05_ols_regs_cluster_robust.html")

# Try FEOLS
feols_reg=feols(e_lit_per ~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+m_lit_per, data=full_data_w, cluster=~careid)
feols_reg=feols(e_lit_per ~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+m_lit_per, data=full_data_w, cluster=~careid)
feols_reg=feols(e_lit_per ~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+m_lit_per, data=full_data_w, cluster=~careid)
feols_reg=feols(e_lit_per ~ e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+m_lit_per, data=full_data_w, cluster=~careid)

modelsummary(feols_reg,
          title="Multivariate OLS Regression",
          estimate = "{estimate}{stars}",
          #dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy"),
          # covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Lagged Outcome","Constant"),
          # omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
          #        'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
          #        'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
          #        'marital_status','cg_schooling','poverty','hh_size','pe_pc1',
          #        'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
          #        'languageSissali','languageOther','region_north_east','region_northern','region_upper_east','region_upper_west'),
          # # se=lapply(va_ols_region_robust_errors, function(x) x$se),
          # p=lapply(va_ols_region_robust_errors, function(x) x$p),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/05_ols_regs_cluster_robust.html")
