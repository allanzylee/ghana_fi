###################################### Introduction ############################################

# Author: Allan Lee
# Date: December 29th, 2023
# Purpose: 2SLS IV Results

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
library(ivreg)
# library(GGally)
# library(broom.helpers)
# library(jtools)
# library(janitor)
library(dataCompareR)
library(broom)
library(estimatr)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')
full_data_l <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_l.rds')

################################# Define Functions ################################
# Education category terms
cat<-c('lit','num','ef','sel')
instruments<-'optimal_p_dev+optimal_t_dev'

# Define fs function
fs_func <- function(outcome, iv,cov){

  fm <- as.formula(paste(outcome,'~',iv,cov))
  
  reg <- lm(fm,
            data=full_data_w)
  return(reg)
}

# Define ss function
ss_func <- function(category, iv,cov){
  
  outcome<-paste0("e_",category,"_per")
  
  fm <- as.formula(paste(outcome,'~','e_ch_fs_dummy+e_cg_fs_dummy',cov,'|',cov,'+',iv))
  
  reg <- ivreg(fm,
            data=full_data_w
            # ,se_type = 'HC1'
            )
  return(reg)
}

# Define ss interaction function
ss_int_func <- function(category, iv,cov,int_term){
  
  outcome<-paste0("e_",category,"_per")
  
  fm <- as.formula(paste0(outcome,'~','e_ch_fs_dummy+e_cg_fs_dummy','+e_ch_fs_dummy*',int_term,'+e_cg_fs_dummy*',int_term,'+',cov,'|',cov,'+',iv))
  
  reg <- ivreg(fm,
               data=full_data_w
               # ,se_type = 'HC1'
  )
  return(reg)
}

################################### Base  #############################

# Define base first stage input
base_fs_input <- expand.grid(outcome=c('e_ch_fs_dummy','e_cg_fs_dummy'),
                             iv=c("optimal_p_dev+optimal_t_dev",
                                  "mean_p_dev+mean_t_dev",
                                  "seasonal_p_dev+seasonal_t_dev"),
                              cov=c(''))

# Regression results
base_fs_results<- pmap(base_fs_input,
                        fs_func) %>% 
  set_names('ch_optimal','cg_optimal',
            'ch_mean','cg_mean',
            'ch_seasonal','cg_seasonal')

summary(base_fs_results[['cg']])

# Define base second stage input
base_ss_input <- expand.grid(category=cat,
                             iv=instruments,
                             cov=c(''))

# Regression results
base_ss_results<- pmap(base_ss_input,
                       ss_func) %>% 
  set_names(cat)

summary(base_ss_results[['lit']],diagnostics = T)

################################### Check first stage F-statistic #############################

# Extract F-statistic results
f_stat_extract<-function(term){
  f_stat<-summary(base_fs_results[[term]])$fstatistic[1]
  return(f_stat)
}

f_stat<-pmap_dfr(
  tribble(~term,'ch_optimal','cg_optimal',
    'ch_mean','cg_mean',
    'ch_seasonal','cg_seasonal'),
  f_stat_extract
)

################################### Multivariate  #############################

# Define multivariate first stage input
multi_fs_input <- expand.grid(outcome=c('e_ch_fs_dummy','e_cg_fs_dummy'),
                             iv=c('optimal_p_dev+optimal_t_dev'),
                             cov=c('+female+age+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'))

# Regression results
multi_fs_results<- pmap(multi_fs_input,
                       fs_func) %>% 
  set_names('ch','cg')

summary(multi_fs_results[['ch']])

# Define base second stage input
multi_ss_input <- expand.grid(category=cat,
                             iv=c('optimal_p_dev+optimal_t_dev'),
                             cov=c('+female+age+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'))

# Regression results
multi_ss_results<- pmap(multi_ss_input,
                       ss_func) %>% 
  set_names(cat)

summary(multi_ss_results[['lit']],diagnostics = T)

#### Export

# FS
stargazer(multi_fs_results,
          title="Multivariate 2SLS: First Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/multivar_fs.html")


# SS
stargazer(multi_ss_results,
          title="Multivariate 2SLS: Second Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/multivar_ss.html")


################################### Multivariate GENDER #############################

# Define multivariate first stage input
gender_multi_fs_input <- expand.grid(outcome=c('e_ch_fs_dummy','e_cg_fs_dummy','e_ch_fs_dummy*female','e_cg_fs_dummy*female'),
                              iv=c('optimal_p_dev+optimal_t_dev+optimal_p_dev*optimal_t_dev+extreme_o_p_dev+extreme_o_t_dev'),
                              cov=c('+age+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'))

# Regression results
gender_multi_fs_results<- pmap(gender_multi_fs_input,
                        fs_func) %>% 
  set_names('ch','cg','ch_g','cg_g')

# Define base second stage input
gender_multi_ss_input <- expand.grid(category=cat,
                              iv=c('optimal_p_dev+optimal_t_dev+optimal_p_dev*optimal_t_dev+extreme_o_p_dev+extreme_o_t_dev'),
                              cov=c('+age+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'),
                              int_term=c('female'))

# Regression results
gender_multi_ss_results<- pmap(gender_multi_ss_input,
                        ss_int_func) %>% 
  set_names(cat)

summary(multi_ss_results[['lit']],diagnostics = T)

#### Export

# FS
stargazer(gender_multi_fs_results,
          title="Multivariate 2SLS with Gender Interaction Terms: First Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/gender_multivar_fs.html")


# SS
stargazer(gender_multi_ss_results,
          title="Multivariate 2SLS with Gender Interaction Terms: Second Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('age','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/gender_multivar_ss.html")

################################### Multivariate AGE #############################

# Define multivariate first stage input
age_multi_fs_input <- expand.grid(outcome=c('e_ch_fs_dummy','e_cg_fs_dummy','e_ch_fs_dummy*age','e_cg_fs_dummy*age'),
                                     iv=c('optimal_p_dev+optimal_t_dev+optimal_p_dev*optimal_t_dev+extreme_o_p_dev+extreme_o_t_dev'),
                                     cov=c('+female+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'))

# Regression results
age_multi_fs_results<- pmap(age_multi_fs_input,
                               fs_func) %>% 
  set_names('ch','cg','ch_a','cg_a')

# Define base second stage input
age_multi_ss_input <- expand.grid(category=cat,
                                     iv=c('optimal_p_dev+optimal_t_dev+optimal_p_dev*optimal_t_dev+extreme_o_p_dev+extreme_o_t_dev'),
                                     cov=c('+female+cg_age +cg_female+marital_status+cg_edu+num_kids+pe_pc1+treatment+language'),
                                     int_term=c('age'))

# Regression results
age_multi_ss_results<- pmap(age_multi_ss_input,
                               ss_int_func) %>% 
  set_names(cat)

summary(multi_ss_results[['lit']],diagnostics = T)

#### Export

# FS
stargazer(age_multi_fs_results,
          title="Multivariate 2SLS with Gender Interaction Terms: First Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/age_multivar_fs.html")


# SS
stargazer(age_multi_ss_results,
          title="Multivariate 2SLS with Gender Interaction Terms: Second Stage",
          dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          # column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
          omit=c('female','enrolled_in_school','current_class1','current_class2','current_class3','current_class4',
                 'current_class5','current_class6','current_class7','current_class8','current_class9','current_class10',
                 'current_class11','current_class12','current_class13','current_class14','private_school','num_books','cg_age','cg_female',
                 'marital_status','cg_edu','poverty','num_kids','pe_pc1',
                 'pe_pc2','pe_pc3','pe_pc4','treatment','languageDagbani','languageGruni',
                 'languageSissali','languageOther'),
          # se=list(multivar_ols_robust_errors[['lit']][,2],multivar_ols_robust_errors[['num']][,2],multivar_ols_robust_errors[['ef']][,2],multivar_ols_robust_errors[['sel']][,2]),
          # p=list(multivar_ols_robust_errors[['lit']][,4],multivar_ols_robust_errors[['num']][,4],multivar_ols_robust_errors[['ef']][,4],multivar_ols_robust_errors[['sel']][,4]),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/2sls_results/age_multivar_ss.html")

###########################################################################################
####################################### Base IV Regression ###############################
##########################################################################################

####################################### Define the Equation ##############################

child_first_stage <- "e_ch_fs_dummy~"
cg_first_stage <-"e_cg_fs_dummy~"
base_iv_instruments <- "mean_p_dev+mean_t_dev"
child_base_iv_model_eq <-paste(child_first_stage,base_iv_instruments
                               # ,"+m_edu"
                               )
cg_base_iv_model_eq <-paste(cg_first_stage,base_iv_instruments
                            # ,"+m_edu"
                            )
# 
# ####################################### First Stage ###############################
# 
# # Literacy
# lit_fs_pc1_1l_frongillo<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_fs_pc1_1l_frongillo)
# lit_fs_pc2_1l_frongillo<-lm(cg_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_fs_pc2_1l_frongillo)
# lit_fs_pc1_1l_frongillo_robust <- coeftest(lit_fs_pc1_1l_frongillo, vcov.=vcovHC(lit_fs_pc1_1l_frongillo,type="HC1"))
# lit_fs_pc2_1l_frongillo_robust <- coeftest(lit_fs_pc2_1l_frongillo, vcov.=vcovHC(lit_fs_pc2_1l_frongillo,type="HC1"))
# 
# 
# # Numeracy
# num_fs_pc1_1l_frongillo<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_fs_pc1_1l_frongillo)
# num_fs_pc2_1l_frongillo<-lm(cg_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_fs_pc2_1l_frongillo)
# num_fs_pc1_1l_frongillo_robust <- coeftest(num_fs_pc1_1l_frongillo, vcov.=vcovHC(num_fs_pc1_1l_frongillo,type="HC1"))
# num_fs_pc2_1l_frongillo_robust <- coeftest(num_fs_pc2_1l_frongillo, vcov.=vcovHC(num_fs_pc2_1l_frongillo,type="HC1"))
# 
# # EF
# ef_fs_pc1_1l_frongillo<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_fs_pc1_1l_frongillo)
# ef_fs_pc2_1l_frongillo<-lm(cg_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_fs_pc2_1l_frongillo)
# ef_fs_pc1_1l_frongillo_robust <- coeftest(ef_fs_pc1_1l_frongillo, vcov.=vcovHC(ef_fs_pc1_1l_frongillo,type="HC1"))
# ef_fs_pc2_1l_frongillo_robust <- coeftest(ef_fs_pc2_1l_frongillo, vcov.=vcovHC(ef_fs_pc2_1l_frongillo,type="HC1"))
# 
# # SEL
# sel_fs_pc1_1l_frongillo<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_fs_pc1_1l_frongillo)
# sel_fs_pc2_1l_frongillo<-lm(cg_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_fs_pc2_1l_frongillo)
# sel_fs_pc1_1l_frongillo_robust <- coeftest(sel_fs_pc1_1l_frongillo, vcov.=vcovHC(sel_fs_pc1_1l_frongillo,type="HC1"))
# sel_fs_pc2_1l_frongillo_robust <- coeftest(sel_fs_pc2_1l_frongillo, vcov.=vcovHC(sel_fs_pc2_1l_frongillo,type="HC1"))
# 
# first_stage_models_frongillo <- list(lit_fs_pc1_1l_frongillo,lit_fs_pc2_1l_frongillo,
#                            num_fs_pc1_1l_frongillo,num_fs_pc2_1l_frongillo,
#                            ef_fs_pc1_1l_frongillo,ef_fs_pc2_1l_frongillo,
#                            sel_fs_pc1_1l_frongillo,sel_fs_pc2_1l_frongillo)
# 
# stargazer(first_stage_models_frongillo,
#           title="Base 2SLS First Stage",
#           dep.var.caption = "Dependent Variable:",
#           covariate.labels=c("Deviance from Mean Precipitation","Deviance from Mean Temperature","Midline Education Outcome","Deviance Interaction Term","Constant"),
#           dep.var.labels = c("Literacy Child FI","Literacy Caregiver FI",
#                             "Numeracy Child FI","Numeracy Caregiver FI",
#                             "EF Child FI","EF Caregiver FI",
#                             "SEL Child FI","SEL Caregiver FI"),
#           se=list(lit_fs_pc1_1l_frongillo_robust[,2],num_fs_pc1_1l_frongillo_robust[,2],ef_fs_pc1_1l_frongillo_robust[,2],sel_fs_pc1_1l_frongillo_robust[,2], lit_fs_pc2_1l_frongillo_robust[,2],num_fs_pc2_1l_frongillo_robust[,2],ef_fs_pc2_1l_frongillo_robust[,2],sel_fs_pc2_1l_frongillo_robust[,2]),
#           p=list(lit_fs_pc1_1l_frongillo_robust[,4],num_fs_pc1_1l_frongillo_robust[,4],ef_fs_pc1_1l_frongillo_robust[,4],sel_fs_pc1_1l_frongillo_robust[,4], lit_fs_pc2_1l_frongillo_robust[,4],num_fs_pc2_1l_frongillo_robust[,4],ef_fs_pc2_1l_frongillo_robust[,4],sel_fs_pc2_1l_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_first_stage_frongillo.html")
# 
# ###### Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients #####
# lit_fs_pc1_1l_coef<-tidy(lit_fs_pc1_1l_frongillo) %>% 
#   mutate(category="Literacy & Child FI")
# num_fs_pc1_1l_coef<-tidy(num_fs_pc1_1l_frongillo) %>% 
#   mutate(category="Numeracy & Child FI")
# ef_fs_pc1_1l_coef<-tidy(ef_fs_pc1_1l_frongillo) %>% 
#   mutate(category="Executive Function & Child FI")
# sel_fs_pc1_1l_coef<-tidy(sel_fs_pc1_1l_frongillo) %>% 
#   mutate(category="SEL & Child FI")
# lit_fs_pc2_1l_coef<-tidy(lit_fs_pc2_1l_frongillo) %>% 
#   mutate(category="Literacy & Caregiver FI")
# num_fs_pc2_1l_coef<-tidy(num_fs_pc2_1l_frongillo) %>% 
#   mutate(category="Numeracy & Caregiver FI")
# ef_fs_pc2_1l_coef<-tidy(ef_fs_pc2_1l_frongillo) %>% 
#   mutate(category="Executive Function & Caregiver FI")
# sel_fs_pc2_1l_coef<-tidy(sel_fs_pc2_1l_frongillo) %>% 
#   mutate(category="SEL & Caregiver FI")
# 
# iv_1l_coef<-rbind(lit_fs_pc1_1l_coef,
#                   lit_fs_pc2_1l_coef,
#                   num_fs_pc1_1l_coef,
#                   num_fs_pc2_1l_coef,
#                   ef_fs_pc1_1l_coef,
#                   ef_fs_pc2_1l_coef,
#                   sel_fs_pc1_1l_coef,
#                   sel_fs_pc2_1l_coef) %>% 
#   mutate(term=str_replace(term,"mean_p_dev","Precip. Dev."),
#          term=str_replace(term,"mean_t_dev","Temp. Dev."))
# iv_1l_coef$category_f <- factor(iv_1l_coef$category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "SEL & Child FI",
#                                                             "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "SEL & Caregiver FI"))
# 
# # Create regression bar chart
# iv_1l_coef_plot<-ggplot(iv_1l_coef %>% 
#                          filter(term=="Precip. Dev."|term=="Temp. Dev."|term=="Precip. Dev.:Temp. Dev."), 
#                        aes(x=term, y=estimate)) + 
#   geom_bar(stat = "identity") +
#   xlab("Variables") + ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
#                     ymax=estimate + 1.96 * std.error), 
#                 size=.75, width=.3) +
#   facet_wrap(~category_f) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/iv_1l_bar_chart.png",
#        width=12,
#        height=7.13,
#        units="in")
# 
# ############################################ Second Stage #####################################
# 
# ########################################### Define the Equation ##############################
# 
# base_iv_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+m_edu |",base_iv_instruments,"+m_edu")
# 
# # Literacy
# lit_ivreg_base_frongillo<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_base_frongillo,vcov=sandwich,diagnostics=TRUE)
# lit_ivreg_base_frongillo_robust <- coeftest(lit_ivreg_base_frongillo, vcov.=vcovHC(lit_ivreg_base_frongillo,type="HC1"))
# 
# # Numeracy
# num_ivreg_base_frongillo<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_base_frongillo,vcov=sandwich,diagnostics=TRUE)
# num_ivreg_base_frongillo_robust <- coeftest(num_ivreg_base_frongillo, vcov.=vcovHC(num_ivreg_base_frongillo,type="HC1"))
# 
# # EF
# ef_ivreg_base_frongillo<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_base_frongillo,vcov=sandwich,diagnostics=TRUE)
# ef_ivreg_base_frongillo_robust <- coeftest(ef_ivreg_base_frongillo, vcov.=vcovHC(ef_ivreg_base_frongillo,type="HC1"))
# 
# # SEL
# sel_ivreg_base_frongillo<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_base_frongillo,vcov=sandwich,diagnostics=TRUE)
# sel_ivreg_base_frongillo_robust <- coeftest(sel_ivreg_base_frongillo, vcov.=vcovHC(sel_ivreg_base_frongillo,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# iv_frongillo <- list(lit_ivreg_base_frongillo,num_ivreg_base_frongillo,ef_ivreg_base_frongillo, sel_ivreg_base_frongillo)
# 
# stargazer(iv_frongillo,
#           title="Base 2SLS Second Stage",
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
#           se=list(lit_ivreg_base_frongillo_robust[,2],num_ivreg_base_frongillo_robust[,2],ef_ivreg_base_frongillo_robust[,2],sel_ivreg_base_frongillo_robust[,2]),
#           p=list(lit_ivreg_base_frongillo_robust[,4],num_ivreg_base_frongillo_robust[,4],ef_ivreg_base_frongillo_robust[,4],sel_ivreg_base_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_second_stage_frongillo.html")
# 
# # Export regression coefficients as bar charts. First, create data frame of all regression coefficients
# lit_2l_coef<-tidy(lit_ivreg_base_frongillo) %>% 
#   mutate(category="Literacy")
# num_2l_coef<-tidy(num_ivreg_base_frongillo) %>% 
#   mutate(category="Numeracy")
# ef_2l_coef<-tidy(ef_ivreg_base_frongillo) %>% 
#   mutate(category="Executive Function")
# sel_2l_coef<-tidy(sel_ivreg_base_frongillo) %>% 
#   mutate(category="SEL")
# 
# iv_2l_coef<-rbind(lit_2l_coef,num_2l_coef,ef_2l_coef,sel_2l_coef) %>% 
#   mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
#          term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
# iv_2l_coef$category_f <- factor(iv_2l_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "SEL"))
# 
# # Create regression bar chart
# iv_2l_coef<-ggplot(iv_2l_coef %>% 
#                          filter(term=="Child FI"|term=="Caregiver FI"), 
#                        aes(x=term, y=estimate)) + 
#   geom_bar(stat = "identity") +
#   xlab("Variables") + ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
#                     ymax=estimate + 1.96 * std.error), 
#                 size=.75, width=.3) +
#   facet_grid(~category_f) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/iv_2l_bar_chart.png",
#        width=9.26,
#        height=5.5,
#        units="in")

##########################################################################################
################################### Multivariate IV Regression ###########################
##########################################################################################

####################################### Define the Equation ##############################

cov_iv_instruments <- "seasonal_p_dev+seasonal_t_dev"
child_cov_iv_model_eq <-paste(child_first_stage,cov_iv_instruments,"+female+age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
cg_cov_iv_model_eq <-paste(cg_first_stage,cov_iv_instruments,"+female+age+enrolled_in_school +private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

####################################### First Stage ###############################

# Literacy
lit_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc1_1l_cov_frongillo)
lit_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc2_1l_cov_frongillo)
lit_fs_pc1_1l_cov_frongillo_robust <- coeftest(lit_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(lit_fs_pc1_1l_cov_frongillo,type="HC1"))
lit_fs_pc2_1l_cov_frongillo_robust <- coeftest(lit_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(lit_fs_pc2_1l_cov_frongillo,type="HC1"))

# Numeracy
num_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc1_1l_cov_frongillo)
num_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc2_1l_cov_frongillo)
num_fs_pc1_1l_cov_frongillo_robust <- coeftest(num_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(num_fs_pc1_1l_cov_frongillo,type="HC1"))
num_fs_pc2_1l_cov_frongillo_robust <- coeftest(num_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(num_fs_pc2_1l_cov_frongillo,type="HC1"))

# EF
ef_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc1_1l_cov_frongillo)
ef_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc2_1l_cov_frongillo)
ef_fs_pc1_1l_cov_frongillo_robust <- coeftest(ef_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(ef_fs_pc1_1l_cov_frongillo,type="HC1"))
ef_fs_pc2_1l_cov_frongillo_robust <- coeftest(ef_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(ef_fs_pc2_1l_cov_frongillo,type="HC1"))

# SEL
sel_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc1_1l_cov_frongillo)
sel_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc2_1l_cov_frongillo)
sel_fs_pc1_1l_cov_frongillo_robust <- coeftest(sel_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(sel_fs_pc1_1l_cov_frongillo,type="HC1"))
sel_fs_pc2_1l_cov_frongillo_robust <- coeftest(sel_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(sel_fs_pc2_1l_cov_frongillo,type="HC1"))

first_stage_models_cov_frongillo <- list(lit_fs_pc1_1l_cov_frongillo,lit_fs_pc2_1l_cov_frongillo,
                           num_fs_pc1_1l_cov_frongillo,num_fs_pc2_1l_cov_frongillo,
                           ef_fs_pc1_1l_cov_frongillo,ef_fs_pc2_1l_cov_frongillo,
                           sel_fs_pc1_1l_cov_frongillo,sel_fs_pc2_1l_cov_frongillo)

stargazer(first_stage_models_cov_frongillo,
          title="Multivariate 2SLS First Stage",
          dep.var.caption = "Dependent Variable:",
          # covariate.labels=iv_variables_cov_1l,
          dep.var.labels = c("Literacy Child FI","Literacy Caregiver FI",
                             "Numeracy Child FI","Numeracy Caregiver FI",
                             "EF Child FI","EF Caregiver FI",
                             "SEL Child FI","SEL Caregiver FI"),
          # omit=omitted_variables,
          se=list(lit_fs_pc1_1l_cov_frongillo_robust[,2],num_fs_pc1_1l_cov_frongillo_robust[,2],ef_fs_pc1_1l_cov_frongillo_robust[,2],sel_fs_pc1_1l_cov_frongillo_robust[,2], lit_fs_pc2_1l_cov_frongillo_robust[,2],num_fs_pc2_1l_cov_frongillo_robust[,2],ef_fs_pc2_1l_cov_frongillo_robust[,2],sel_fs_pc2_1l_cov_frongillo_robust[,2]),
          p=list(lit_fs_pc1_1l_cov_frongillo_robust[,4],num_fs_pc1_1l_cov_frongillo_robust[,4],ef_fs_pc1_1l_cov_frongillo_robust[,4],sel_fs_pc1_1l_cov_frongillo_robust[,4], lit_fs_pc2_1l_cov_frongillo_robust[,4],num_fs_pc2_1l_cov_frongillo_robust[,4],ef_fs_pc2_1l_cov_frongillo_robust[,4],sel_fs_pc2_1l_cov_frongillo_robust[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/first_stage_seasonal_iv.html")

# ##### Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients####
# lit_fs_pc1_1l_cov_coef<-tidy(lit_fs_pc1_1l_cov_frongillo) %>% 
#   mutate(category="Literacy & Child FI")
# num_fs_pc1_1l_cov_coef<-tidy(num_fs_pc1_1l_cov_frongillo) %>% 
#   mutate(category="Numeracy & Child FI")
# ef_fs_pc1_1l_cov_coef<-tidy(ef_fs_pc1_1l_cov_frongillo) %>% 
#   mutate(category="Executive Function & Child FI")
# sel_fs_pc1_1l_cov_coef<-tidy(sel_fs_pc1_1l_cov_frongillo) %>% 
#   mutate(category="SEL & Child FI")
# lit_fs_pc2_1l_cov_coef<-tidy(lit_fs_pc2_1l_cov_frongillo) %>% 
#   mutate(category="Literacy & Caregiver FI")
# num_fs_pc2_1l_cov_coef<-tidy(num_fs_pc2_1l_cov_frongillo) %>% 
#   mutate(category="Numeracy & Caregiver FI")
# ef_fs_pc2_1l_cov_coef<-tidy(ef_fs_pc2_1l_cov_frongillo) %>% 
#   mutate(category="Executive Function & Caregiver FI")
# sel_fs_pc2_1l_cov_coef<-tidy(sel_fs_pc2_1l_cov_frongillo) %>% 
#   mutate(category="SEL & Caregiver FI")
# 
# iv_1l_cov_coef<-rbind(lit_fs_pc1_1l_cov_coef,
#                   lit_fs_pc2_1l_cov_coef,
#                   num_fs_pc1_1l_cov_coef,
#                   num_fs_pc2_1l_cov_coef,
#                   ef_fs_pc1_1l_cov_coef,
#                   ef_fs_pc2_1l_cov_coef,
#                   sel_fs_pc1_1l_cov_coef,
#                   sel_fs_pc2_1l_cov_coef) %>% 
#   mutate(term=str_replace(term,"extreme_m_p_dev","Precip. Dev."),
#          term=str_replace(term,"extreme_m_t_dev","Temp. Dev."))
# iv_1l_cov_coef$category_f <- factor(iv_1l_cov_coef$category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "SEL & Child FI",
#                                                               "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "SEL & Caregiver FI"))
# 
# # Create regression bar chart
# iv_1l_cov_coef_plot<-ggplot(iv_1l_cov_coef %>% 
#                           filter(term=="Precip. Dev."|term=="Temp. Dev."|term=="Precip. Dev.:Temp. Dev."), 
#                         aes(x=term, y=estimate)) + 
#   geom_bar(stat = "identity") +
#   xlab("Variables") + ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
#                     ymax=estimate + 1.96 * std.error), 
#                 size=.75, width=.3) +
#   facet_wrap(~factor(category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "SEL & Child FI",
#                                         "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "SEL & Caregiver FI"))) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/iv_1l_cov_bar_chart.png",
#        width=12,
#        height=7.13,
#        units="in")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

cov_iv_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+female+age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language|",cov_iv_instruments,"+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

# Literacy
lit_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_full_cov_frongillo,diagnostics=TRUE)
lit_ivreg_full_cov_frongillo_robust <- coeftest(lit_ivreg_full_cov_frongillo, vcov.=vcovHC(lit_ivreg_full_cov_frongillo,type="HC1"))

# Numeracy
num_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_full_cov_frongillo,diagnostics=TRUE)
num_ivreg_full_cov_frongillo_robust <- coeftest(num_ivreg_full_cov_frongillo, vcov.=vcovHC(num_ivreg_full_cov_frongillo,type="HC1"))

# EF
ef_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_full_cov_frongillo,diagnostics=TRUE)
ef_ivreg_full_cov_frongillo_robust <- coeftest(ef_ivreg_full_cov_frongillo, vcov.=vcovHC(ef_ivreg_full_cov_frongillo,type="HC1"))

# SDQ
sel_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_full_cov_frongillo,diagnostics=TRUE)
sel_ivreg_full_cov_frongillo_robust <- coeftest(sel_ivreg_full_cov_frongillo, vcov.=vcovHC(sel_ivreg_full_cov_frongillo,type="HC1"))

############################## Exporting Results ###############################

iv_cov_frongillo <- list(lit_ivreg_full_cov_frongillo,num_ivreg_full_cov_frongillo,ef_ivreg_full_cov_frongillo, sel_ivreg_full_cov_frongillo)

stargazer(iv_cov_frongillo,
          title="Multivariate 2SLS Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
          # covariate.labels=variables[!variables %in% c('Parental Engagement PC2','Parental Engagement PC3','Parental Engagement PC4','Parental Engagement PC5')],
          # omit=omitted_variables,
          se=list(lit_ivreg_full_cov_frongillo_robust[,2],num_ivreg_full_cov_frongillo_robust[,2],ef_ivreg_full_cov_frongillo_robust[,2],sel_ivreg_full_cov_frongillo_robust[,2]),
          p=list(lit_ivreg_full_cov_frongillo_robust[,4],num_ivreg_full_cov_frongillo_robust[,4],ef_ivreg_full_cov_frongillo_robust[,4],sel_ivreg_full_cov_frongillo_robust[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/second_stage_seasonal_iv.html")

# # Export regression coefficients as bar charts. First, create data frame of all regression coefficients
# lit_2l_cov_coef<-tidy(lit_ivreg_full_cov_frongillo) %>% 
#   mutate(category="Literacy")
# num_2l_cov_coef<-tidy(num_ivreg_full_cov_frongillo) %>% 
#   mutate(category="Numeracy")
# ef_2l_cov_coef<-tidy(ef_ivreg_full_cov_frongillo) %>% 
#   mutate(category="Executive Function")
# sel_2l_cov_coef<-tidy(sel_ivreg_full_cov_frongillo) %>% 
#   mutate(category="SEL")
# 
# iv_2l_cov_coef<-rbind(lit_2l_cov_coef,num_2l_cov_coef,ef_2l_cov_coef,sel_2l_cov_coef) %>% 
#   mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
#          term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
# iv_2l_cov_coef$category_f <- factor(iv_2l_cov_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "SEL"))
# 
# # Create regression bar chart
# iv_2l_cov_coef<-ggplot(iv_2l_cov_coef %>% 
#                          filter(term=="Child FI"|term=="Caregiver FI"), 
#                        aes(x=term, y=estimate)) + 
#   geom_bar(stat = "identity") +
#   xlab("Variables") + ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
#                     ymax=estimate + 1.96 * std.error), 
#                 size=.75, width=.3) +
#   facet_grid(~category_f) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/iv_2l_cov_bar_chart.png",
#        width=9.26,
#        height=5.5,
#        units="in")

##########################################################################################
################################## Base IV Regression w/ Gender Interaction ##############
##########################################################################################
# Note that the IV interaction models will use e_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality

####################################### First Stage ###############################

########################################### Define the Equation ##############################

# base_iv_g_instruments <- "extreme_m_p_dev +optimal_p_dev+optimal_t_dev"
first_stage_int_ch<-"e_ch_fs_dummy~"
first_stage_g <-"female~"
first_stage_int_ch_g<-"e_ch_fs_dummy*female~"
first_stage_int_cg<-"e_cg_fs_dummy~"
first_stage_int_cg_g<-"e_cg_fs_dummy*female~"

# base_first_stage_int_model_eq <-paste(first_stage_int,base_iv_g_instruments,"+m_edu")
# base_first_stage_g_model_eq <-paste(first_stage_g,base_iv_g_instruments,"+m_edu")
# base_first_stage_int_g_model_eq <-paste(first_stage_int_g,base_iv_g_instruments,"+m_edu")
# 
# # Literacy
# lit_child_fs_1l_base_frongillo<-lm(base_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_frongillo)
# lit_child_fs_1l_base_g_frongillo<-lm(base_first_stage_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_g_frongillo)
# lit_child_fs_1l_base_int_g_frongillo<-lm(base_first_stage_int_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_int_g_frongillo)
# 
# lit_child_fs_1l_base_frongillo_robust <- coeftest(lit_child_fs_1l_base_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_frongillo,type="HC1"))
# lit_child_fs_1l_base_g_frongillo_robust <- coeftest(lit_child_fs_1l_base_g_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_g_frongillo,type="HC1"))
# lit_child_fs_1l_base_int_g_frongillo_robust <- coeftest(lit_child_fs_1l_base_int_g_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_int_g_frongillo,type="HC1"))
# 
# first_stage_models_base_int_g <- list(lit_child_fs_1l_base_frongillo,
#                                       lit_child_fs_1l_base_g_frongillo,
#                                       lit_child_fs_1l_base_int_g_frongillo)
# 
# stargazer(first_stage_models_base_int_g,
#           title="Base IV Regression with Gender Interaction: First stage",
#           dep.var.caption = "Dependent Variable:",
#           column.labels = c("Food Insecurity Dummy","Child Female",
#                              "Food Insecurity Dummy: Child Female"),
#           covariate.labels=c("Mean Precipitation Deviance Indicator","Optimal Precipitation Deviance","Optimal Temperature Deviance","Midline Education"),
#           se=list(lit_child_fs_1l_base_frongillo_robust[,2], lit_child_fs_1l_base_g_frongillo_robust[,2], lit_child_fs_1l_base_int_g_frongillo_robust[,2]),
#           p=list(lit_child_fs_1l_base_frongillo_robust[,4], lit_child_fs_1l_base_g_frongillo_robust[,4], lit_child_fs_1l_base_int_g_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_first_stage_g_frongillo.html")
# 
# ####################################### Second Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# base_iv_g_second_stage_model_eq <- paste("e_edu ~ e_fs_dummy+female+e_fs_dummy*female+ m_edu|",base_iv_g_instruments,"+m_edu")
# 
# # Literacy
# lit_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)
# lit_ivreg_base_int_g_robust <- coeftest(lit_ivreg_base_int_g, vcov.=vcovHC(lit_ivreg_base_int_g,type="HC1"))
# 
# # Numeracy
# num_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)
# num_ivreg_base_int_g_robust <- coeftest(num_ivreg_base_int_g, vcov.=vcovHC(num_ivreg_base_int_g,type="HC1"))
# 
# # EF
# ef_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)
# ef_ivreg_base_int_g_robust <- coeftest(ef_ivreg_base_int_g, vcov.=vcovHC(ef_ivreg_base_int_g,type="HC1"))
# 
# # SEL
# sel_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)
# sel_ivreg_base_int_g_robust <- coeftest(sel_ivreg_base_int_g, vcov.=vcovHC(sel_ivreg_base_int_g,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# second_stage_models_base_int_g <- list(lit_ivreg_base_int_g,num_ivreg_base_int_g,ef_ivreg_base_int_g,sel_ivreg_base_int_g)
# 
# stargazer(second_stage_models_base_int_g,
#           title="Base IV Regression with Gender Interaction: Second Stage",
#           type='latex',
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
#           covariate.labels=c("Food Insecurity","Child Female","Midline Education Outcome","Food Insecurity: Child Female"),
#           se=list(lit_ivreg_base_int_g_robust[,2],num_ivreg_base_int_g_robust[,2],ef_ivreg_base_int_g_robust[,2],sel_ivreg_base_int_g_robust[,2]),
#           p=list(lit_ivreg_base_int_g_robust[,4],num_ivreg_base_int_g_robust[,4],ef_ivreg_base_int_g_robust[,4],sel_ivreg_base_int_g_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_second_stage_g_frongillo.html")

##########################################################################################
################################## Multivariate IV Regression w/ Gender Interaction ###########################
##########################################################################################

####################################### First Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_g_instruments <- "seasonal_p_dev + seasonal_t_dev+seasonal_p_dev*seasonal_t_dev+extreme_s_p_dev+extreme_s_t_dev"
multivariate_first_stage_int_model_eq_ch <-paste(first_stage_int_ch,multivariate_iv_g_instruments,"+age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_g_model_eq <-paste(first_stage_g,multivariate_iv_g_instruments,"+age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_g_model_eq_ch <-paste(first_stage_int_ch_g,multivariate_iv_g_instruments,"+age+enrolled_in_school +private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_model_eq_cg <-paste(first_stage_int_cg,multivariate_iv_g_instruments,"+age+enrolled_in_school +private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_g_model_eq_cg <-paste(first_stage_int_cg_g,multivariate_iv_g_instruments,"+age+enrolled_in_school +private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

# Literacy 
lit_child_fs_1l_multivariate_frongillo_ch<-lm(multivariate_first_stage_int_model_eq_ch,data=full_data_l )
summary(lit_child_fs_1l_multivariate_frongillo_ch)
lit_child_fs_1l_multivariate_frongillo_cg<-lm(multivariate_first_stage_int_model_eq_cg,data=full_data_l )
summary(lit_child_fs_1l_multivariate_frongillo_cg)
lit_child_fs_1l_multivariate_g_frongillo<-lm(multivariate_first_stage_g_model_eq,data=full_data_l )
summary(lit_child_fs_1l_multivariate_g_frongillo)
lit_child_fs_1l_multivariate_int_g_frongillo_ch<-lm(multivariate_first_stage_int_g_model_eq_ch,data=full_data_l )
summary(lit_child_fs_1l_multivariate_int_g_frongillo_ch)
lit_child_fs_1l_multivariate_int_g_frongillo_cg<-lm(multivariate_first_stage_int_g_model_eq_cg,data=full_data_l )
summary(lit_child_fs_1l_multivariate_int_g_frongillo_cg)

lit_child_fs_1l_multivariate_frongillo_robust_ch <- coeftest(lit_child_fs_1l_multivariate_frongillo_ch, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo_ch,type="HC1"))
lit_child_fs_1l_multivariate_g_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_g_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_g_frongillo,type="HC1"))
lit_child_fs_1l_multivariate_int_g_frongillo_robust_ch <- coeftest(lit_child_fs_1l_multivariate_int_g_frongillo_ch, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_g_frongillo_ch,type="HC1"))
lit_child_fs_1l_multivariate_frongillo_robust_cg <- coeftest(lit_child_fs_1l_multivariate_frongillo_cg, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo_cg,type="HC1"))
lit_child_fs_1l_multivariate_int_g_frongillo_robust_cg <- coeftest(lit_child_fs_1l_multivariate_int_g_frongillo_cg, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_g_frongillo_cg,type="HC1"))

first_stage_models_multivariate_int_g <- list(lit_child_fs_1l_multivariate_frongillo_ch,
                                              lit_child_fs_1l_multivariate_frongillo_cg,
                                              lit_child_fs_1l_multivariate_g_frongillo,
                                              lit_child_fs_1l_multivariate_int_g_frongillo_ch,
                                              lit_child_fs_1l_multivariate_int_g_frongillo_cg)

stargazer(first_stage_models_multivariate_int_g,
          title="Multivariate IV Regression with Gender Interaction: First Stage",
          dep.var.caption = "Dependent Variable:",
          # covariate.labels=iv_variables_cov_1l_g,
          # dep.var.labels = c("Food Insecurity","Child Female",
          #                    "Food Insecurity: Child Female"),
          # omit=omitted_variables,
          se=list(lit_child_fs_1l_multivariate_frongillo_robust_ch[,2], lit_child_fs_1l_multivariate_frongillo_robust_cg[,2], lit_child_fs_1l_multivariate_g_frongillo_robust[,2], lit_child_fs_1l_multivariate_int_g_frongillo_robust_ch[,2],lit_child_fs_1l_multivariate_int_g_frongillo_robust_cg[,2]),
          p=list(lit_child_fs_1l_multivariate_frongillo_robust_ch[,4],lit_child_fs_1l_multivariate_frongillo_robust_cg[,4], lit_child_fs_1l_multivariate_g_frongillo_robust[,4], lit_child_fs_1l_multivariate_int_g_frongillo_robust_cg[,4],lit_child_fs_1l_multivariate_int_g_frongillo_robust_cg[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/gender_first_stage_seasonal_iv.html")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_g_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+female+e_ch_fs_dummy*female+e_cg_fs_dummy*female+",
                                                 "age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language",'|',
                                                 multivariate_iv_g_instruments,
                                                 "+age+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

# Literacy
lit_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_multivariate_int_g,diagnostics=TRUE)
lit_ivreg_multivariate_int_g_robust <- coeftest(lit_ivreg_multivariate_int_g, vcov.=vcovHC(lit_ivreg_multivariate_int_g,type="HC1"))

# Numeracy
num_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_multivariate_int_g,diagnostics=TRUE)
num_ivreg_multivariate_int_g_robust <- coeftest(num_ivreg_multivariate_int_g, vcov.=vcovHC(num_ivreg_multivariate_int_g,type="HC1"))

# EF
ef_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_multivariate_int_g,diagnostics=TRUE)
ef_ivreg_multivariate_int_g_robust <- coeftest(ef_ivreg_multivariate_int_g, vcov.=vcovHC(ef_ivreg_multivariate_int_g,type="HC1"))
# SEL
sel_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_multivariate_int_g,diagnostics=TRUE)
sel_ivreg_multivariate_int_g_robust <- coeftest(sel_ivreg_multivariate_int_g, vcov.=vcovHC(sel_ivreg_multivariate_int_g,type="HC1"))

############################## Exporting Results ###############################

second_stage_models_multivariate_int_g <- list(lit_ivreg_multivariate_int_g,num_ivreg_multivariate_int_g,ef_ivreg_multivariate_int_g,sel_ivreg_multivariate_int_g)

stargazer(second_stage_models_multivariate_int_g,
          title="Multivariate IV Regression with Gender Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
          # covariate.labels=iv_variables_cov_2l_g ,
          # omit=omitted_variables_g,
          se=list(lit_ivreg_multivariate_int_g_robust[,2],num_ivreg_multivariate_int_g_robust[,2],ef_ivreg_multivariate_int_g_robust[,2],sel_ivreg_multivariate_int_g_robust[,2]),
          p=list(lit_ivreg_multivariate_int_g_robust[,4],num_ivreg_multivariate_int_g_robust[,4],ef_ivreg_multivariate_int_g_robust[,4],sel_ivreg_multivariate_int_g_robust[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/gender_second_stage_seasonal_iv.html")

##########################################################################################
################################## Base IV Regression w/ Age Interaction ##############
##########################################################################################
# Note that the IV interaction models will use e_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality

####################################### First Stage ###############################

########################################### Define the Equation ##############################

# base_iv_a_instruments <- "extreme_m_p_dev + extreme_o_p_dev+optimal_t_dev"
first_stage_a <-"age~"
first_stage_int_ch_a<-"e_ch_fs_dummy*age~"
first_stage_int_cg_a<-"e_cg_fs_dummy*age~"
# base_first_stage_int_model_eq <-paste(first_stage_int,base_iv_a_instruments,"+m_edu")
# base_first_stage_a_model_eq <-paste(first_stage_a,base_iv_a_instruments,"+m_edu")
# base_first_stage_int_a_model_eq <-paste(first_stage_int_a,base_iv_a_instruments,"+m_edu")

# 
# # Literacy
# lit_child_fs_1l_base_frongillo<-lm(base_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_frongillo)
# lit_child_fs_1l_base_a_frongillo<-lm(base_first_stage_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_a_frongillo)
# lit_child_fs_1l_base_int_a_frongillo<-lm(base_first_stage_int_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_base_int_a_frongillo)
# 
# lit_child_fs_1l_base_frongillo_robust <- coeftest(lit_child_fs_1l_base_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_frongillo,type="HC1"))
# lit_child_fs_1l_base_a_frongillo_robust <- coeftest(lit_child_fs_1l_base_a_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_a_frongillo,type="HC1"))
# lit_child_fs_1l_base_int_a_frongillo_robust <- coeftest(lit_child_fs_1l_base_int_a_frongillo, vcov.=vcovHC(lit_child_fs_1l_base_int_a_frongillo,type="HC1"))
# 
# 
# first_stage_models_base_int_a <- list(lit_child_fs_1l_base_frongillo,
#                                       lit_child_fs_1l_base_a_frongillo,
#                                       lit_child_fs_1l_base_int_a_frongillo)
# 
# stargazer(first_stage_models_base_int_a,
#           title="Base IV Regression with Gender Interaction: First Stage",
#           dep.var.caption = "Dependent Variable:",
#           column.labels = c("Food Insecurity Dummy","Child Age",
#                             "Food Insecurity Dummy: Child Age"),
#           covariate.labels=c("Mean Precipitation Deviance Indicator","Optimal Precipitation Deviance Indicator","Optimal Temperature Deviance","Midline Education"),
#           se=list(lit_child_fs_1l_base_frongillo_robust[,2], lit_child_fs_1l_base_a_frongillo_robust[,2], lit_child_fs_1l_base_int_a_frongillo_robust[,2]),
#           p=list(lit_child_fs_1l_base_frongillo_robust[,4], lit_child_fs_1l_base_a_frongillo_robust[,4], lit_child_fs_1l_base_int_a_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_first_stage_a_frongillo.html")
# 
# ####################################### Second Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# base_iv_a_second_stage_model_eq <- paste("e_edu ~ e_fs_dummy+age+e_fs_dummy*age+ m_edu|",base_iv_a_instruments,"+m_edu")
# 
# # Literacy
# lit_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)
# lit_ivreg_base_int_a_robust <- coeftest(lit_ivreg_base_int_a, vcov.=vcovHC(lit_ivreg_base_int_a,type="HC1"))
# 
# # Numeracy
# num_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)
# num_ivreg_base_int_a_robust <- coeftest(num_ivreg_base_int_a, vcov.=vcovHC(num_ivreg_base_int_a,type="HC1"))
# 
# # EF
# ef_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)
# ef_ivreg_base_int_a_robust <- coeftest(ef_ivreg_base_int_a, vcov.=vcovHC(ef_ivreg_base_int_a,type="HC1"))
# 
# # SEL
# sel_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)
# sel_ivreg_base_int_a_robust <- coeftest(sel_ivreg_base_int_a, vcov.=vcovHC(sel_ivreg_base_int_a,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# second_stage_models_base_int_a <- list(lit_ivreg_base_int_a,num_ivreg_base_int_a,ef_ivreg_base_int_a,sel_ivreg_base_int_a)
# 
# stargazer(second_stage_models_base_int_a,
#           title="Base IV Regression with Age Interaction: Second Stage",
#           type='latex',
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
#           covariate.labels=c("Food Insecurity","Child Female","Midline Education Outcome","Food Insecurity: Child Female"),
#           se=list(lit_ivreg_base_int_a_robust[,2],num_ivreg_base_int_a_robust[,2],ef_ivreg_base_int_a_robust[,2],sel_ivreg_base_int_a_robust[,2]),
#           p=list(lit_ivreg_base_int_a_robust[,4],num_ivreg_base_int_a_robust[,4],ef_ivreg_base_int_a_robust[,4],sel_ivreg_base_int_a_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/base_second_stage_a_frongillo.html")
# 
# ##########################################################################################
# ################################## Multivariate IV Regression w/ Age Interaction ##############
# ##########################################################################################
# # Note that the IV interaction models will use e_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality
# 
# ####################################### First Stage ###############################
# 
# ########################################### Define the Equation ##############################

multivariate_iv_a_instruments <- "optimal_p_dev + optimal_t_dev+optimal_p_dev*optimal_t_dev+extreme_o_p_dev+extreme_o_t_dev"
multivariate_first_stage_int_model_eq_ch <-paste(first_stage_int_ch,multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_model_eq_cg <-paste(first_stage_int_cg,multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_a_model_eq <-paste(first_stage_a,multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_a_model_eq_ch <-paste(first_stage_int_ch_a,multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
multivariate_first_stage_int_a_model_eq_cg <-paste(first_stage_int_cg_a,multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

# Literacy
lit_child_fs_1l_multivariate_frongillo_ch<-lm(multivariate_first_stage_int_model_eq_ch,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_frongillo_ch)
lit_child_fs_1l_multivariate_a_frongillo<-lm(multivariate_first_stage_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_a_frongillo)
lit_child_fs_1l_multivariate_int_a_frongillo_ch<-lm(multivariate_first_stage_int_a_model_eq_ch,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_int_a_frongillo_ch)
lit_child_fs_1l_multivariate_frongillo_cg<-lm(multivariate_first_stage_int_model_eq_cg,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_frongillo_cg)
lit_child_fs_1l_multivariate_int_a_frongillo_cg<-lm(multivariate_first_stage_int_a_model_eq_cg,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_int_a_frongillo_cg)

lit_child_fs_1l_multivariate_frongillo_robust_ch <- coeftest(lit_child_fs_1l_multivariate_frongillo_ch, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo_ch,type="HC1"))
lit_child_fs_1l_multivariate_frongillo_robust_cg <- coeftest(lit_child_fs_1l_multivariate_frongillo_cg, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo_cg,type="HC1"))
lit_child_fs_1l_multivariate_a_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_a_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_a_frongillo,type="HC1"))
lit_child_fs_1l_multivariate_int_a_frongillo_robust_ch <- coeftest(lit_child_fs_1l_multivariate_int_a_frongillo_ch, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_a_frongillo_ch,type="HC1"))
lit_child_fs_1l_multivariate_int_a_frongillo_robust_cg <- coeftest(lit_child_fs_1l_multivariate_int_a_frongillo_cg, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_a_frongillo_cg,type="HC1"))

first_stage_models_multivariate_int_a <- list(lit_child_fs_1l_multivariate_frongillo_ch,
                                              lit_child_fs_1l_multivariate_frongillo_cg,
                                              lit_child_fs_1l_multivariate_a_frongillo,
                                              lit_child_fs_1l_multivariate_int_a_frongillo_ch,
                                              lit_child_fs_1l_multivariate_int_a_frongillo_cg)

stargazer(first_stage_models_multivariate_int_a,
          title="Multivariate IV Regression with Age Interaction: First Stage",
          dep.var.caption = "Dependent Variable:",
          # column.labels = c("Food Insecurity Dummy","Child Age",
          #                   "Food Insecurity Dummy: Child Age"),
          # covariate.labels=iv_variables_cov_1l_a,
          # omit=omitted_variables_a,
          se=list(lit_child_fs_1l_multivariate_frongillo_robust_ch[,2], lit_child_fs_1l_multivariate_frongillo_robust_cg[,2],lit_child_fs_1l_multivariate_a_frongillo_robust[,2], lit_child_fs_1l_multivariate_int_a_frongillo_robust_ch[,2],lit_child_fs_1l_multivariate_int_a_frongillo_robust_cg[,2]),
          p=list(lit_child_fs_1l_multivariate_frongillo_robust_ch[,4], lit_child_fs_1l_multivariate_frongillo_robust_cg[,4],lit_child_fs_1l_multivariate_a_frongillo_robust[,4], lit_child_fs_1l_multivariate_int_a_frongillo_robust_ch[,4],lit_child_fs_1l_multivariate_int_a_frongillo_robust_cg[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/age_first_stage_optimal_iv.html")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_a_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+age+e_ch_fs_dummy*age+e_cg_fs_dummy*age+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language|",multivariate_iv_a_instruments,"+female+enrolled_in_school + private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")

# Literacy
lit_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_multivariate_int_a,diagnostics=TRUE)
lit_ivreg_multivariate_int_a_robust <- coeftest(lit_ivreg_multivariate_int_a, vcov.=vcovHC(lit_ivreg_multivariate_int_a,type="HC1"))

# Numeracy
num_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_multivariate_int_a,diagnostics=TRUE)
num_ivreg_multivariate_int_a_robust <- coeftest(num_ivreg_multivariate_int_a, vcov.=vcovHC(num_ivreg_multivariate_int_a,type="HC1"))

# EF
ef_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_multivariate_int_a,diagnostics=TRUE)
ef_ivreg_multivariate_int_a_robust <- coeftest(ef_ivreg_multivariate_int_a, vcov.=vcovHC(ef_ivreg_multivariate_int_a,type="HC1"))

# SEL
sel_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_multivariate_int_a,diagnostics=TRUE)
sel_ivreg_multivariate_int_a_robust <- coeftest(sel_ivreg_multivariate_int_a, vcov.=vcovHC(sel_ivreg_multivariate_int_a,type="HC1"))

############################## Exporting Results ###############################

second_stage_models_multivariate_int_a <- list(lit_ivreg_multivariate_int_a,num_ivreg_multivariate_int_a,ef_ivreg_multivariate_int_a,sel_ivreg_multivariate_int_a)

stargazer(second_stage_models_multivariate_int_a,
          title="Multivariate IV Regression with Age Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
          # covariate.labels=iv_variables_cov_2l_a,
          # omit=omitted_variables_a,
          se=list(lit_ivreg_multivariate_int_a_robust[,2],num_ivreg_multivariate_int_a_robust[,2],ef_ivreg_multivariate_int_a_robust[,2],sel_ivreg_multivariate_int_a_robust[,2]),
          p=list(lit_ivreg_multivariate_int_a_robust[,4],num_ivreg_multivariate_int_a_robust[,4],ef_ivreg_multivariate_int_a_robust[,4],sel_ivreg_multivariate_int_a_robust[,4]),
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/12.08.2023_2sls_results_no_m_edu/age_second_stage_optimal_iv.html")

