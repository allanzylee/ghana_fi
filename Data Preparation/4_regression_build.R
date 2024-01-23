###################################### Introduction ############################################

# Author: Allan Lee
# Date: December 29, 2023
# Purpose: Create build used for all regressions

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(foreign)
library(haven)
library(tidyverse)
library(stargazer)
library(psych)
library(corrr)
library(tibble)
library(writexl)
library(timechange)
library(rnoaa)
library(base)
library(arsenal)
library(labelled)
library(zoo)
library(AER)
library(GGally)
library(broom.helpers)
library(jtools)
library(janitor)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

outcome <- read_rds("build/outcome.rds")
treatment <- read_rds("build/treatment.rds")%>% 
  mutate(across(contains('id'),~as.double(.)))
cg_pe <- read_rds("build/cg_pe.rds") %>% 
  clean_names()
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(careid=as.double(caseid),
         childid=as.double(childid))
e_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid))
child_order <-read_dta("import/Child Order Dataset_12.15.22.dta") %>% 
  dplyr::select(-community, -region)
iv <- read_rds("build/iv.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))
e_household <- read_dta("import/01_PNP_Endline_HouseholdSurvey.dta")

##########################################################################################
########################## Calculate household size variable #############################
##########################################################################################
num_kids <- e_household %>% 
  select(careid, contains('c_ind'), contains('cr2_')) %>% 
  mutate(across(everything(),~as.double(str_trim(.))),
         num_kidstest = pmax(c_ind_1,
                          c_ind_2,
                          c_ind_3,
                          c_ind_4,
                          c_ind_5,
                          c_ind_6,
                          c_ind_7,
                          c_ind_8,
                          c_ind_9,
                          c_ind_10,
                          c_ind_11,
                          c_ind_12,
                          na.rm=T),
         careid=as.double(careid)) %>% 
  select(-contains('c_ind'))

##########################################################################################
################################## Putting all data together #############################
##########################################################################################

# Put all data together
full_data_w <- e_child %>% 
  dplyr::select(careid,
         childid,
         age=childage,
         current_class=ed3,
         private_school=ed2,
         language=io1
         ) %>% 
  # COME BACK HERE TO CHECK
  dplyr::right_join(e_cg %>% dplyr::select(careid, 
                                    childid, 
                                    enrolled_in_school=cr7,
                                    female=cr3,
                                    cg_age=cb1,
                                    cg_female=cb2,
                                    cg_edu=cb3,
                                    marital_status=cb5,
                                    num_books=pe7,
                                    treatment,
                                    contains('gb')
                                    ),
                    by=c("childid",'careid')) %>% 
  dplyr::left_join(outcome %>% dplyr::select(childid, 
                                      careid, 
                                      contains('per')),
                   by=c("childid","careid")) %>% 
  dplyr::left_join(cg_pe %>% dplyr::select(childid,
                                    careid,
                                    pe_pc1=pc1,
                                    pe_pc2=pc2,
                                    pe_pc3=pc3,
                                    pe_pc4=pc4,),
                   by=c("childid","careid")) %>% 
  dplyr::left_join(treatment,
                   by=c("childid","careid")) %>%
  dplyr::left_join(child_order %>% dplyr::select(childid,
                                          careid,
                                          poverty=mid_reverse_ppi,
                                          num_kids),
                   by=c("childid","careid")) %>%
  dplyr::left_join(iv,
                   by=c("careid","childid")) %>% 
  # Adjsut variables to become ordinal/binary
  mutate(female=if_else(female==1,0,1),
         cg_female=if_else(cg_female==1,0,1),
         private_school=if_else(private_school==1,0,1),
         marital_status=case_when(marital_status==3~1,
                                  marital_status==4~1,
                                  T~0),
         age=if_else((age>=5 & age <=9),0,1),
         cg_edu=case_when((cg_edu>1 & cg_edu<6)~1,
                          T~0),
         treatment=case_when(treatment>0 ~ 1,
                             T~0),
         current_class=as.double(current_class),
         language=case_when(language %in% c('Dagaari','Dagaari, Wali','English','TWI')~"Other",
                            T~language),
         current_class=as.factor(case_when(current_class<0~NA_real_,
                                 T~current_class))
         ) %>% 
  # Standardize outcome data
  mutate(m_sel_per=scale(m_sel_per)[,1],
         m_lit_per=scale(m_lit_per)[,1],
         m_ef_per=scale(m_ef_per)[,1],
         m_num_per=scale(m_num_per)[,1],
         e_sel_per=scale(e_sel_per)[,1],
         e_lit_per=scale(e_lit_per)[,1],
         e_ef_per=scale(e_ef_per)[,1],
         e_num_per=scale(e_num_per)[,1]) %>% 
  # Filter out NAs
  filter(!is.na(female),
         !is.na(age),
         !is.na(e_ch_fs_pc),
         !is.na(e_cg_fs_dummy)
         ) %>% 
  left_join(num_kids,by=c('careid'))

# Create long version of the data

full_data_l <- full_data_w %>% 
  # Pivot the data such that education columns only represent midline and endline education outcomes
  pivot_longer(cols=c('m_sel_per','m_lit_per','m_ef_per',"m_num_per"),
               names_to="m_outcome_type",
               values_to="m_edu") %>% 
  pivot_longer(cols=c('e_sel_per','e_lit_per','e_ef_per',"e_num_per"),
               names_to="e_outcome_type",
               values_to="e_edu") %>% 
  # Mutate the data such that education type columns are the same names. Then filter for rows with same education type
  mutate(m_outcome_type = substr(m_outcome_type, 3, nchar(m_outcome_type)),
         e_outcome_type = substr(e_outcome_type, 3, nchar(e_outcome_type))) %>% 
  filter(m_outcome_type==e_outcome_type)

# Export
saveRDS(full_data_w, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds")
saveRDS(full_data_l, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_l.rds")

# 
# #########################################################################################
# ######################################## Summary Statistics ##############################
# ##########################################################################################
# 
# # Create df with the child level demographics: PRIMARY
# summary_stat<-full_data_w %>% 
#   dplyr::dplyr::select(e_ch_fs_dummy, e_cg_fs_dummy, female,age,enrolled_in_school,current_class,private_school,num_books,cg_age,cg_female,cg_edu,marital_status,poverty,num_kids, treatment) %>% 
#   mutate(current_class=as.numeric(current_class)) %>% 
#   as.data.frame()
# 
# # Export the summary statistic table
# stargazer(summary_stat,
#           header=FALSE, 
#           type='latex',
#           title = "Child Demographics Summary Statistics",
#           covariate.labels=c("Child Food Insecurity","Caregiver Food Insecurity","Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School","Number of Books Child Owns",
#                              "Caregiver Age","Caregiver Female","Caregiver Education","Caregiver Marital Status","Poverty","Household Size","Participated in PNP Treatment Group")
#           )
# 
# ############################# Table the different languages that were used for child interviews###############################
# # lan_break_down<-table(full_data_w$language)/(nrow(full_data_w)-table(is.na(full_data_w$language))[2])
# # lan_break_down<-as.data.frame(lan_break_down)
# # write_xlsx(lan_break_down, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/lan_break_down.xlsx")
# 
# ############################# Table the percentage of families that experience "extreme" weather deviation ###############################
# extreme_weather_deviation <- iv %>% 
#   summarise(across(starts_with("extreme"), mean)) %>% 
#   rename("Optimal Precipitation Deviance Indicator"=extreme_o_p_dev,
#          "Optimal Temperature Deviance Indicator"=extreme_o_t_dev,
#          "Mean Precipitation Deviance Indicator"=extreme_m_p_dev,
#          "Mean Temperature Deviance Indicator"=extreme_m_t_dev,
#          "Regional Precipitation Deviance Indicator"=extreme_s_p_dev,
#          "Regional Temperature Deviance Indicator"=extreme_s_t_dev) %>% 
#   pivot_longer(cols=c(everything()),
#                names_to="Variable",
#                values_to="Larger than One SD") %>% 
#   mutate(`Larger than One SD`=round(`Larger than One SD`,digits=2),
#          `Smaller than One SD`=1-`Larger than One SD`)
#   
# stargazer(extreme_weather_deviation,
#           summary=FALSE,
#           rownames=FALSE)
# 
# ############################# Summary Statistics of Gender Bias ###############################
# gender_bias <- full_data_w %>% 
#   dplyr::select(childid,careid,contains("gb")) %>% 
#   dplyr::mutate_at(vars(gb1:gb15),~case_when(.==1~0,
#                                                T~1)) %>% 
#   dplyr::select(-childid,-careid) %>% 
#   as.data.frame()
# 
# names(gender_bias)<-toupper(names(gender_bias))
# stargazer(gender_bias)
# 
# ##########################################################################################
# ######################################## Base OLS Regression ############################
# ##########################################################################################
# 
# # Define equation
# ols_base_model_eq <- "e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+m_edu"
# 
# # OLS WITHOUT COVARIATES: PRIMARY
# 
# lit_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_test)
# lit_test_robust <- coeftest(lit_test, vcov.=vcovHC(lit_test,type="HC1"))
# 
# num_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_test)
# num_test_robust <- coeftest(num_test, vcov.=vcovHC(num_test,type="HC1"))
# 
# ef_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_test)
# ef_test_robust <- coeftest(ef_test, vcov.=vcovHC(ef_test,type="HC1"))
# 
# sel_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_test,robust=T)
# sel_test_robust <- coeftest(sel_test, vcov.=vcovHC(sel_test,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models <- list(lit_test,num_test,ef_test,sel_test)
# 
# stargazer(ols_models,
#           title="Base OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables[1:3],
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           # covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Midline Education Outcome","Constant"),
#           se=list(lit_test_robust[,2],num_test_robust[,2],ef_test_robust[,2],sel_test_robust[,2]),
#           p=list(lit_test_robust[,4],num_test_robust[,4],ef_test_robust[,4],sel_test_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_full_frongillo.html")
# 
# ##########################################################################################
# ############################## OLS Regression w/ Covariates ###############################
# ##########################################################################################
# 
# # Define equation
# ols_cov_model_eq <- "e_edu~e_ch_fs_dummy+e_cg_fs_dummy+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language"
# 
# ### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables
# 
# # Literacy
# lit_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_reg_full)
# lit_reg_full_robust <- coeftest(lit_reg_full, vcov.=vcovHC(lit_reg_full,type="HC1"))
# 
# # Numeracy
# num_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_reg_full)
# num_reg_full_robust <- coeftest(num_reg_full, vcov.=vcovHC(num_reg_full,type="HC1"))
# 
# # Executive Function
# ef_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_reg_full)
# ef_reg_full_robust <- coeftest(ef_reg_full, vcov.=vcovHC(ef_reg_full,type="HC1"))
# 
# # SEL
# sel_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_reg_full)
# sel_reg_full_robust <- coeftest(sel_reg_full, vcov.=vcovHC(sel_reg_full,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models_cov <- list(lit_reg_full,num_reg_full,ef_reg_full, sel_reg_full)
# 
# stargazer(ols_models_cov,
#           title="Multivariate OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=variables,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           # omit=omitted_variables,
#           se=list(lit_reg_full_robust[,2],num_reg_full_robust[,2],ef_reg_full_robust[,2],sel_reg_full_robust[,2]),
#           p=list(lit_reg_full_robust[,4],num_reg_full_robust[,4],ef_reg_full_robust[,4],sel_reg_full_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_full_cov_frongillo.html")
# 
# # Shortened table for paper
# 
# stargazer(ols_models_cov,
#           title="Multivariate OLS Regression",
#           dep.var.caption = "Endline Dependent Variable:",
#           # covariate.labels=c(variables[1:3],variables[38]),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL","Constant"),
#           omit=omitted_variables,
#           se=list(lit_reg_full_robust[,2],num_reg_full_robust[,2],ef_reg_full_robust[,2],sel_reg_full_robust[,2]),
#           p=list(lit_reg_full_robust[,4],num_reg_full_robust[,4],ef_reg_full_robust[,4],sel_reg_full_robust[,4]))
# 
# ###### Export regression coefficients as bar charts. First, create data frame of all regression coefficients #####
# lit_reg_full_coef<-tidy(lit_reg_full) %>% 
#   mutate(category="Literacy")
# num_reg_full_coef<-tidy(num_reg_full) %>% 
#   mutate(category="Numeracy")
# ef_reg_full_coef<-tidy(ef_reg_full) %>% 
#   mutate(category="Executive Function")
# sel_reg_full_coef<-tidy(sel_reg_full) %>% 
#   mutate(category="SEL")
# 
# full_coef<-rbind(lit_reg_full_coef,num_reg_full_coef,ef_reg_full_coef,sel_reg_full_coef) %>% 
#   mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
#          term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
# full_coef$category_f <- factor(full_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "SEL"))
# 
# # Create regression bar chart
# full_coef_plot<-ggplot(full_coef %>% 
#          filter(term=="Child FI"|term=="Caregiver FI"), 
#        aes(x=term, y=estimate)) + 
#   geom_bar(stat = "identity") +
#   xlab("Variables") + ylab("Estimate") +
#   geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
#                     ymax=estimate + 1.96 * std.error), 
#                 size=.75, width=.3) +
#   facet_grid(~category_f) +
#   theme_bw() +
#   theme(text=element_text(family="Times", size=12))
# 
# ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/ols_full_bar_chart.png",
#        width=9.26,
#        height=5.5,
#        units="in")
# 
# ##########################################################################################
# ############################## Base OLS Regression w/ Gender Interaction ###############################
# ##########################################################################################
# 
# # Define equation
# ols_base_g_model_eq <- "e_edu~e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*female+e_cg_fs_dummy*female+m_edu"
# 
# ### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables
# 
# # Literacy
# lit_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_reg_base_int_g)
# lit_reg_base_int_g_robust <- coeftest(lit_reg_base_int_g, vcov.=vcovHC(lit_reg_base_int_g,type="HC1"))
# 
# # Numeracy
# num_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_reg_base_int_g)
# num_reg_base_int_g_robust <- coeftest(num_reg_base_int_g, vcov.=vcovHC(num_reg_base_int_g,type="HC1"))
# 
# # Executive Function
# ef_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_reg_base_int_g)
# ef_reg_base_int_g_robust <- coeftest(ef_reg_base_int_g, vcov.=vcovHC(ef_reg_base_int_g,type="HC1"))
# 
# # Strength and Difficulties
# sel_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_reg_base_int_g)
# sel_reg_base_int_g_robust <- coeftest(sel_reg_base_int_g, vcov.=vcovHC(sel_reg_base_int_g,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models_base_int_g <- list(lit_reg_base_int_g,num_reg_base_int_g,ef_reg_base_int_g, sel_reg_base_int_g)
# 
# stargazer(ols_models_base_int_g,
#           title="Base OLS Regression with Gender Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=c("Child-Reported Food Insecurity","Caregiver-Reported Food Insecurity","Child Female","Midline Education Outcome","Child-Reported Food Insecurity: Child Female","Caregiver-Reported Food Insecurity: Child Female","Constant"),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           se=list(lit_reg_base_int_g_robust[,2],num_reg_base_int_g_robust[,2],ef_reg_base_int_g_robust[,2],sel_reg_base_int_g_robust[,2]),
#           p=list(lit_reg_base_int_g_robust[,4],num_reg_base_int_g_robust[,4],ef_reg_base_int_g_robust[,4],sel_reg_base_int_g_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_base_int_g_frongillo.html")
# 
# ##########################################################################################
# ############################## OLS Regression w/ Covariates AND Gender Interaction ###############################
# ##########################################################################################
# 
# # Define equation
# ols_cov_g_model_eq <- "e_edu~e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*female+e_cg_fs_dummy*female+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language"
# 
# ### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables
# 
# # Literacy
# lit_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_reg_full_int_g)
# lit_reg_full_int_g_robust <- coeftest(lit_reg_full_int_g, vcov.=vcovHC(lit_reg_full_int_g,type="HC1"))
# 
# # Numeracy
# num_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_reg_full_int_g)
# num_reg_full_int_g_robust <- coeftest(num_reg_full_int_g, vcov.=vcovHC(num_reg_full_int_g,type="HC1"))
# 
# # Executive Function
# ef_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_reg_full_int_g)
# ef_reg_full_int_g_robust <- coeftest(ef_reg_full_int_g, vcov.=vcovHC(ef_reg_full_int_g,type="HC1"))
# 
# # Strength and Difficulties
# sel_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_reg_full_int_g)
# sel_reg_full_int_g_robust <- coeftest(sel_reg_full_int_g, vcov.=vcovHC(sel_reg_full_int_g,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models_cov_int_g <- list(lit_reg_full_int_g,num_reg_full_int_g,ef_reg_full_int_g,sel_reg_full_int_g)
# 
# stargazer(ols_models_cov_int_g,
#           title="Multivariate OLS Regression with Gender Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=variables_g,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           # omit=omitted_variables_g,
#           se=list(lit_reg_full_int_g_robust[,2],num_reg_full_int_g_robust[,2],ef_reg_full_int_g_robust[,2],sel_reg_full_int_g_robust[,2]),
#           p=list(lit_reg_full_int_g_robust[,4],num_reg_full_int_g_robust[,4],ef_reg_full_int_g_robust[,4],sel_reg_full_int_g_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_full_int_g_frongillo.html")
# 
# # Shortened table for paper
# 
# stargazer(ols_models_cov_int_g,
#           title="Multivariate OLS Regression with Gender Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=c(variables_g[1:4],variables_g[38:40]),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           omit=omitted_variables_g,
#           se=list(lit_reg_full_int_g_robust[,2],num_reg_full_int_g_robust[,2],ef_reg_full_int_g_robust[,2],sel_reg_full_int_g_robust[,2]),
#           p=list(lit_reg_full_int_g_robust[,4],num_reg_full_int_g_robust[,4],ef_reg_full_int_g_robust[,4],sel_reg_full_int_g_robust[,4]))
# 
# 
# ##########################################################################################
# ############################## Base OLS Regression w/ Age Interaction ###############################
# ##########################################################################################
# 
# # Define equation
# ols_base_a_model_eq <- "e_edu~e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+m_edu"
# 
# ### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables
# 
# # Literacy
# lit_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_reg_base_int_a)
# lit_reg_base_int_a_robust <- coeftest(lit_reg_base_int_a, vcov.=vcovHC(lit_reg_base_int_a,type="HC1"))
# 
# # Numeracy
# num_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_reg_base_int_a)
# num_reg_base_int_a_robust <- coeftest(num_reg_base_int_a, vcov.=vcovHC(num_reg_base_int_a,type="HC1"))
# 
# # Executive Function
# ef_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_reg_base_int_a)
# ef_reg_base_int_a_robust <- coeftest(ef_reg_base_int_a, vcov.=vcovHC(ef_reg_base_int_a,type="HC1"))
# 
# # SEL
# sel_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_reg_base_int_a)
# sel_reg_base_int_a_robust <- coeftest(sel_reg_base_int_a, vcov.=vcovHC(sel_reg_base_int_a,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models_base_int_a <- list(lit_reg_base_int_a,num_reg_base_int_a,ef_reg_base_int_a, sel_reg_base_int_a)
# 
# stargazer(ols_models_base_int_a,
#           title="Base OLS Regression with Age Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=c("Child-Reported FI","Caregiver-Reported FI","Child Age","Midline Education Outcome","Child-Reported FI: Child Age","Caregiver-Reported FI: Child Age","Constant"),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           se=list(lit_reg_base_int_a_robust[,2],num_reg_base_int_a_robust[,2],ef_reg_base_int_a_robust[,2],sel_reg_base_int_a_robust[,2]),
#           p=list(lit_reg_base_int_a_robust[,4],num_reg_base_int_a_robust[,4],ef_reg_base_int_a_robust[,4],sel_reg_base_int_a_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_base_int_a_frongillo.html")
# 
# ##########################################################################################
# ############################## OLS Regression w/ Covariates AND Age Interaction ###############################
# ##########################################################################################
# 
# # Define equation
# ols_cov_a_model_eq <- "e_edu~e_ch_fs_dummy+e_cg_fs_dummy+e_ch_fs_dummy*age+e_cg_fs_dummy*age+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment+language"
# 
# ### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables
# 
# # Literacy
# lit_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_reg_full_int_a)
# lit_reg_full_int_a_robust <- coeftest(lit_reg_full_int_a, vcov.=vcovHC(lit_reg_full_int_a,type="HC1"))
# 
# # Numeracy
# num_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_reg_full_int_a)
# num_reg_full_int_a_robust <- coeftest(num_reg_full_int_a, vcov.=vcovHC(num_reg_full_int_a,type="HC1"))
# 
# # Executive Function
# ef_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_reg_full_int_a)
# ef_reg_full_int_a_robust <- coeftest(ef_reg_full_int_a, vcov.=vcovHC(ef_reg_full_int_a,type="HC1"))
# 
# # Strength and Difficulties
# sel_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_reg_full_int_a)
# sel_reg_full_int_a_robust <- coeftest(sel_reg_full_int_a, vcov.=vcovHC(sel_reg_full_int_a,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# ols_models_cov_int_a <- list(lit_reg_full_int_a,num_reg_full_int_a,ef_reg_full_int_a,sel_reg_full_int_a)
# 
# stargazer(ols_models_cov_int_a,
#           title="Multivariate OLS Regression with Age Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=variables_a,
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           # omit=omitted_variables_a,
#           se=list(lit_reg_full_int_a_robust[,2],num_reg_full_int_a_robust[,2],ef_reg_full_int_a_robust[,2],sel_reg_full_int_a_robust[,2]),
#           p=list(lit_reg_full_int_a_robust[,4],num_reg_full_int_a_robust[,4],ef_reg_full_int_a_robust[,4],sel_reg_full_int_a_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/OLS_full_int_a_frongillo.html")
# 
# # Shortened table for paper
# stargazer(ols_models_cov_int_a,
#           title="Multivariate OLS Regression with Gender Interaction",
#           dep.var.caption = "Endline Dependent Variable:",
#           covariate.labels=c(variables_a[1:4],variables_a[38:40]),
#           column.labels = c("Literacy","Numeracy","Executive Function","SEL"),
#           omit=omitted_variables_a,
#           se=list(lit_reg_full_int_a_robust[,2],num_reg_full_int_a_robust[,2],ef_reg_full_int_a_robust[,2],sel_reg_full_int_a_robust[,2]),
#           p=list(lit_reg_full_int_a_robust[,4],num_reg_full_int_a_robust[,4],ef_reg_full_int_a_robust[,4],sel_reg_full_int_a_robust[,4]))
# 
# ###########################################################################################
# ####################################### Base IV Regression ###############################
# ##########################################################################################
# 
# ####################################### Define the Equation ##############################
# 
# child_first_stage <- "e_ch_fs_dummy~"
# cg_first_stage <-"e_cg_fs_dummy~"
# base_iv_instruments <- "mean_p_dev+mean_t_dev+mean_p_dev*mean_t_dev"
# child_base_iv_model_eq <-paste(child_first_stage,base_iv_instruments,"+m_edu")
# cg_base_iv_model_eq <-paste(cg_first_stage,base_iv_instruments,"+m_edu")
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
# 
# ##########################################################################################
# ################################### Multivariate IV Regression ###########################
# ##########################################################################################
# 
# ####################################### Define the Equation ##############################
# 
# cov_iv_instruments <- "mean_p_dev+seasonal_t_dev+mean_p_dev*seasonal_t_dev"
# child_cov_iv_model_eq <-paste(child_first_stage,cov_iv_instruments,"+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# cg_cov_iv_model_eq <-paste(cg_first_stage,cov_iv_instruments,"+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# ####################################### First Stage ###############################
# 
# # Literacy
# lit_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_fs_pc1_1l_cov_frongillo)
# lit_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_fs_pc2_1l_cov_frongillo)
# lit_fs_pc1_1l_cov_frongillo_robust <- coeftest(lit_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(lit_fs_pc1_1l_cov_frongillo,type="HC1"))
# lit_fs_pc2_1l_cov_frongillo_robust <- coeftest(lit_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(lit_fs_pc2_1l_cov_frongillo,type="HC1"))
# 
# # Numeracy
# num_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_fs_pc1_1l_cov_frongillo)
# num_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
# summary(num_fs_pc2_1l_cov_frongillo)
# num_fs_pc1_1l_cov_frongillo_robust <- coeftest(num_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(num_fs_pc1_1l_cov_frongillo,type="HC1"))
# num_fs_pc2_1l_cov_frongillo_robust <- coeftest(num_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(num_fs_pc2_1l_cov_frongillo,type="HC1"))
# 
# # EF
# ef_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_fs_pc1_1l_cov_frongillo)
# ef_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
# summary(ef_fs_pc2_1l_cov_frongillo)
# ef_fs_pc1_1l_cov_frongillo_robust <- coeftest(ef_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(ef_fs_pc1_1l_cov_frongillo,type="HC1"))
# ef_fs_pc2_1l_cov_frongillo_robust <- coeftest(ef_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(ef_fs_pc2_1l_cov_frongillo,type="HC1"))
# 
# # SEL
# sel_fs_pc1_1l_cov_frongillo<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_fs_pc1_1l_cov_frongillo)
# sel_fs_pc2_1l_cov_frongillo<-lm(cg_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
# summary(sel_fs_pc2_1l_cov_frongillo)
# sel_fs_pc1_1l_cov_frongillo_robust <- coeftest(sel_fs_pc1_1l_cov_frongillo, vcov.=vcovHC(sel_fs_pc1_1l_cov_frongillo,type="HC1"))
# sel_fs_pc2_1l_cov_frongillo_robust <- coeftest(sel_fs_pc2_1l_cov_frongillo, vcov.=vcovHC(sel_fs_pc2_1l_cov_frongillo,type="HC1"))
# 
# first_stage_models_cov_frongillo <- list(lit_fs_pc1_1l_cov_frongillo,lit_fs_pc2_1l_cov_frongillo,
#                            num_fs_pc1_1l_cov_frongillo,num_fs_pc2_1l_cov_frongillo,
#                            ef_fs_pc1_1l_cov_frongillo,ef_fs_pc2_1l_cov_frongillo,
#                            sel_fs_pc1_1l_cov_frongillo,sel_fs_pc2_1l_cov_frongillo)
# 
# stargazer(first_stage_models_cov_frongillo,
#           title="Multivariate 2SLS First Stage",
#           dep.var.caption = "Dependent Variable:",
#           covariate.labels=iv_variables_cov_1l,
#           dep.var.labels = c("Literacy Child FI","Literacy Caregiver FI",
#                              "Numeracy Child FI","Numeracy Caregiver FI",
#                              "EF Child FI","EF Caregiver FI",
#                              "SEL Child FI","SEL Caregiver FI"),
#           # omit=omitted_variables,
#           se=list(lit_fs_pc1_1l_cov_frongillo_robust[,2],num_fs_pc1_1l_cov_frongillo_robust[,2],ef_fs_pc1_1l_cov_frongillo_robust[,2],sel_fs_pc1_1l_cov_frongillo_robust[,2], lit_fs_pc2_1l_cov_frongillo_robust[,2],num_fs_pc2_1l_cov_frongillo_robust[,2],ef_fs_pc2_1l_cov_frongillo_robust[,2],sel_fs_pc2_1l_cov_frongillo_robust[,2]),
#           p=list(lit_fs_pc1_1l_cov_frongillo_robust[,4],num_fs_pc1_1l_cov_frongillo_robust[,4],ef_fs_pc1_1l_cov_frongillo_robust[,4],sel_fs_pc1_1l_cov_frongillo_robust[,4], lit_fs_pc2_1l_cov_frongillo_robust[,4],num_fs_pc2_1l_cov_frongillo_robust[,4],ef_fs_pc2_1l_cov_frongillo_robust[,4],sel_fs_pc2_1l_cov_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_first_stage_cov_frongillo.html")
# 
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
# 
# ####################################### Second Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# cov_iv_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+e_cg_fs_dummy+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language|",cov_iv_instruments,"+m_edu+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# # Literacy
# lit_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_full_cov_frongillo,diagnostics=TRUE)
# lit_ivreg_full_cov_frongillo_robust <- coeftest(lit_ivreg_full_cov_frongillo, vcov.=vcovHC(lit_ivreg_full_cov_frongillo,type="HC1"))
# 
# # Numeracy
# num_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_full_cov_frongillo,diagnostics=TRUE)
# num_ivreg_full_cov_frongillo_robust <- coeftest(num_ivreg_full_cov_frongillo, vcov.=vcovHC(num_ivreg_full_cov_frongillo,type="HC1"))
# 
# # EF
# ef_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_full_cov_frongillo,diagnostics=TRUE)
# ef_ivreg_full_cov_frongillo_robust <- coeftest(ef_ivreg_full_cov_frongillo, vcov.=vcovHC(ef_ivreg_full_cov_frongillo,type="HC1"))
# 
# # SDQ
# sel_ivreg_full_cov_frongillo<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_full_cov_frongillo,diagnostics=TRUE)
# sel_ivreg_full_cov_frongillo_robust <- coeftest(sel_ivreg_full_cov_frongillo, vcov.=vcovHC(sel_ivreg_full_cov_frongillo,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# iv_cov_frongillo <- list(lit_ivreg_full_cov_frongillo,num_ivreg_full_cov_frongillo,ef_ivreg_full_cov_frongillo, sel_ivreg_full_cov_frongillo)
# 
# stargazer(iv_cov_frongillo,
#           title="Multivariate 2SLS Second Stage",
#           type='latex',
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
#           covariate.labels=variables[!variables %in% c('Parental Engagement PC2','Parental Engagement PC3','Parental Engagement PC4','Parental Engagement PC5')],
#           # omit=omitted_variables,
#           se=list(lit_ivreg_full_cov_frongillo_robust[,2],num_ivreg_full_cov_frongillo_robust[,2],ef_ivreg_full_cov_frongillo_robust[,2],sel_ivreg_full_cov_frongillo_robust[,2]),
#           p=list(lit_ivreg_full_cov_frongillo_robust[,4],num_ivreg_full_cov_frongillo_robust[,4],ef_ivreg_full_cov_frongillo_robust[,4],sel_ivreg_full_cov_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_second_stage_frongillo.html")
# 
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
# 
# ##########################################################################################
# ################################## Base IV Regression w/ Gender Interaction ##############
# ##########################################################################################
# # Note that the IV interaction models will use e_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality
# 
# ####################################### First Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# base_iv_g_instruments <- "extreme_m_p_dev +optimal_p_dev+optimal_t_dev"
# first_stage_int<-"e_fs_dummy~"
# first_stage_g <-"female~"
# first_stage_int_g<-"e_fs_dummy*female~"
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
# 
# ##########################################################################################
# ################################## Multivariate IV Regression w/ Gender Interaction ###########################
# ##########################################################################################
# 
# ####################################### First Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# multivariate_iv_g_instruments <- "mean_p_dev + optimal_t_dev+mean_p_dev * optimal_t_dev"
# multivariate_first_stage_int_model_eq <-paste(first_stage_int,multivariate_iv_g_instruments,"+m_edu+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# multivariate_first_stage_g_model_eq <-paste(first_stage_g,multivariate_iv_g_instruments,"+m_edu+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# multivariate_first_stage_int_g_model_eq <-paste(first_stage_int_g,multivariate_iv_g_instruments,"+m_edu+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# # Literacy 
# lit_child_fs_1l_multivariate_frongillo<-lm(multivariate_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_frongillo)
# lit_child_fs_1l_multivariate_g_frongillo<-lm(multivariate_first_stage_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_g_frongillo)
# lit_child_fs_1l_multivariate_int_g_frongillo<-lm(multivariate_first_stage_int_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_int_g_frongillo)
# 
# lit_child_fs_1l_multivariate_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo,type="HC1"))
# lit_child_fs_1l_multivariate_g_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_g_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_g_frongillo,type="HC1"))
# lit_child_fs_1l_multivariate_int_g_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_int_g_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_g_frongillo,type="HC1"))
# 
# first_stage_models_multivariate_int_g <- list(lit_child_fs_1l_multivariate_frongillo,
#                                               lit_child_fs_1l_multivariate_g_frongillo,
#                                               lit_child_fs_1l_multivariate_int_g_frongillo)
# 
# stargazer(first_stage_models_multivariate_int_g,
#           title="Multivariate IV Regression with Gender Interaction: First Stage",
#           dep.var.caption = "Dependent Variable:",
#           covariate.labels=iv_variables_cov_1l_g,
#           dep.var.labels = c("Food Insecurity","Child Female",
#                              "Food Insecurity: Child Female"),
#           # omit=omitted_variables,
#           se=list(lit_child_fs_1l_multivariate_frongillo_robust[,2], lit_child_fs_1l_multivariate_g_frongillo_robust[,2], lit_child_fs_1l_multivariate_int_g_frongillo_robust[,2]),
#           p=list(lit_child_fs_1l_multivariate_frongillo_robust[,4], lit_child_fs_1l_multivariate_g_frongillo_robust[,4], lit_child_fs_1l_multivariate_int_g_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_first_stage_g_frongillo.html")
# 
# ####################################### Second Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# multivariate_iv_g_second_stage_model_eq <- paste("e_edu ~ e_fs_dummy+female+e_fs_dummy*female+",
#                                                  "m_edu+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language",'|',
#                                                  multivariate_iv_g_instruments,
#                                                  "+m_edu+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# # Literacy
# lit_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_multivariate_int_g,diagnostics=TRUE)
# lit_ivreg_multivariate_int_g_robust <- coeftest(lit_ivreg_multivariate_int_g, vcov.=vcovHC(lit_ivreg_multivariate_int_g,type="HC1"))
# 
# # Numeracy
# num_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_multivariate_int_g,diagnostics=TRUE)
# num_ivreg_multivariate_int_g_robust <- coeftest(num_ivreg_multivariate_int_g, vcov.=vcovHC(num_ivreg_multivariate_int_g,type="HC1"))
# 
# # EF
# ef_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_multivariate_int_g,diagnostics=TRUE)
# ef_ivreg_multivariate_int_g_robust <- coeftest(ef_ivreg_multivariate_int_g, vcov.=vcovHC(ef_ivreg_multivariate_int_g,type="HC1"))
# # SEL
# sel_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_multivariate_int_g,diagnostics=TRUE)
# sel_ivreg_multivariate_int_g_robust <- coeftest(sel_ivreg_multivariate_int_g, vcov.=vcovHC(sel_ivreg_multivariate_int_g,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# second_stage_models_multivariate_int_g <- list(lit_ivreg_multivariate_int_g,num_ivreg_multivariate_int_g,ef_ivreg_multivariate_int_g,sel_ivreg_multivariate_int_g)
# 
# stargazer(second_stage_models_multivariate_int_g,
#           title="Multivariate IV Regression with Gender Interaction: Second Stage",
#           type='latex',
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
#           covariate.labels=iv_variables_cov_2l_g ,
#           # omit=omitted_variables_g,
#           se=list(lit_ivreg_multivariate_int_g_robust[,2],num_ivreg_multivariate_int_g_robust[,2],ef_ivreg_multivariate_int_g_robust[,2],sel_ivreg_multivariate_int_g_robust[,2]),
#           p=list(lit_ivreg_multivariate_int_g_robust[,4],num_ivreg_multivariate_int_g_robust[,4],ef_ivreg_multivariate_int_g_robust[,4],sel_ivreg_multivariate_int_g_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_second_stage_g_frongillo.html")
# 
# ##########################################################################################
# ################################## Base IV Regression w/ Age Interaction ##############
# ##########################################################################################
# # Note that the IV interaction models will use e_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality
# 
# ####################################### First Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# base_iv_a_instruments <- "extreme_m_p_dev + extreme_o_p_dev+optimal_t_dev"
# first_stage_a <-"age~"
# first_stage_int_a<-"e_fs_dummy*age~"
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
# 
# multivariate_iv_a_instruments <- "extreme_m_t_dev + extreme_o_t_dev+optimal_t_dev"
# multivariate_first_stage_int_model_eq <-paste(first_stage_int,multivariate_iv_a_instruments,"+m_edu+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# multivariate_first_stage_a_model_eq <-paste(first_stage_a,multivariate_iv_a_instruments,"+m_edu+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# multivariate_first_stage_int_a_model_eq <-paste(first_stage_int_a,multivariate_iv_a_instruments,"+m_edu+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# # Literacy
# lit_child_fs_1l_multivariate_frongillo<-lm(multivariate_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_frongillo)
# lit_child_fs_1l_multivariate_a_frongillo<-lm(multivariate_first_stage_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_a_frongillo)
# lit_child_fs_1l_multivariate_int_a_frongillo<-lm(multivariate_first_stage_int_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
# summary(lit_child_fs_1l_multivariate_int_a_frongillo)
# 
# lit_child_fs_1l_multivariate_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_frongillo,type="HC1"))
# lit_child_fs_1l_multivariate_a_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_a_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_a_frongillo,type="HC1"))
# lit_child_fs_1l_multivariate_int_a_frongillo_robust <- coeftest(lit_child_fs_1l_multivariate_int_a_frongillo, vcov.=vcovHC(lit_child_fs_1l_multivariate_int_a_frongillo,type="HC1"))
# 
# first_stage_models_multivariate_int_a <- list(lit_child_fs_1l_multivariate_frongillo,
#                                               lit_child_fs_1l_multivariate_a_frongillo,
#                                               lit_child_fs_1l_multivariate_int_a_frongillo)
# 
# stargazer(first_stage_models_multivariate_int_a,
#           title="Multivariate IV Regression with Age Interaction: First Stage",
#           dep.var.caption = "Dependent Variable:",
#           column.labels = c("Food Insecurity Dummy","Child Age",
#                             "Food Insecurity Dummy: Child Age"),
#           covariate.labels=iv_variables_cov_1l_a,
#           # omit=omitted_variables_a,
#           se=list(lit_child_fs_1l_multivariate_frongillo_robust[,2], lit_child_fs_1l_multivariate_a_frongillo_robust[,2], lit_child_fs_1l_multivariate_int_a_frongillo_robust[,2]),
#           p=list(lit_child_fs_1l_multivariate_frongillo_robust[,4], lit_child_fs_1l_multivariate_a_frongillo_robust[,4], lit_child_fs_1l_multivariate_int_a_frongillo_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_first_stage_a_frongillo.html")
# 
# ####################################### Second Stage ###############################
# 
# ########################################### Define the Equation ##############################
# 
# multivariate_iv_a_second_stage_model_eq <- paste("e_edu ~ e_fs_dummy+age+e_fs_dummy*age+m_edu+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language|",multivariate_iv_a_instruments,"+m_edu+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment+language")
# 
# # Literacy
# lit_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
# summary(lit_ivreg_multivariate_int_a,diagnostics=TRUE)
# lit_ivreg_multivariate_int_a_robust <- coeftest(lit_ivreg_multivariate_int_a, vcov.=vcovHC(lit_ivreg_multivariate_int_a,type="HC1"))
# 
# # Numeracy
# num_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
# summary(num_ivreg_multivariate_int_a,diagnostics=TRUE)
# num_ivreg_multivariate_int_a_robust <- coeftest(num_ivreg_multivariate_int_a, vcov.=vcovHC(num_ivreg_multivariate_int_a,type="HC1"))
# 
# # EF
# ef_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
# summary(ef_ivreg_multivariate_int_a,diagnostics=TRUE)
# ef_ivreg_multivariate_int_a_robust <- coeftest(ef_ivreg_multivariate_int_a, vcov.=vcovHC(ef_ivreg_multivariate_int_a,type="HC1"))
# 
# # SEL
# sel_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
# summary(sel_ivreg_multivariate_int_a,diagnostics=TRUE)
# sel_ivreg_multivariate_int_a_robust <- coeftest(sel_ivreg_multivariate_int_a, vcov.=vcovHC(sel_ivreg_multivariate_int_a,type="HC1"))
# 
# ############################## Exporting Results ###############################
# 
# second_stage_models_multivariate_int_a <- list(lit_ivreg_multivariate_int_a,num_ivreg_multivariate_int_a,ef_ivreg_multivariate_int_a,sel_ivreg_multivariate_int_a)
# 
# stargazer(second_stage_models_multivariate_int_a,
#           title="Multivariate IV Regression with Age Interaction: Second Stage",
#           type='latex',
#           dep.var.caption = "Endline Dependent Variable:",
#           column.labels = c("Literacy","Numeracy", "Executive Function","SEL"),
#           covariate.labels=iv_variables_cov_2l_a,
#           # omit=omitted_variables_a,
#           se=list(lit_ivreg_multivariate_int_a_robust[,2],num_ivreg_multivariate_int_a_robust[,2],ef_ivreg_multivariate_int_a_robust[,2],sel_ivreg_multivariate_int_a_robust[,2]),
#           p=list(lit_ivreg_multivariate_int_a_robust[,4],num_ivreg_multivariate_int_a_robust[,4],ef_ivreg_multivariate_int_a_robust[,4],sel_ivreg_multivariate_int_a_robust[,4]),
#           out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/results_with_frongillo_code/multivariate_second_stage_a_frongillo.html")

