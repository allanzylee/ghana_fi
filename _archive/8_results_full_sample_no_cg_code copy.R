###################################### Introduction ############################################

# Author: Allan Lee
# Date: Feb 15th, 2023
# Purpose: Combine all data together, conduct summary statistics, and run the regressions, USING FRONGILLO CODED FI DATA

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Penn/ECON4900/Data")

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

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

outcome <- read_csv("outcome.csv")
treatment <- read_csv("treatment.csv")
cg_pe <- read_csv("cg_pe.csv")
e_child <- read_dta("Midline/data/03_PNP_Midline_ChildSurvey.dta")
m_house<- read_dta("Midline/data/01_PNP_Midline_HouseholdSurvey.dta")
e_cg <- read_dta("Midline/data/02_PNP_Midline_CaregiverSurvey.dta")
child_order <-read_dta("Child Order Dataset_12.15.22.dta") %>% 
  dplyr::select(-community, -region)
iv <- read_csv("iv.csv")
drought_index_iv<-read_csv("drought_index_iv.csv")

##########################################################################################
################################## Putting all data together #############################
##########################################################################################

# Convert relevant childid and careid variables to numeric
e_child$careid<-as.numeric(e_child$careid)
e_child$childid<-as.numeric(e_child$childid)
e_cg$careid<-as.numeric(e_cg$careid)
e_cg$childid<-as.numeric(e_cg$childid)

# Put all data together
full_data_w <- e_child %>% 
  dplyr::right_join(e_cg,by=c("childid","careid","region","district","community")) %>% 
  dplyr::left_join(outcome,by=c("childid","careid")) %>% 
  dplyr::left_join(cg_pe,by=c("childid","careid")) %>% 
  dplyr::left_join(treatment,by=c("childid","careid")) %>%
  dplyr::left_join(child_order,by=c("childid","careid")) %>%
  dplyr:: left_join(iv,by=c("careid","childid","region","district")) %>% 
  dplyr::left_join(drought_index_iv,by=c("childid")) %>% 
  dplyr::select(region,district,community,careid,childid,cr3.y,cr5_2.y,cr6.x,cr7.x,ed2,cb1,cb2,cb3,cb5,avg_yield,optimal_p_dev,optimal_t_dev,mean_p_dev,mean_t_dev,contains("extreme"),contains("_w"),
                m_sel_per,m_lit_per,m_ef_per,m_num_per,e_sel_per,e_lit_per,e_ef_per,e_num_per,contains("fs_pc"),contains("fs_dummy"),PC1,PC2,PC3,PC4,home_scale,mid_reverse_ppi,num_kids,io1,ed3,pe7,treatment) %>% 
  # Correct for missing data for one person
  mutate(region=ifelse(careid==11117814,"NORTHERN",region)) %>% 
  mutate(cr3.y=if_else(cr3.y==1,0,1),
         cb2=if_else(cb2==1,0,1),
         ed2=if_else(ed2==1,0,1),
         cb5=if_else(cb5==3,1,
                     if_else(cb5==4,1,0)),
         region=case_when(region=="NORTH EAST"~1,
                          region=="NORTHERN"~2,
                          region=="SAVANNAH"~3,
                          region=="UPPER EAST"~4,
                          region=="UPPER WEST"~5,
                          TRUE ~0),
         region=ifelse(region==0,NA,region),
         region=as.numeric(region)) %>% 
  rename(female=cr3.y,
         month_of_birth=cr5_2.y,
         age=cr6.x,
         enrolled_in_school=cr7.x,
         private_school=ed2,
         cg_age=cb1,
         cg_female=cb2,
         cg_edu=cb3,
         marital_status=cb5,
         poverty=mid_reverse_ppi,
         pe_pc1=PC1,
         pe_pc2=PC2,
         pe_pc3=PC3,
         pe_pc4=PC4,
         language=io1,
         current_class=ed3,
         num_books=pe7) %>% 
  # Standardize outcome data
  mutate(m_sel_per=scale(m_sel_per)[,1],
         m_lit_per=scale(m_lit_per)[,1],
         m_ef_per=scale(m_ef_per)[,1],
         m_num_per=scale(m_num_per)[,1],
         e_sel_per=scale(e_sel_per)[,1],
         e_lit_per=scale(e_lit_per)[,1],
         e_ef_per=scale(e_ef_per)[,1],
         e_num_per=scale(e_num_per)[,1]) %>% 
  # Create a combined food insecurity variable
  mutate(e_fs_dummy=e_ch_fs_dummy+e_cg_fs_dummy) %>% 
  # Deselect the midline food insecurity variables
  dplyr::select(-m_ch_fs_pc,-m_cg_fs_pc,-starts_with("m_fs_pc"),-m_ch_fs_dummy,-m_cg_fs_dummy) %>% 
  # Filter out NAs
  filter(!is.na(female),!is.na(age),!is.na(e_fs_dummy)) %>% 
  # Make age into age groups
  mutate(age=if_else((age>=5 & age <=9),1,0))

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

#########################################################################################
######################################## Summary Statistics ##############################
##########################################################################################

# Create df with the child level demographics: PRIMARY
summary_stat<-full_data_w %>% 
  dplyr::select(female,age,enrolled_in_school,current_class,private_school,num_books,cg_age,cg_female,cg_edu,marital_status,poverty,num_kids) %>% 
  as.data.frame()

# Export the summary statistic table
stargazer(summary_stat,
          header=FALSE, 
          type='latex',
          title = "Child Demographics Summary Statistics",
          covariate.labels=c("Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School","Number of Books Child Owns",
                             "Caregiver Age","Caregiver Female","Caregiver Education","Caregiver Marital Status","Poverty","Household Size"))

################################ Table the different languages that were used for child interviews###############################
lan_break_down<-table(full_data_w$language)/(nrow(full_data_w)-table(is.na(full_data_w$language))[2])
lan_break_down<-as.data.frame(lan_break_down)
write_xlsx(lan_break_down, "/Users/AllanLee/Desktop/Penn/ECON4900/Data/lan_break_down.xlsx")

##########################################################################################
######################################## Base OLS Regression ############################
##########################################################################################

# Define equation
ols_base_model_eq <- "e_edu ~ e_ch_fs_dummy+m_edu+pe_pc1"

# OLS WITHOUT COVARIATES: PRIMARY

lit_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_test)

num_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_test)

ef_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_test)

sel_test <- lm(ols_base_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_test)

############################## Exporting Results ###############################

ols_models <- list(lit_test,num_test,ef_test,sel_test)

stargazer(ols_models,
          title="Base OLS Regression",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          #covariate.labels=c("Child Reported Food Insecurity","Midline Education Outcome","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_full_child_only.html")

##########################################################################################
############################## OLS Regression w/ Covariates ###############################
##########################################################################################

# Define equation
ols_cov_model_eq <- "e_edu~e_ch_fs_dummy+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment"

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_full)

# Numeracy
num_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_full)

# Executive Function
ef_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_full)

# SEL
sel_reg_full<-lm(ols_cov_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_full)

############################## Exporting Results ###############################

ols_models_cov <- list(lit_reg_full,num_reg_full,ef_reg_full, sel_reg_full)

stargazer(ols_models_cov,
          title="Multivariate OLS Regression",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Child Reported Food Insecurity","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement: PC1","Parental Engagement: PC2","Parental Engagement: PC3","Parental Engagement: PC4","Treatment"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_full_cov_child_only.html")

###### Export regression coefficients as bar charts. First, create data frame of all regression coefficients #####
lit_reg_full_coef<-tidy(lit_reg_full) %>% 
  mutate(category="Literacy")
num_reg_full_coef<-tidy(num_reg_full) %>% 
  mutate(category="Numeracy")
ef_reg_full_coef<-tidy(ef_reg_full) %>% 
  mutate(category="Executive Function")
sel_reg_full_coef<-tidy(sel_reg_full) %>% 
  mutate(category="Socioemotional Learning")

full_coef<-rbind(lit_reg_full_coef,num_reg_full_coef,ef_reg_full_coef,sel_reg_full_coef) %>% 
  mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
         term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
full_coef$category_f <- factor(full_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "Socioemotional Learning"))

# Create regression bar chart
full_coef_plot<-ggplot(full_coef %>% 
         filter(term=="Child FI"|term=="Caregiver FI"), 
       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_grid(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/ols_full_bar_chart.png",
       width=9.26,
       height=5.5,
       units="in")

##########################################################################################
############################## Base OLS Regression w/ Gender Interaction ###############################
##########################################################################################

# Define equation
ols_base_g_model_eq <- "e_edu~e_ch_fs_dummy+e_ch_fs_dummy*female+m_edu"

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_base_int_g)

# Numeracy
num_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_base_int_g)

# Executive Function
ef_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_base_int_g)

# Strength and Difficulties
sel_reg_base_int_g<-lm(ols_base_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_base_int_g)

############################## Exporting Results ###############################

ols_models_base_int_g <- list(lit_reg_base_int_g,num_reg_base_int_g,ef_reg_base_int_g, sel_reg_base_int_g)

stargazer(ols_models_base_int_g,
          title="Base OLS Regression with Gender Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Child Reported Food Insecurity","Child Female","Midline Education Outcome","Child Reported Food Insecurity: Child Female","Constant"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_base_int_g_child_only.html")

##########################################################################################
############################## OLS Regression w/ Covariates AND Gender Interaction ###############################
##########################################################################################

# Define equation
ols_cov_g_model_eq <- "e_edu~e_ch_fs_dummy+e_ch_fs_dummy*female+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment"

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_full_int_g)

# Numeracy
num_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_full_int_g)

# Executive Function
ef_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_full_int_g)

# Strength and Difficulties
sel_reg_full_int_g<-lm(ols_cov_g_model_eq ,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_full_int_g)

############################## Exporting Results ###############################

ols_models_cov_int <- list(lit_reg_full_int_g,num_reg_full_int_g,ef_reg_full_int_g,sel_reg_full_int_g)

stargazer(ols_models_cov_int,
          title="Multivariate OLS Regression with Gender Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Child Reported Food Insecurity","Child Female","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", "Age",
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement: PC1","Parental Engagement: PC2","Parental Engagement: PC3","Parental Engagement: PC4","Treatment","Child Reported Food Insecurity: Child Female","Constant"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_full_int_g_child_only.html")

##########################################################################################
############################## Base OLS Regression w/ Age Interaction ###############################
##########################################################################################

# Define equation
ols_base_a_model_eq <- "e_edu~e_ch_fs_dummy+e_ch_fs_dummy*age+m_edu"

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_base_int_a)

# Numeracy
num_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_base_int_a)

# Executive Function
ef_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_base_int_a)

# SEL
sel_reg_base_int_a<-lm(ols_base_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_base_int_a)

############################## Exporting Results ###############################

ols_models_base_int_a <- list(lit_reg_base_int_a,num_reg_base_int_a,ef_reg_base_int_a, sel_reg_base_int_a)

stargazer(ols_models_base_int_a,
          title="Base OLS Regression with Age Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Child Reported Food Insecurity","Child Age","Midline Education Outcome","Child Reported Food Insecurity: Child Age","Constant"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_base_int_a_child_only.html")

##########################################################################################
############################## OLS Regression w/ Covariates AND Age Interaction ###############################
##########################################################################################

# Define equation
ols_cov_a_model_eq <- "e_edu~e_ch_fs_dummy+e_ch_fs_dummy*age+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment"

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_full_int_a)

# Numeracy
num_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_full_int_a)

# Executive Function
ef_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_full_int_a)

# Strength and Difficulties
sel_reg_full_int_a<-lm(ols_cov_a_model_eq ,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_full_int_a)

############################## Exporting Results ###############################

ols_models_cov_int <- list(lit_reg_full_int_a,num_reg_full_int_a,ef_reg_full_int_a,sel_reg_full_int_a)

stargazer(ols_models_cov_int,
          title="Multivariate OLS Regression with Age Interaction",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Child Reported Food Insecurity","Child Age","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", "Child Female",
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement: PC1","Parental Engagement: PC2","Parental Engagement: PC3","Parental Engagement: PC4","Treatment","Child Reported Food Insecurity: Child Age","Constant"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/OLS_full_int_a_child_only.html")

##########################################################################################
####################################### Base IV Regression ###############################
##########################################################################################

####################################### Define the Equation ##############################

child_first_stage <- "e_ch_fs_dummy~"
base_iv_instruments <- "mean_p_dev+mean_t_dev+mean_p_dev*mean_t_dev"
child_base_iv_model_eq <-paste(child_first_stage,base_iv_instruments,"+m_edu")

####################################### First Stage ###############################

# Literacy
lit_fs_pc1_1l_child_only<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc1_1l_child_only)

# Numeracy
num_fs_pc1_1l_child_only<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc1_1l_child_only)

# EF
ef_fs_pc1_1l_child_only<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc1_1l_child_only)

# SEL
sel_fs_pc1_1l_child_only<-lm(child_base_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc1_1l_child_only)

first_stage_models_child_only <- list(lit_fs_pc1_1l_child_only,
                           num_fs_pc1_1l_child_only,
                           ef_fs_pc1_1l_child_only,
                           sel_fs_pc1_1l_child_only)

stargazer(first_stage_models_child_only,
          title="Base 2SLS First Stage",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("Deviance from Mean Precipitation","Deviance from Mean Temperature","Midline Education Outcome","Deviance Interaction Term","Constant"),
          column.labels = c("Literacy Child FI",
                            "Numeracy Child FI",
                            "EF Child FI",
                            "SEL Child FI"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_first_stage_child_only.html")

###### Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients #####
lit_fs_pc1_1l_coef<-tidy(lit_fs_pc1_1l_child_only) %>% 
  mutate(category="Literacy & Child FI")
num_fs_pc1_1l_coef<-tidy(num_fs_pc1_1l_child_only) %>% 
  mutate(category="Numeracy & Child FI")
ef_fs_pc1_1l_coef<-tidy(ef_fs_pc1_1l_child_only) %>% 
  mutate(category="Executive Function & Child FI")
sel_fs_pc1_1l_coef<-tidy(sel_fs_pc1_1l_child_only) %>% 
  mutate(category="Socioemotional Learning & Child FI")
lit_fs_pc2_1l_coef<-tidy(lit_fs_pc2_1l_child_only) %>% 
  mutate(category="Literacy & Caregiver FI")
num_fs_pc2_1l_coef<-tidy(num_fs_pc2_1l_child_only) %>% 
  mutate(category="Numeracy & Caregiver FI")
ef_fs_pc2_1l_coef<-tidy(ef_fs_pc2_1l_child_only) %>% 
  mutate(category="Executive Function & Caregiver FI")
sel_fs_pc2_1l_coef<-tidy(sel_fs_pc2_1l_child_only) %>% 
  mutate(category="Socioemotional Learning & Caregiver FI")

iv_1l_coef<-rbind(lit_fs_pc1_1l_coef,
                  lit_fs_pc2_1l_coef,
                  num_fs_pc1_1l_coef,
                  num_fs_pc2_1l_coef,
                  ef_fs_pc1_1l_coef,
                  ef_fs_pc2_1l_coef,
                  sel_fs_pc1_1l_coef,
                  sel_fs_pc2_1l_coef) %>% 
  mutate(term=str_replace(term,"mean_p_dev","Precip. Dev."),
         term=str_replace(term,"mean_t_dev","Temp. Dev."))
iv_1l_coef$category_f <- factor(iv_1l_coef$category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "Socioemotional Learning & Child FI",
                                                            "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "Socioemotional Learning & Caregiver FI"))

# Create regression bar chart
iv_1l_coef_plot<-ggplot(iv_1l_coef %>% 
                         filter(term=="Precip. Dev."|term=="Temp. Dev."|term=="Precip. Dev.:Temp. Dev."), 
                       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_wrap(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/iv_1l_bar_chart.png",
       width=12,
       height=7.13,
       units="in")

############################################ Second Stage #####################################

########################################### Define the Equation ##############################

base_iv_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+m_edu |",base_iv_instruments,"+m_edu")

# Literacy
lit_ivreg_base_child_only<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_base_child_only,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_base_child_only<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_base_child_only,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_base_child_only<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_base_child_only,vcov=sandwich,diagnostics=TRUE)

# SEL
sel_ivreg_base_child_only<-ivreg(base_iv_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_base_child_only,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

iv_child_only <- list(lit_ivreg_base_child_only,num_ivreg_base_child_only,ef_ivreg_base_child_only, sel_ivreg_base_child_only)

stargazer(iv_child_only,
          title="Base 2SLS Second Stage",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          covariate.labels=c("Child Reported Food Insecurity","Midline Education Outcome","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_second_stage_child_only.html")

# Export regression coefficients as bar charts. First, create data frame of all regression coefficients
lit_2l_coef<-tidy(lit_ivreg_base_child_only) %>% 
  mutate(category="Literacy")
num_2l_coef<-tidy(num_ivreg_base_child_only) %>% 
  mutate(category="Numeracy")
ef_2l_coef<-tidy(ef_ivreg_base_child_only) %>% 
  mutate(category="Executive Function")
sel_2l_coef<-tidy(sel_ivreg_base_child_only) %>% 
  mutate(category="Socioemotional Learning")

iv_2l_coef<-rbind(lit_2l_coef,num_2l_coef,ef_2l_coef,sel_2l_coef) %>% 
  mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
         term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
iv_2l_coef$category_f <- factor(iv_2l_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "Socioemotional Learning"))

# Create regression bar chart
iv_2l_coef<-ggplot(iv_2l_coef %>% 
                         filter(term=="Child FI"|term=="Caregiver FI"), 
                       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_grid(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/iv_2l_bar_chart.png",
       width=9.26,
       height=5.5,
       units="in")

##########################################################################################
################################### Multivariate IV Regression ###########################
##########################################################################################

####################################### Define the Equation ##############################

cov_iv_instruments <- "extreme_m_p_dev+extreme_m_t_dev+extreme_m_p_dev*extreme_m_t_dev"
child_cov_iv_model_eq <-paste(child_first_stage,cov_iv_instruments,"+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

####################################### First Stage ###############################

# Literacy
lit_fs_pc1_1l_cov_child_only<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc1_1l_cov_child_only)

# Numeracy
num_fs_pc1_1l_cov_child_only<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc1_1l_cov_child_only)

# EF
ef_fs_pc1_1l_cov_child_only<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc1_1l_cov_child_only)

# SEL
sel_fs_pc1_1l_cov_child_only<-lm(child_cov_iv_model_eq,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc1_1l_cov_child_only)

first_stage_models_cov_child_only <- list(lit_fs_pc1_1l_cov_child_only,
                           num_fs_pc1_1l_cov_child_only,
                           ef_fs_pc1_1l_cov_child_only,
                           sel_fs_pc1_1l_cov_child_only)

stargazer(first_stage_models_cov_child_only,
          title="Multivariate 2SLS First Stage",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("Precipitation Deviance Indicator","Temperature Deviance Indicator","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids","Parental Engagement","Deviance Indicator Interaction","Treatment"),        
          column.labels = c("Literacy Child FI",
                             "Numeracy Child FI",
                             "EF Child FI",
                             "SEL Child FI"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_first_stage_cov_child_only.html")

##### Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients####
lit_fs_pc1_1l_cov_coef<-tidy(lit_fs_pc1_1l_cov_child_only) %>% 
  mutate(category="Literacy & Child FI")
num_fs_pc1_1l_cov_coef<-tidy(num_fs_pc1_1l_cov_child_only) %>% 
  mutate(category="Numeracy & Child FI")
ef_fs_pc1_1l_cov_coef<-tidy(ef_fs_pc1_1l_cov_child_only) %>% 
  mutate(category="Executive Function & Child FI")
sel_fs_pc1_1l_cov_coef<-tidy(sel_fs_pc1_1l_cov_child_only) %>% 
  mutate(category="Socioemotional Learning & Child FI")
lit_fs_pc2_1l_cov_coef<-tidy(lit_fs_pc2_1l_cov_child_only) %>% 
  mutate(category="Literacy & Caregiver FI")
num_fs_pc2_1l_cov_coef<-tidy(num_fs_pc2_1l_cov_child_only) %>% 
  mutate(category="Numeracy & Caregiver FI")
ef_fs_pc2_1l_cov_coef<-tidy(ef_fs_pc2_1l_cov_child_only) %>% 
  mutate(category="Executive Function & Caregiver FI")
sel_fs_pc2_1l_cov_coef<-tidy(sel_fs_pc2_1l_cov_child_only) %>% 
  mutate(category="Socioemotional Learning & Caregiver FI")

iv_1l_cov_coef<-rbind(lit_fs_pc1_1l_cov_coef,
                  lit_fs_pc2_1l_cov_coef,
                  num_fs_pc1_1l_cov_coef,
                  num_fs_pc2_1l_cov_coef,
                  ef_fs_pc1_1l_cov_coef,
                  ef_fs_pc2_1l_cov_coef,
                  sel_fs_pc1_1l_cov_coef,
                  sel_fs_pc2_1l_cov_coef) %>% 
  mutate(term=str_replace(term,"extreme_m_p_dev","Precip. Dev."),
         term=str_replace(term,"extreme_m_t_dev","Temp. Dev."))
iv_1l_cov_coef$category_f <- factor(iv_1l_cov_coef$category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "Socioemotional Learning & Child FI",
                                                              "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "Socioemotional Learning & Caregiver FI"))

# Create regression bar chart
iv_1l_cov_coef_plot<-ggplot(iv_1l_cov_coef %>% 
                          filter(term=="Precip. Dev."|term=="Temp. Dev."|term=="Precip. Dev.:Temp. Dev."), 
                        aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_wrap(~factor(category, levels=c("Literacy & Child FI", "Numeracy & Child FI", "Executive Function & Child FI", "Socioemotional Learning & Child FI",
                                        "Literacy & Caregiver FI", "Numeracy & Caregiver FI", "Executive Function & Caregiver FI", "Socioemotional Learning & Caregiver FI"))) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/iv_1l_cov_bar_chart.png",
       width=12,
       height=7.13,
       units="in")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

cov_iv_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+m_edu +region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment|",cov_iv_instruments,"+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

# Literacy
lit_ivreg_full_cov_child_only<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_full_cov_child_only,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_full_cov_child_only<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_full_cov_child_only,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_full_cov_child_only<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_full_cov_child_only,vcov=sandwich,diagnostics=TRUE)

# SDQ
sel_ivreg_full_cov_child_only<-ivreg(cov_iv_second_stage_model_eq, data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_full_cov_child_only,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

iv_cov_child_only <- list(lit_ivreg_full_cov_child_only,num_ivreg_full_cov_child_only,ef_ivreg_full_cov_child_only, sel_ivreg_full_cov_child_only)

stargazer(iv_cov_child_only,
          title="Multivariate 2SLS Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Child Reported Food Insecurity","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_second_stage_child_only.html")

# Export regression coefficients as bar charts. First, create data frame of all regression coefficients
lit_2l_cov_coef<-tidy(lit_ivreg_full_cov_child_only) %>% 
  mutate(category="Literacy")
num_2l_cov_coef<-tidy(num_ivreg_full_cov_child_only) %>% 
  mutate(category="Numeracy")
ef_2l_cov_coef<-tidy(ef_ivreg_full_cov_child_only) %>% 
  mutate(category="Executive Function")
sel_2l_cov_coef<-tidy(sel_ivreg_full_cov_child_only) %>% 
  mutate(category="Socioemotional Learning")

iv_2l_cov_coef<-rbind(lit_2l_cov_coef,num_2l_cov_coef,ef_2l_cov_coef,sel_2l_cov_coef) %>% 
  mutate(term=str_replace(term,"e_ch_fs_dummy","Child FI"),
         term=str_replace(term,"e_cg_fs_dummy","Caregiver FI"))
iv_2l_cov_coef$category_f <- factor(iv_2l_cov_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "Socioemotional Learning"))

# Create regression bar chart
iv_2l_cov_coef<-ggplot(iv_2l_cov_coef %>% 
                         filter(term=="Child FI"|term=="Caregiver FI"), 
                       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_grid(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/iv_2l_cov_bar_chart.png",
       width=9.26,
       height=5.5,
       units="in")

##########################################################################################
################################## Base IV Regression w/ Gender Interaction ##############
##########################################################################################

####################################### First Stage ###############################

########################################### Define the Equation ##############################

base_iv_g_instruments <- "extreme_m_p_dev + extreme_o_p_dev+optimal_t_dev"
first_stage_int<-"e_ch_fs_dummy~"
first_stage_g <-"female~"
first_stage_int_g<-"e_ch_fs_dummy*female~"
base_first_stage_int_model_eq <-paste(first_stage_int,base_iv_g_instruments,"+m_edu")
base_first_stage_g_model_eq <-paste(first_stage_g,base_iv_g_instruments,"+m_edu")
base_first_stage_int_g_model_eq <-paste(first_stage_int_g,base_iv_g_instruments,"+m_edu")

# Literacy
lit_child_fs_1l_base_child_only<-lm(base_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_child_only)
lit_child_fs_1l_base_g_child_only<-lm(base_first_stage_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_g_child_only)
lit_child_fs_1l_base_int_g_child_only<-lm(base_first_stage_int_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_int_g_child_only)

first_stage_models_base_int_g <- list(lit_child_fs_1l_base_child_only,
                                      lit_child_fs_1l_base_g_child_only,
                                      lit_child_fs_1l_base_int_g_child_only)

stargazer(first_stage_models_base_int_g,
          title="Base IV Regression with Gender Interaction: First stage",
          dep.var.caption = "Dependent Variable:",
          column.labels = c("Child Reported Food Insecurity","Child Female",
                             "Child Reported Food Insecurity: Child Female"),
          covariate.labels=c("Mean Precipitation Deviance Indicator","Optimal Precipitation Deviance Indicator","Optimal Temperature Deviance","Midline Education"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_first_stage_g_child_only.html")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

base_iv_g_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+female+e_ch_fs_dummy*female+ m_edu|",base_iv_g_instruments,"+m_edu")

# Literacy
lit_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)

# SEL
sel_ivreg_base_int_g<-ivreg(base_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_base_int_g,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

second_stage_models_base_int_g <- list(lit_ivreg_base_int_g,num_ivreg_base_int_g,ef_ivreg_base_int_g,sel_ivreg_base_int_g)

stargazer(second_stage_models_base_int_g,
          title="Base IV Regression with Gender Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity","Child Female","Midline Education Outcome","Food Insecurity: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_second_stage_g_child_only.html")

##########################################################################################
################################## Multivariate IV Regression w/ Gender Interaction ###########################
##########################################################################################

####################################### First Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_g_instruments <- "extreme_o_t_dev + extreme_m_t_dev + mean_t_dev"
multivariate_first_stage_int_model_eq <-paste(first_stage_int,multivariate_iv_g_instruments,"+m_edu+region+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")
multivariate_first_stage_g_model_eq <-paste(first_stage_g,multivariate_iv_g_instruments,"+m_edu+region+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")
multivariate_first_stage_int_g_model_eq <-paste(first_stage_int_g,multivariate_iv_g_instruments,"+m_edu+region+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

# Literacy 
lit_child_fs_1l_multivariate_child_only<-lm(multivariate_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_child_only)
lit_child_fs_1l_multivariate_g_child_only<-lm(multivariate_first_stage_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_g_child_only)
lit_child_fs_1l_multivariate_int_g_child_only<-lm(multivariate_first_stage_int_g_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_int_g_child_only)

first_stage_models_multivariate_int_g <- list(lit_child_fs_1l_multivariate_child_only,
                                              lit_child_fs_1l_multivariate_g_child_only,
                                              lit_child_fs_1l_multivariate_int_g_child_only)

stargazer(first_stage_models_multivariate_int_g,
          title="Multivariate IV Regression with Gender Interaction: First Stage",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("Optimal Temperature Deviance Indicator","Mean Temperature Deviance Indicator","Mean Temperature Deviance","Midline Education","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment"),
          dep.var.labels = c("Food Insecurity PC","Child Female",
                             "Food Insecurity PC: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_first_stage_g_child_only.html")


####################################### Second Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_g_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+female+e_ch_fs_dummy*female+m_edu+region+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment|",multivariate_iv_g_instruments,"+m_edu+region+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

# Literacy
lit_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_multivariate_int_g,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_multivariate_int_g,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_multivariate_int_g,vcov=sandwich,diagnostics=TRUE)

# SEL
sel_ivreg_multivariate_int_g<-ivreg(multivariate_iv_g_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_multivariate_int_g,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

second_stage_models_multivariate_int_g <- list(lit_ivreg_multivariate_int_g,num_ivreg_multivariate_int_g,ef_ivreg_multivariate_int_g,sel_ivreg_multivariate_int_g)

stargazer(second_stage_models_multivariate_int_g,
          title="Multivariate IV Regression with Gender Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity","Child Female","Midline Education","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", "Child Age",
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment","Food Insecurity: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_second_stage_g_child_only.html")

##########################################################################################
################################## Base IV Regression w/ Age Interaction ##############
##########################################################################################
# Note that the IV interaction models will use e_ch_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality

####################################### First Stage ###############################

########################################### Define the Equation ##############################

base_iv_a_instruments <- "extreme_m_p_dev + extreme_o_p_dev+optimal_t_dev"
first_stage_a <-"age~"
first_stage_int_a<-"e_ch_fs_dummy*age~"
base_first_stage_int_model_eq <-paste(first_stage_int,base_iv_a_instruments,"+m_edu")
base_first_stage_a_model_eq <-paste(first_stage_a,base_iv_a_instruments,"+m_edu")
base_first_stage_int_a_model_eq <-paste(first_stage_int_a,base_iv_a_instruments,"+m_edu")

# Literacy
lit_child_fs_1l_base_child_only<-lm(base_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_child_only)
lit_child_fs_1l_base_a_child_only<-lm(base_first_stage_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_a_child_only)
lit_child_fs_1l_base_int_a_child_only<-lm(base_first_stage_int_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_base_int_a_child_only)

first_stage_models_base_int_a <- list(lit_child_fs_1l_base_child_only,
                                      lit_child_fs_1l_base_a_child_only,
                                      lit_child_fs_1l_base_int_a_child_only)

stargazer(first_stage_models_base_int_a,
          title="Base IV Regression with Gender Interaction: First Stage",
          dep.var.caption = "Dependent Variable:",
          column.labels = c("Child Reported Food Insecurity","Child Age",
                            "Child Reported Food Insecurity: Child Age"),
          covariate.labels=c("Mean Precipitation Deviance Indicator","Optimal Precipitation Deviance Indicator","Optimal Temperature Deviance","Midline Education"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_first_stage_a_child_only.html")


####################################### Second Stage ###############################

########################################### Define the Equation ##############################

base_iv_a_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+age+e_ch_fs_dummy*age+ m_edu|",base_iv_a_instruments,"+m_edu")

# Literacy
lit_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)

# SEL
sel_ivreg_base_int_a<-ivreg(base_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_base_int_a,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

second_stage_models_base_int_a <- list(lit_ivreg_base_int_a,num_ivreg_base_int_a,ef_ivreg_base_int_a,sel_ivreg_base_int_a)

stargazer(second_stage_models_base_int_a,
          title="Base IV Regression with Age Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity","Child Female","Midline Education Outcome","Food Insecurity: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/base_second_stage_a_child_only.html")

##########################################################################################
################################## Multivariate IV Regression w/ Age Interaction ##############
##########################################################################################
# Note that the IV interaction models will use e_ch_fs_dummy (a combined version of fs_dummy from caregivers and children) to reduce dimensionality

####################################### First Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_a_instruments <- "extreme_m_t_dev + extreme_o_t_dev+optimal_t_dev"
multivariate_first_stage_int_model_eq <-paste(first_stage_int,multivariate_iv_a_instruments,"+m_edu+region+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")
multivariate_first_stage_a_model_eq <-paste(first_stage_a,multivariate_iv_a_instruments,"+m_edu+region+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")
multivariate_first_stage_int_a_model_eq <-paste(first_stage_int_a,multivariate_iv_a_instruments,"+m_edu+region+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

# Literacy
lit_child_fs_1l_multivariate_child_only<-lm(multivariate_first_stage_int_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_child_only)
lit_child_fs_1l_multivariate_a_child_only<-lm(multivariate_first_stage_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_a_child_only)
lit_child_fs_1l_multivariate_int_a_child_only<-lm(multivariate_first_stage_int_a_model_eq,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_child_fs_1l_multivariate_int_a_child_only)

first_stage_models_multivariate_int_a <- list(lit_child_fs_1l_multivariate_child_only,
                                              lit_child_fs_1l_multivariate_a_child_only,
                                              lit_child_fs_1l_multivariate_int_a_child_only)

stargazer(first_stage_models_multivariate_int_a,
          title="Multivariate IV Regression with Age Interaction: First Stage",
          dep.var.caption = "Dependent Variable:",
          column.labels = c("Child Reported Food Insecurity","Child Age",
                            "Child Reported Food Insecurity: Child Age"),
          covariate.labels=c("Mean Temperature Deviance Indicator","Optimal Temperature Deviance Indicator","Optimal Temperature Deviance","Midline Education"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_first_stage_a_child_only.html")

####################################### Second Stage ###############################

########################################### Define the Equation ##############################

multivariate_iv_a_second_stage_model_eq <- paste("e_edu ~ e_ch_fs_dummy+age+e_ch_fs_dummy*age+ +m_edu+region+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment|",multivariate_iv_a_instruments,"+m_edu+region+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment")

# Literacy
lit_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_multivariate_int_a,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_multivariate_int_a,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_multivariate_int_a,vcov=sandwich,diagnostics=TRUE)

# SEL
sel_ivreg_multivariate_int_a<-ivreg(multivariate_iv_a_second_stage_model_eq,data=full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_multivariate_int_a,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

second_stage_models_multivariate_int_a <- list(lit_ivreg_multivariate_int_a,num_ivreg_multivariate_int_a,ef_ivreg_multivariate_int_a,sel_ivreg_multivariate_int_a)

stargazer(second_stage_models_multivariate_int_a,
          title="Multivariate IV Regression with Age Interaction: Second Stage",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity","Child Age","Midline Education","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", "Child Age",
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment","Food Insecurity: Child Age"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/results_with_child_only_code/multivariate_second_stage_a_child_only.html")

