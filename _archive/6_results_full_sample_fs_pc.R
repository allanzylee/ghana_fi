###################################### Introduction ############################################

# Author: Allan Lee
# Date: Feb 15th, 2023
# Purpose: Combine all data together, conduct summary statistics, and run the regressions

##########################################################################################
############################################### Set up ###################################
##########################################################################################

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
         ed4,m_sel_per,m_lit_per,m_ef_per,m_num_per,e_sel_per,e_lit_per,e_ef_per,e_num_per,contains("fs_pc"),contains("fs_dummy"),PC1,PC2,PC3,PC4,home_scale,mid_reverse_ppi,num_kids,io1,ed3,pe7,treatment) %>% 
  mutate(cr3.y=if_else(cr3.y==1,0,1),
         cb2=if_else(cb2==1,0,1),
         ed2=if_else(ed2==1,0,1),
         cb5=if_else(cb5==3,1,
                     if_else(cb5==4,1,0)),
         fs_pc=(e_fs_pc1+e_fs_pc2)/2) %>% 
  rename(female=cr3.y,
         month_of_birth=cr5_2.y,
         age=cr6.x,
         enrolled_in_school=cr7.x,
         private_school=ed2,
         cg_age=cb1,
         cg_female=cb2,
         cg_edu=cb3,
         marital_status=cb5,
         minute_to_school=ed4,
         poverty=mid_reverse_ppi,
         pe_pc1=PC1,
         pe_pc2=PC2,
         pe_pc3=PC3,
         pe_pc4=PC4,
         language=io1,
         current_class=ed3,
         num_books=pe7) %>% 
  # Correct for missing data for one person
  mutate(region=ifelse(careid==11117814,"NORTHERN",region)) %>% 
  # Standardize outcome data
  mutate(m_sel_per=scale(m_sel_per)[,1],
         m_lit_per=scale(m_lit_per)[,1],
         m_ef_per=scale(m_ef_per)[,1],
         m_num_per=scale(m_num_per)[,1],
         e_sel_per=scale(e_sel_per)[,1],
         e_lit_per=scale(e_lit_per)[,1],
         e_ef_per=scale(e_ef_per)[,1],
         e_num_per=scale(e_num_per)[,1]) %>% 
  # Select the midline food insecurity variables
  dplyr::select(-m_ch_fs_pc,-m_cg_fs_pc,-starts_with("m_fs_pc"),-m_ch_fs_dummy,-m_cg_fs_dummy) %>% 
  # Filter out NAs
  filter(!is.na(ssmi_w+spei_w),!is.na(spei_w),!is.na(ssfi_w))

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

######################################## Child Demographics ##############################

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
######################################## Base OLS Regression  ############################
##########################################################################################

# OLS WITHOUT COVARIATES: PRIMARY

lit_test <- lm(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_test)

num_test <- lm(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_test)

ef_test <- lm(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_test)

sel_test <- lm(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_test)

############################## Exporting Results ###############################

ols_models <- list(lit_test,num_test,ef_test,sel_test)

stargazer(ols_models,
          title="OLS Regression with No Covariates",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          covariate.labels=c("Endline Food Insecurity PC1","Endline Food Insecurity PC2","Midline Education Outcome","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/OLS_full.html")

##########################################################################################
############################## OLS Regression w/ Covariates ###############################
##########################################################################################

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_full<-lm(e_edu~e_fs_pc1+e_fs_pc2+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_full)

# Numeracy
num_reg_full<-lm(e_edu~e_fs_pc1+e_fs_pc2+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_full)

# Executive Function
ef_reg_full<-lm(e_edu~e_fs_pc1+e_fs_pc2+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_full)

# SEL
sel_reg_full<-lm(e_edu~e_fs_pc1+e_fs_pc2+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_full)

############################## Exporting Results ###############################

ols_models_cov <- list(lit_reg_full,num_reg_full,ef_reg_full, sel_reg_full)

stargazer(ols_models_cov,
          title="OLS Regression with Covariates: Full data",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Endline Food Insecurity PC1","Endline Food Insecurity PC2","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement: PC1","Parental Engagement: PC2","Parental Engagement: PC3","Parental Engagement: PC4","Treatment"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/OLS_full_cov.html")

# Export regression coefficients as bar charts. First, create data frame of all regression coefficients
lit_reg_full_coef<-tidy(lit_reg_full) %>% 
  mutate(category="Literacy")
num_reg_full_coef<-tidy(num_reg_full) %>% 
  mutate(category="Numeracy")
ef_reg_full_coef<-tidy(ef_reg_full) %>% 
  mutate(category="Executive Function")
sel_reg_full_coef<-tidy(sel_reg_full) %>% 
  mutate(category="Socioemotional Learning")

full_coef<-rbind(lit_reg_full_coef,num_reg_full_coef,ef_reg_full_coef,sel_reg_full_coef) %>% 
  mutate(term=str_replace(term,"e_fs_pc1","FI PC1"),
         term=str_replace(term,"e_fs_pc2","FI PC2"))
full_coef$category_f <- factor(full_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "Socioemotional Learning"))

# Create regression bar chart
full_coef_plot<-ggplot(full_coef %>% 
         filter(term=="FI PC1"|term=="FI PC2"), 
       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_grid(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/ols_full_cov_bar_chart.png",
       width=9.26,
       height=5.5,
       units="in")

##########################################################################################
############################## OLS Regression w/ Covariates AND Interaction ###############################
##########################################################################################

### PRIMARY SCHOOL: Run an OLS regression of each outcome variable on all variables

# Literacy
lit_reg_full_int<-lm(e_edu~e_fs_pc1+e_fs_pc2+e_fs_pc1*age+e_fs_pc2*age+e_fs_pc1*female+e_fs_pc2*female+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_reg_full_int)

# Numeracy
num_reg_full_int<-lm(e_edu~e_fs_pc1+e_fs_pc2+e_fs_pc1*age+e_fs_pc2*age+e_fs_pc1*female+e_fs_pc2*female+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_reg_full_int)

# Executive Function
ef_reg_full_int<-lm(e_edu~e_fs_pc1+e_fs_pc2+e_fs_pc1*age+e_fs_pc2*age+e_fs_pc1*female+e_fs_pc2*female+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_reg_full_int)

# Strength and Difficulties
sel_reg_full_int<-lm(e_edu~e_fs_pc1+e_fs_pc2+e_fs_pc1*age+e_fs_pc2*age+e_fs_pc1*female+e_fs_pc2*female+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+pe_pc2+pe_pc3+pe_pc4+treatment,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_reg_full_int)

############################## Exporting Results ###############################

ols_models_cov_int <- list(lit_reg_full_int,num_reg_full_int,ef_reg_full_int, sel_reg_full_int)

stargazer(ols_models_cov_int,
          title="OLS Regression with Covariates: Full data",
          dep.var.caption = "Endline Dependent Variable:",
          covariate.labels=c("Food Insecurity PC1","Food Insecurity PC2","Child Age","Child Female","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement: PC1","Parental Engagement: PC2","Parental Engagement: PC3","Parental Engagement: PC4","Treatment","Food Insecurity PC1: Child Age","Food Insecurity PC2: Child Age","Food Insecurity PC1: Child Female","Food Insecurity PC2: Child Female","Constant"),
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/OLS_full_cov_int.html")

##########################################################################################
####################################### Base IV Regression ###############################
##########################################################################################

####################################### First Stage ###############################

# Literacy
lit_fs_pc1_1l<-lm(e_fs_pc1~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc1_1l)
lit_fs_pc2_1l<-lm(e_fs_pc2~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc2_1l)

# Numeracy
num_fs_pc1_1l<-lm(e_fs_pc1~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc1_1l)
num_fs_pc2_1l<-lm(e_fs_pc2~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc2_1l)

# EF
ef_fs_pc1_1l<-lm(e_fs_pc1~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc1_1l)
ef_fs_pc2_1l<-lm(e_fs_pc2~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc2_1l)

# SEL
sel_fs_pc1_1l<-lm(e_fs_pc1~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc1_1l)
sel_fs_pc2_1l<-lm(e_fs_pc2~ssmi_w+spei_w+m_edu,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc2_1l)

first_stage_models <- list(lit_fs_pc1_1l,lit_fs_pc2_1l,
                           num_fs_pc1_1l,num_fs_pc2_1l,
                           ef_fs_pc1_1l,ef_fs_pc2_1l,
                           sel_fs_pc1_1l,sel_fs_pc2_1l)

stargazer(first_stage_models,
          title="2SLS First Stage: Regression of Food Insecurity PC1 and PC2 (averaged) on Household Deviance from Optimal Crop Growing Temperature and Precipitation",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("Standardized Soil Moisture Index","Standardized Precipitation-Evapotraspiration Index","Midline Education Outcome","Precipitation Deviance: Temperature Deviance"),
          dep.var.labels = c("Literacy FI PC1","Literacy FI PC1",
                            "Numeracy FI PC1","Numeracy FI PC2",
                            "EF FI PC1","EF FI PC2",
                            "SEL FI PC1","SEL FI PC2"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/first_stage.html")

###### Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients #####
lit_fs_pc1_1l_coef<-tidy(lit_fs_pc1_1l) %>% 
  mutate(category="Literacy & FI PC1")
num_fs_pc1_1l_coef<-tidy(num_fs_pc1_1l) %>% 
  mutate(category="Numeracy & FI PC1")
ef_fs_pc1_1l_coef<-tidy(ef_fs_pc1_1l) %>% 
  mutate(category="Executive Function & FI PC1")
sel_fs_pc1_1l_coef<-tidy(sel_fs_pc1_1l) %>% 
  mutate(category="Socioemotional Learning & FI PC1")
lit_fs_pc2_1l_coef<-tidy(lit_fs_pc2_1l) %>% 
  mutate(category="Literacy & FI PC2")
num_fs_pc2_1l_coef<-tidy(num_fs_pc2_1l) %>% 
  mutate(category="Numeracy & FI PC2")
ef_fs_pc2_1l_coef<-tidy(ef_fs_pc2_1l) %>% 
  mutate(category="Executive Function & FI PC2")
sel_fs_pc2_1l_coef<-tidy(sel_fs_pc2_1l) %>% 
  mutate(category="Socioemotional Learning & FI PC2")

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
iv_1l_coef$category_f <- factor(iv_1l_coef$category, levels=c("Literacy & FI PC1", "Numeracy & FI PC1", "Executive Function & FI PC1", "Socioemotional Learning & FI PC1",
                                                            "Literacy & FI PC2", "Numeracy & FI PC2", "Executive Function & FI PC2", "Socioemotional Learning & FI PC2"))

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

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/iv_1l_bar_chart.png",
       width=12,
       height=7.13,
       units="in")

############################################ Second Stage #####################################

# Literacy
lit_ivreg_base<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu, ~ssmi_w+spei_w+m_edu,full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_base,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_base<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu, ~ssmi_w+spei_w+m_edu,full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_base,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_base<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu, ~ssmi_w+spei_w+m_edu,full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_base,vcov=sandwich,diagnostics=TRUE)

# SDQ
sel_ivreg_base<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu, ~ssmi_w+spei_w+m_edu,full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_base,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

iv <- list(lit_ivreg_base,num_ivreg_base,ef_ivreg_base, sel_ivreg_base)

stargazer(iv,
          title="Base IV Regression",
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy","Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity PC1","Food Insecurity PC2","Midline Education Outcome","Constant"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/IV_full.html")

# Export regression coefficients as bar charts. First, create data frame of all regression coefficients
lit_2l_coef<-tidy(lit_ivreg_base) %>% 
  mutate(category="Literacy")
num_2l_coef<-tidy(num_ivreg_base) %>% 
  mutate(category="Numeracy")
ef_2l_coef<-tidy(ef_ivreg_base) %>% 
  mutate(category="Executive Function")
sel_2l_coef<-tidy(sel_ivreg_base) %>% 
  mutate(category="Socioemotional Learning")

iv_2l_coef<-rbind(lit_2l_coef,num_2l_coef,ef_2l_coef,sel_2l_coef) %>% 
  mutate(term=str_replace(term,"e_fs_pc1","FI PC1"),
         term=str_replace(term,"e_fs_pc2","FI PC2"))
iv_2l_coef$category_f <- factor(iv_2l_coef$category, levels=c("Literacy", "Numeracy", "Executive Function", "Socioemotional Learning"))

# Create regression bar chart
iv_2l_coef<-ggplot(iv_2l_coef %>% 
                         filter(term=="FI PC1"|term=="FI PC2"), 
                       aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_grid(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/iv_2l_bar_chart.png",
       width=9.26,
       height=5.5,
       units="in")

##########################################################################################
################################## IV Regression w/ Covariates ###########################
##########################################################################################

####################################### First Stage ###############################

# Literacy
lit_fs_pc1_1l_cov<-lm(e_fs_pc1~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc1_1l_cov)
lit_fs_pc2_1l_cov<-lm(e_fs_pc2~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc2_1l_cov)

# Numeracy
num_fs_pc1_1l_cov<-lm(e_fs_pc1~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc1_1l_cov)
num_fs_pc2_1l_cov<-lm(e_fs_pc2~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="num_per"))
summary(num_fs_pc2_1l_cov)

# EF
ef_fs_pc1_1l_cov<-lm(e_fs_pc1~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc1_1l_cov)
ef_fs_pc2_1l_cov<-lm(e_fs_pc2~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="ef_per"))
summary(ef_fs_pc2_1l_cov)

# SEL
sel_fs_pc1_1l_cov<-lm(e_fs_pc1~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc1_1l_cov)
sel_fs_pc2_1l_cov<-lm(e_fs_pc2~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="sel_per"))
summary(sel_fs_pc2_1l_cov)

first_stage_models_cov <- list(lit_fs_pc1_1l_cov,lit_fs_pc2_1l_cov,
                           num_fs_pc1_1l_cov,num_fs_pc2_1l_cov,
                           ef_fs_pc1_1l_cov,ef_fs_pc2_1l_cov,
                           sel_fs_pc1_1l_cov,sel_fs_pc2_1l_cov)

stargazer(first_stage_models_cov,
          title="2SLS First Stage: Regression of Food Insecurity PC1 and PC2 (averaged) on Household Deviance from Optimal Crop Growing Temperature and Precipitation",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("SSFI","SSFI Squared","SSFI Cubed","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids","Parental Engagement","Treatment"),          
          dep.var.labels = c("Literacy FI PC1","Literacy FI PC2",
                             "Numeracy FI PC1","Numeracy FI PC2",
                             "EF FI PC1","EF FI PC2",
                             "SEL FI PC1","SEL FI PC2"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/first_stage_cov.html")



# Export FIRST STAGE regression coefficients as bar charts. First, create data frame of all regression coefficients
lit_fs_pc1_1l_cov_coef<-tidy(lit_fs_pc1_1l_cov) %>% 
  mutate(category="Literacy & FI PC1")
num_fs_pc1_1l_cov_coef<-tidy(num_fs_pc1_1l_cov) %>% 
  mutate(category="Numeracy & FI PC1")
ef_fs_pc1_1l_cov_coef<-tidy(ef_fs_pc1_1l_cov) %>% 
  mutate(category="Executive Function & FI PC1")
sel_fs_pc1_1l_cov_coef<-tidy(sel_fs_pc1_1l_cov) %>% 
  mutate(category="Socioemotional Learning & FI PC1")
lit_fs_pc2_1l_cov_coef<-tidy(lit_fs_pc2_1l_cov) %>% 
  mutate(category="Literacy & FI PC2")
num_fs_pc2_1l_cov_coef<-tidy(num_fs_pc2_1l_cov) %>% 
  mutate(category="Numeracy & FI PC2")
ef_fs_pc2_1l_cov_coef<-tidy(ef_fs_pc2_1l_cov) %>% 
  mutate(category="Executive Function & FI PC2")
sel_fs_pc2_1l_cov_coef<-tidy(sel_fs_pc2_1l_cov) %>% 
  mutate(category="Socioemotional Learning & FI PC2")

iv_1l_cov_coef<-rbind(lit_fs_pc1_1l_cov_coef,
                  lit_fs_pc2_1l_cov_coef,
                  num_fs_pc1_1l_cov_coef,
                  num_fs_pc2_1l_cov_coef,
                  ef_fs_pc1_1l_cov_coef,
                  ef_fs_pc2_1l_cov_coef,
                  sel_fs_pc1_1l_cov_coef,
                  sel_fs_pc2_1l_cov_coef) %>% 
  mutate(term=str_replace(term,"extreme_m_p_dev","Mean Precip. Dev. Indicator"),
         term=str_replace(term,"extreme_m_t_dev","Mean Temp. Dev. Indicator"))
iv_1l_cov_coef$category_f <- factor(iv_1l_cov_coef$category, levels=c("Literacy & FI PC1", "Numeracy & FI PC1", "Executive Function & FI PC1", "Socioemotional Learning & FI PC1",
                                                              "Literacy & FI PC2", "Numeracy & FI PC2", "Executive Function & FI PC2", "Socioemotional Learning & FI PC2"))

# Create regression bar chart
iv_1l_cov_coef_plot<-ggplot(iv_1l_cov_coef %>% 
                          filter(term=="Mean Precip. Dev. Indicator"|term=="Mean Temp. Dev. Indicator"), 
                        aes(x=term, y=estimate)) + 
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.96 * std.error, 
                    ymax=estimate + 1.96 * std.error), 
                size=.75, width=.3) +
  facet_wrap(~category_f) +
  theme_bw() +
  theme(text=element_text(family="Times", size=12))

ggsave("/Users/AllanLee/Desktop/Penn/ECON4900/Output/iv_1l_cov_bar_chart.png",
       width=12,
       height=7.13,
       units="in")

####################################### Second Stage ###############################

# Literacy
lit_ivreg_full_cov<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,
                          ~poly(ssmi_w,3)+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_full_cov,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_full_cov<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                           ~poly(ssmi_w,3)+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_full_cov,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_full_cov<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                           ~poly(ssmi_w,3)+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_full_cov,vcov=sandwich,diagnostics=TRUE)

# SDQ
sel_ivreg_full_cov<-ivreg(e_edu ~ e_fs_pc1+e_fs_pc2+m_edu+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                          ~poly(ssmi_w,3)+m_edu+region+female+age+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sel_ivreg_full_cov,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

iv_cov <- list(lit_ivreg_full_cov,num_ivreg_full_cov,ef_ivreg_full_cov, sel_ivreg_full_cov)

stargazer(iv_cov,
          title="IV Regression with Controls: Full Data",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity PC1","Food Insecurity PC2","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Child Female","Child Age","Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/IV_full_cov.html")

##########################################################################################
################################## IV Regression w/ Covariates AND Interaction ###########################
##########################################################################################

####################################### First Stage ###############################

# Literacy
lit_fs_pc_1l_cov_int<-lm(fs_pc~ ssmi_w+ssfi_w+spei_w+m_edu+region+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc_1l_cov_int)
lit_fs_pc_age_1l_cov_int<-lm(fs_pc*age~ssmi_w+ssfi_w+spei_w+m_edu+region+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc_age_1l_cov_int)
lit_fs_pc_female_1l_cov_int<-lm(fs_pc*female~ssmi_w+ssfi_w+spei_w+m_edu+region+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,data=full_data_l %>% filter(m_outcome_type=="lit_per"))
summary(lit_fs_pc_age_1l_cov_int)

first_stage_models_cov_int <- list(lit_fs_pc_1l_cov_int,
                                   lit_fs_pc_age_1l_cov_int,
                                   lit_fs_pc_female_1l_cov_int)

stargazer(first_stage_models_cov_int,
          title="2SLS First Stage: Regression of Averaged Food Insecurity on Household Deviance from Optimal Crop Growing Temperature and Precipitation",
          dep.var.caption = "Dependent Variable:",
          covariate.labels=c("SSMI","SSFI","SPEI","Midline Education","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment"),
          dep.var.labels = c("Food Insecurity PC","Food Insecurity PC: Age",
                             "Food Insecurity PC: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/first_stage_cov_int.html")

####################################### Second Stage ###############################

# Literacy
lit_ivreg_full_cov_int<-ivreg(e_edu ~ fs_pc+fs_pc*age+fs_pc*female+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,
                          ~ssmi_w+ssfi_w+spei_w+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="lit_per"))
summary(lit_ivreg_full_cov_int,vcov=sandwich,diagnostics=TRUE)

# Numeracy
num_ivreg_full_cov_int<-ivreg(e_edu ~ fs_pc+fs_pc*age+fs_pc*female+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                          ~ssmi_w+ssfi_w+spei_w+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="num_per"))
summary(num_ivreg_full_cov_int,vcov=sandwich,diagnostics=TRUE)

# EF
ef_ivreg_full_cov_int<-ivreg(e_edu ~ fs_pc+fs_pc*age+fs_pc*female+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                         ~ssmi_w+ssfi_w+spei_w+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="ef_per"))
summary(ef_ivreg_full_cov_int,vcov=sandwich,diagnostics=TRUE)

# SDQ
sdq_ivreg_full_cov_int<-ivreg(e_edu ~ fs_pc+fs_pc*age+fs_pc*female+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment, 
                          ~ssmi_w+ssfi_w+spei_w+m_edu+m_edu+region+age+female+enrolled_in_school +current_class+ private_school+num_books+cg_age +cg_female +marital_status+cg_edu +poverty+num_kids+pe_pc1+treatment,full_data_l%>% filter(m_outcome_type=="sel_per"))
summary(sdq_ivreg_full_cov_int,vcov=sandwich,diagnostics=TRUE)

############################## Exporting Results ###############################

iv_cov_int <- list(lit_ivreg_full_cov_int,num_ivreg_full_cov_int,ef_ivreg_full_cov_int, sdq_ivreg_full_cov_int)

stargazer(iv_cov_int,
          title="IV Regression with Controls and Interactions: Full Data",
          type='latex',
          dep.var.caption = "Endline Dependent Variable:",
          column.labels = c("Literacy","Numeracy", "Executive Function","Socioemotional Learning"),
          covariate.labels=c("Food Insecurity PC","Child Age","Child Female","Midline Education Outcome","Region: Northern","Region: Savannah","Region: Upper East","Region: Upper West", 
                             "Enrolled in School","Current Class", "Attends Private School", "Number of Books",
                             "Caregiver Age","Caregiver Female","Marital Status","Caregiver Education","Poverty", "Number of Kids",
                             "Parental Engagement","Treatment","Food Insecurity PC: Child Age","Food Insecurity PC: Child Female"),
          out="/Users/AllanLee/Desktop/Penn/ECON4900/Output/IV_full_cov_int.html")



