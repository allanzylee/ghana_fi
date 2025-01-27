###################################### Introduction ############################################

# Author: Allan Lee
# Date: January 12, 2025
# Purpose: Investigate whether the rows filtered out are systematically related to any covariates

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
# library(foreign)
library(haven)
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
# library(AER)
# library(GGally)
# library(broom.helpers)
# library(jtools)
library(janitor)
# library(fastDummies)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

outcome <- read_rds("build/outcome_zscore.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))
fi <- read_rds("build/fi.rds")%>% 
  mutate(across(contains('id'),~as.double(.)))
controls <- read_rds("build/controls.rds") %>% 
  clean_names()
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  rename(careid=caseid) %>% 
  filter(io2==1)
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid))
child_order <-read_dta("import/Child Order Dataset_12.15.22.dta") %>% 
  dplyr::select(-community, -region)
baseline_enrollment_reg<-read_dta("import/Enrolment & Caregiver Survey_depii.dta") %>% 
  mutate(careid=as.double(careid))
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
outcome_checker<- read_rds("build/outcome_zscore_checker.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))

##########################################################################################
################################## Putting all data together #############################
##########################################################################################

# Put all data together
full_data_w <- e_child %>% 
  dplyr::select(careid,
                childid,
                age=childage,
                current_class=ed3,
                e_school_type=ed2,
                e_enroll_ch=ed1,
                # e_ch_attend=ed7b,
                language=io1,
                region,
                e_ch_health=cw1,
                e_ch_health_rel=cw2,
                e_ch_edu_asp=ja3
  ) %>% 
  dplyr::left_join(e_cg %>% dplyr::select(careid, 
                                          childid, 
                                          e_enroll_cg=cr7,
                                          e_attend=cr8,
                                          female=cr3,
                                          #cg_edu=cb3,
                                          marital_status=cb5,
                                          # num_books=pe7,
                                          treatment,
                                          contains('gb'),
                                          e_cg_edu_asp=ea1
  ),
  by=c("childid",'careid')) %>% 
  dplyr::inner_join(outcome %>% dplyr::select(childid, 
                                              careid, 
                                              contains('per')),
                    by=c("childid","careid")) %>% 
  dplyr::left_join(controls %>% rename(
    pe_pc1=pc1,
    pe_pc2=pc2,
    pe_pc3=pc3,
    pe_pc4=pc4,),
    by=c("childid","careid")) %>% 
  dplyr::left_join(fi %>% 
                     dplyr::select(childid,
                                   careid,
                                   contains('fs_dummy'),
                                   contains('fies')),
                   by=c("childid","careid")) %>%
  dplyr::left_join(child_order %>% dplyr::select(childid,
                                                 careid,
                                                 poverty=mid_reverse_ppi
                                                 #,num_kids
  ),
  by=c("childid","careid")) %>%
  # dplyr::left_join(iv %>% dplyr::select(-region),
  #                  by=c("careid","childid")) %>% 
  dplyr::left_join(baseline_enrollment_reg %>% 
                     dplyr::select(careid,
                                   hh_size=ps1,
                                   cg_schooling=hr10),
                   by=c('careid')) %>% 
  left_join(m_child %>%
              dplyr::select(careid,
                            childid,
                            m_ch_health=cw1,
                            m_ch_health_rel=cw2,
                            m_enroll_ch=ed1,
                            m_school_type=ed2
                            # m_ch_attend=ed7b
              ),
            by=c('childid','careid')
  ) %>%
  left_join(m_cg %>%
              dplyr::select(careid,
                            childid,
                            m_enroll_cg=cr7,
                            m_attend=cr8,
                            cg_age=cb1,
                            cg_female=cb2,
              ),
            by=c('childid','careid')
  ) %>%
  # Adjust variables to become ordinal/binary
  mutate(across(contains('enroll_ch'),~case_when(.!=1~0,
                                                 T~1)),
         female=if_else(female==1,0,1),
         cg_female=if_else(cg_female==1,0,1),
         m_public_school=case_when(m_school_type==1~1,
                                   T~0),
         m_private_school=case_when(m_school_type==2~1,
                                    is.na(m_school_type)~0,
                                    T~0),
         e_public_school=case_when(e_school_type==1~1,
                                   T~0),
         e_private_school=case_when(e_school_type==2~1,
                                    is.na(e_school_type)~0,
                                    T~0),
         across(contains('school_type'),~if_else(.==1,0,1)),
         marital_status=case_when(marital_status==3~1,
                                  marital_status==4~1,
                                  T~0),
         age_num=as.double(age),
         age=if_else((age>=5 & age <=9),0,1),
         treatment=case_when(treatment>0 ~ 1,
                             T~0),
         current_class=as.double(current_class),
         language=case_when(language %in% c('Dagaari','Dagaari, Wali','English','TWI')~"Other",
                            T~language),
         current_class=as.factor(case_when(current_class<0~NA_real_,
                                           T~current_class)),
         region=case_when(region==""~"Northern",
                          T~region),
         across(contains('health'),~as.factor(case_when(as.double(.)<0~NA_real_,
                                                        T~as.double(.)))),
         across(contains('attend'),~as.factor(.)),
         e_ch_edu_asp=case_when(e_ch_edu_asp>=5~1,
                                is.na(e_ch_edu_asp) ~ NA_real_, 
                                T~0),
         e_cg_edu_asp=case_when(e_cg_edu_asp>=3~1,
                                is.na(e_cg_edu_asp) ~ NA_real_, 
                                T~0)
  ) %>%
  # Create binary variables for health
  mutate(across(contains('health'),
                ~as.double(case_when(.==4~1,
                                     .==5~1,
                                     .==3~0,
                                     .==2~0,
                                     .==1~0,
                                     T~NA_real_)),
                .names = "{col}_dummy"
  )) %>% 
  filter(
    !is.na(female) &
    !is.na(age) &
    !is.na(e_ch_fs_dummy)&
    !is.na(e_cg_fs_dummy)&
    !is.na(m_ch_fs_dummy)&
    !is.na(m_cg_fs_dummy)&
    !is.na(treatment)&
    !is.na(m_lit_per)&
    !is.na(m_num_per)&
    !is.na(m_sel_per)&
    !is.na(m_ef_per)&
    !is.na(e_lit_per)&
    !is.na(e_num_per)&
    !is.na(e_sel_per)&
    !is.na(e_ef_per)
  ) %>%
  # left_join(num_kids,by=c('careid')) %>%
  fastDummies::dummy_cols(select_columns='region') %>%
  clean_names() %>% 
  # Filter to extended model build
  filter(age==1) %>% 
  mutate(missing=case_when(!is.na(e_ch_health)&
                           !is.na(e_enroll_ch)&
                           !is.na(e_private_school)&
                           !is.na(e_cg_edu_engagement)&
                           !is.na(e_ch_motiv)&
                           !is.na(e_cg_edu_asp)&
                           !is.na(e_cg_emotional_engagement)~0,
                           T~1))

# Regress missingness on child sex, age, caregiver has education, caregiver age, caregiver gender, poverty status, region, pnp
reg<-glm(missing ~ female + cg_schooling + cg_age + cg_female + poverty + treatment+region_north_east+region_northern+region_upper_east+region_upper_west+e_ch_health+e_enroll_ch+e_private_school+e_cg_edu_engagement+e_ch_motiv+e_cg_edu_asp+e_cg_emotional_engagement,
         data = full_data_w)
summary(reg)

# Export results
stargazer(reg,
          title="Missingness Regression",
          #dep.var.caption = "Endline Dependent Variable:",
          # covariate.labels=variables,
          column.labels = c("Child is Removed from Data"),
          covariate.labels=c("Child is 10–17",
                             "Child is Female",
                             "Caregiver Attended Primary School",
                             "Caregiver Age",
                             "Caregiver is Female",
                             "Poverty",
                             "PNP Treatment",
                             "Region: North East",
                             "Region: Northern",
                             "Region: Upper East",
                             "Region: Upper West"),
          star.cutoffs = c(.05, .01, NA),
          notes.append     = FALSE,
          notes            = "*$p<0.05$; **$p<0.01$",
          out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/4_missing_data_extended_model.html")
