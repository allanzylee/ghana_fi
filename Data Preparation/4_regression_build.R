###################################### Introduction ############################################

# Author: Allan Lee
# Date: 2025.7.22
# Purpose: Create build used for all regressions

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Load header
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

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
  mutate(across(contains('fs'),~as.double(.))) %>% 
  rename(careid=caseid) %>% 
  filter(io2==1)
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid)) %>% 
  mutate(across(contains('fs'),~as.double(.)))
# child_order <-read_dta("import/Child Order Dataset_12.15.22.dta") %>% 
#   dplyr::select(-community, -region)
# baseline_enrollment_reg<-read_dta("import/Enrolment & Caregiver Survey_depii.dta") %>% 
#   mutate(careid=as.double(careid))
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
outcome_checker<- read_rds("build/outcome_zscore_checker.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))
outcome_raw<- read_rds("build/outcome_raw.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))


##########################################################################################
################################## Putting all data together #############################
##########################################################################################

# Put all data together
full_data_w <- e_child %>% 
  dplyr::select(careid,
         childid,
         age=childage,
         e_school_type=ed2,
         region,
         e_ch_health=cw1,
         e_ch_health_rel=cw2,
         e_ch_edu_asp=ja3,
         contains('fs'),
         treatment
         ) %>% 
  rename_with(~ paste0(., "_child"), .cols = matches("^fs\\d+$")) %>% 
  mutate(across(contains('fs'),~case_when(. == 1 ~ 2,
                                          . == 2 ~ 1,
                                          . == 3 ~ 0,
                                          TRUE ~ NA_real_))) %>%
  left_join(outcome_checker,
            by=c('childid')) %>% 
  left_join(outcome_raw %>% 
              dplyr::select(childid,careid,contains('raw')),
            by=c('childid',
                 'careid'),
            suffix = c("",
                       '_checker')) %>% 
  dplyr::left_join(e_cg %>% dplyr::select(careid, 
                                    childid, 
                                    e_enroll_cg=cr7,
                                    e_attend=cr8,
                                    female=cr3,
                                    contains('fs')
                                    )%>% 
                    rename_with(~ paste0(., "_cg"), .cols = matches("^fs\\d+$")),
                    by=c("childid",'careid')) %>% 
  dplyr::select(-matches("^fs\\d+$")) %>% 
  # dplyr::inner_join(outcome %>% dplyr::select(childid, 
  #                                     careid, 
  #                                     contains('per')),
  #                  by=c("childid","careid")) %>% 
  dplyr::left_join(controls,
                   by=c("childid","careid")) %>% 
  dplyr::left_join(fi,
                   by=c("childid")
  ) %>%
  # Adjust variables to become ordinal/binary
  mutate(across(contains('enroll_ch'),~case_when(.!=1~0,
                                                 T~1)),
         female=if_else(female==1,0,1),
         e_private_school=case_when(e_school_type==2~1,
                                    is.na(e_school_type)~0,
                                   T~0),
         across(contains('school_type'),~if_else(.==1,0,1)),
         age_num=as.double(age),
         age=if_else((age>=5 & age <=9),0,1),
         treatment=case_when(treatment>0 ~ 1,
                             T~0),
         region=case_when(region==""~"Northern",
                          T~region),
         across(contains('health'),~as.factor(case_when(as.double(.)<0~NA_real_,
                                              T~as.double(.)))),
         across(contains('attend'),~as.factor(.)),
         e_ch_edu_asp=case_when(e_ch_edu_asp>=5~1,
                                is.na(e_ch_edu_asp) ~ NA_real_, 
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
  # Standardize investment mechanisms
  mutate(across(c(
                e_ch_motiv,
                # e_ch_edu_asp,
                e_cg_emotional_engagement),
         ~scale(.)[,1])) %>%
  # Filter out NAs
  filter(
    !is.na(female),
    !is.na(age),
    !is.na(e_cfies_sum),
    !is.na(e_fies_sum),
    !is.na(treatment),
    !is.na(m_lit_per),
    !is.na(m_num_per),
    !is.na(m_sel_per),
    !is.na(m_ef_per),
    !is.na(e_lit_per),
    !is.na(e_num_per),
    !is.na(e_sel_per),
    !is.na(e_ef_per)
         ) %>%
  # left_join(num_kids,by=c('careid')) %>%
  fastDummies::dummy_cols(select_columns='region') %>% 
  clean_names()

# # Create long version of the data
# 
# full_data_l <- full_data_w %>% 
#   # Pivot the data such that education columns only represent midline and endline education outcomes
#   pivot_longer(cols=c('m_sel_per','m_lit_per','m_ef_per',"m_num_per"),
#                names_to="m_outcome_type",
#                values_to="m_edu") %>% 
#   pivot_longer(cols=c('e_sel_per','e_lit_per','e_ef_per',"e_num_per"),
#                names_to="e_outcome_type",
#                values_to="e_edu") %>% 
#   # Mutate the data such that education type columns are the same names. Then filter for rows with same education type
#   mutate(m_outcome_type = substr(m_outcome_type, 3, nchar(m_outcome_type)),
#          e_outcome_type = substr(e_outcome_type, 3, nchar(e_outcome_type))) %>% 
#   filter(m_outcome_type==e_outcome_type)

# Export
saveRDS(full_data_w %>% dplyr::select(-contains('checker')), "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds")
# saveRDS(full_data_l, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_l.rds")

# write_csv(full_data_w,"/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.csv")

