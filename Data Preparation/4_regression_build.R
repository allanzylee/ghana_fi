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
child_order <-read_dta("import/Child Order Dataset_12.15.22.dta") %>%
  dplyr::select(childid, ch_rank=rank, num_kids)
# baseline_enrollment_reg<-read_dta("import/Enrolment & Caregiver Survey_depii.dta") %>% 
#   mutate(careid=as.double(careid))
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1) %>% 
  select(careid,
         childid,
         m_ch_health=cw1,
         m_school_type=ed2,
         m_ch_edu_asp=ja3,
         )

m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  mutate(cg_female=case_when(cb2==2~1,
                             T~0),
         edu_rowmax=pmax(cb3,cb4,na.rm=T),
         cg_primary=case_when(edu_rowmax>=2 & edu_rowmax<=5~1,
                              T~0))
outcome_checker<- read_rds("build/outcome_zscore_checker.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))
outcome_raw<- read_rds("build/outcome_raw.rds") %>% 
  mutate(across(contains('id'),~as.double(.)))
# Calculate child rank variable
e_household <- read_dta("import/01_PNP_Endline_HouseholdSurvey.dta") %>%
  mutate(
    across(starts_with("cr0_"), as.double),
    across(starts_with("cr6_"), as.double),
    careid = as.double(careid)
  ) %>%
  select(careid, starts_with("cr0_"), starts_with("cr6_")) %>%
  pivot_longer(
    cols = c(starts_with("cr0_"), starts_with("cr6_")),
    names_to = c(".value", "child_num"),
    names_pattern = "cr(0|6)_(.*)"
  ) %>%
  rename(
    childid   = `0`,
    child_age = `6`
  ) %>%
  select(careid, childid, child_age) %>% 
  arrange(careid,child_age) %>% 
  group_by(careid) %>% 
  mutate(
    age_pct_rank = percent_rank(child_age)
  ) %>%
  ungroup()

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
         treatment_raw=treatment,
         startdate
         ) %>% 
  left_join(m_child,by=c('childid','careid')) %>% 
  rename_with(~ paste0(., "_child"), .cols = matches("^fs\\d+$")) %>% 
  mutate(across(contains('fs'),~case_when(. == 1 ~ 2,
                                          . == 2 ~ 1,
                                          . == 3 ~ 0,
                                          TRUE ~ NA_real_)),
         year=year(startdate),
         month=month(startdate)) %>%
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
                   by=c("childid")) %>% 
  dplyr::left_join(fi,
                   by=c("childid")
  ) %>%
  dplyr::left_join(m_cg %>% 
                     select(cg_age=cb1,
                            cg_female,
                            cg_primary,
                            childid,
                            m_attend=cr8,),
                   by=c("childid")
  ) %>%
  dplyr::left_join(child_order,
                   by=c("childid")
  
  ) %>%
  dplyr::left_join(e_household %>% select(childid,age_pct_rank),
                   by=c("childid")) %>% 
  # Adjust variables to become ordinal/binary
  mutate(across(contains('enroll_ch'),~case_when(.!=1~0,
                                                 T~1)),
         female=if_else(female==1,0,1),
         e_private_school=case_when(e_school_type==2~1,
                                    is.na(e_school_type)~0,
                                   T~0),
         m_private_school=case_when(m_school_type==2~1,
                                    is.na(m_school_type)~0,
                                    T~0),
         across(contains('school_type'),~if_else(.==1,0,1)),
         age_num=as.double(age),
         age=if_else((age>=5 & age <=9),0,1),
         treatment=case_when(treatment_raw>0 ~ 1,
                             T~0),
         region=case_when(region==""~"Northern",
                          T~region),
         across(contains('health'),~as.factor(case_when(as.double(.)<0~NA_real_,
                                              T~as.double(.)))),
         across(contains('attend'),~as.factor(.)),
         e_ch_edu_asp=case_when(e_ch_edu_asp>=5~1,
                                is.na(e_ch_edu_asp) ~ NA_real_, 
                                T~0),
         m_ch_edu_asp=case_when(m_ch_edu_asp>=5~1,
                                is.na(m_ch_edu_asp) ~ NA_real_, 
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
                m_ch_motiv,
                e_cg_emotional_engagement,
                m_cg_emotional_engagement),
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
    !is.na(e_ef_per),
    !is.na(age_pct_rank)
         ) %>%
  # left_join(num_kids,by=c('careid')) %>%
  fastDummies::dummy_cols(select_columns='region') %>% 
  clean_names()

# Export
saveRDS(full_data_w %>% dplyr::select(-contains('checker')), "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds")



