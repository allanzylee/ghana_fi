###################################### Introduction ############################################

# Author: Allan Lee
# Date: 2025.7.22
# Purpose: Calculate sum stat between excluded and non-excluded children 

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
  # left_join(num_kids,by=c('careid')) %>%
  fastDummies::dummy_cols(select_columns='region') %>% 
  clean_names() %>% 
  # Denote whether a child is excluded or not
  mutate(missing=case_when(
    !is.na(female) &
      !is.na(age) &
      !is.na(e_cfies_indicator)&
      !is.na(e_fies_indicator)&
      !is.na(treatment)&
      !is.na(m_lit_per)&
      !is.na(m_num_per)&
      !is.na(m_sel_per)&
      !is.na(m_ef_per)&
      !is.na(e_lit_per)&
      !is.na(e_num_per)&
      !is.na(e_sel_per)&
      !is.na(e_ef_per) ~ 0,
    T~1
  ))

# Keep relevant columns for summary stat calculation

summary_stat<-full_data_w %>% 
  dplyr::select(
    e_cfies_indicator,
    e_fies_indicator,
    female,
    age,
    age_num,
    contains('region_'),
    contains('per')
  ) %>% 
  select(-contains('raw'))

######################################## T-Test: Create function for group based summary statistics ##############################

t_test_func<-function(var,
                      group){
  
  exp <- expr(!!ensym(var) ~ !!ensym(group))
  
  test <- t.test(formula = eval(exp),
                 data=full_data_w)
    
    out<-tribble(~var,~not_excluded,~excluded,~pval_excluded,
                 var,test$estimate[1],test$estimate[2],test$p.value)
  
  return(out)
  
}

# Create function input
input<-names(summary_stat) %>% 
  as_tibble() %>% 
  rename(var=value) %>% 
  mutate(group='missing'
  )

# Run function
missing_summary_stat<-pmap_dfr(input,
                              t_test_func)

# Combine
out<-missing_summary_stat %>% 
  mutate(across(c('excluded','not_excluded'),~case_when(.==0~NA,
                                                T~.))) %>% 
  filter(var!='age',
         !str_detect(var,'^m_.*.per$')) %>% 
  rename('Excluded'=excluded,
    'Not Excluded'=not_excluded,
    'P-Value: Exclusion'=pval_excluded) %>% 
  mutate(var=case_when(var=='e_cfies_indicator'~'Child-Reported FI (%)',
                       var=='e_fies_indicator'~'Caregiver-Reported FI (%)',
                       var=='female'~'Child is Female (%)',
                       var=='age_num'~'Child Age (Years)',
                       var=='region_north_east'~"Region: North East",
                       var=='region_northern'~"Region: Northern",
                       var=='region_savannah'~"Region: Savannah",
                       var=='region_upper_east'~"Region: Upper East",
                       var=='region_upper_west'~"Region: Upper West",
                       var=='e_lit_per'~'Endline Literacy Z-Score',
                       var=='e_num_per'~'Endline Numeracy Z-Score',
                       var=='e_ef_per'~'Endline EF Z-Score',
                       var=='poverty'~'Poverty',
                       var=='cg_schooling'~'Caregiver Attended Primary School',
                       var=='cg_age'~'Caregiver age',
                       var=='cg_female'~'Caregiver is Female',
                       T~'Endline SEL Z-Score',
  )
  ) %>% 
  rename('Statistic'=var)

# Export
write_xlsx(out,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/07_missing_data_sum_stat.xlsx")

# Create Latex Table
latex=xtable(out, 
             type = "latex")

names(latex)=c("\\multicolumn{1}{p{2.5in}}{Statistic}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Not Excluded}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Excluded}",
               "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Exclusion}"
)

print(latex, sanitize.colnames.function=function(x){x},
      include.rownames=FALSE)

