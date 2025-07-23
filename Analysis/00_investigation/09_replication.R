###################################### Introduction ############################################

# Author: Allan Lee
# Date: July 20, 2025
# Purpose: Calculate midline vs endline FI correlation

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Load header
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

# Load std function
std_outcome<-function(df_str){
  
  outcome<-paste0(df_str,"_per")
  df<-get(df_str) %>% 
    filter(!is.na(!!sym(outcome)),
           !is.na(enum_id))
  
  # Calculate outcome scores net of enumerator effects  
  fm=as.formula(glue('{outcome}~factor(enum_id)'))
  model <- lm(fm, data = df)
  df[['res']] <- resid(model)
  
  # Calculate control group means and SDs of residuals
  mean_res <- df %>% 
    filter(treatment==0) %>% 
    summarise(mean=mean(res)) %>% 
    pull()
  
  sd_res <- df %>% 
    filter(treatment==0) %>% 
    summarise(sd=sd(res)) %>% 
    pull()
  
  # Add back mean and sd
  out<-df %>% 
    mutate(mean=mean_res,
           sd=sd_res) %>% 
    mutate(z_score=(res-mean)/sd) %>% 
    dplyr::select(
                  childid,
                  !!quo_name(outcome) := z_score)
  
  return(out)
  
}

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  dplyr::select(-contains("gb")) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  mutate(across(contains('fs'),~as.double(.))) %>% 
  rename(careid=caseid) %>% 
  filter(io2==1) %>% 
  mutate(childage=as.double(childage))
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate(careid=as.double(careid),
         childid=as.double(childid)) %>% 
  mutate(across(contains('fs'),~as.double(.)))
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))

### Clean data
e_child_fs<-e_child %>% 
  select(childid,
         careid,
         matches('fs[0-9]'),
         treatment,
         childage,
         region,
         ) %>% 
  mutate(across(contains('fs'),~as.double(case_when(.==1~2,
                                          .==2~1,
                                          .==3~0,
                                          T~NA_real_)))) %>% 
  mutate(e_cfies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>% 
  mutate(e_cfies_indicator=case_when(e_cfies_sum>=7~1,
                                     T~0),
         e_cfies_scale=as.factor(case_when(e_cfies_sum==0~0,
                                 e_cfies_sum>=1 & e_cfies_sum<=6~1,
                                 e_cfies_sum>=7 & e_cfies_sum<=10~2,
                                 T~3))) %>% 
  select(-matches('fs[0-9]')) %>% 
  filter(region!="") %>% 
  fastDummies::dummy_cols('region') %>% 
  clean_names() %>% 
  filter(!is.na(e_cfies_sum))

e_cg_fs<-e_cg %>% 
  select(childid,
         careid,
         matches('fs[0-9]'),
         female=cr3) %>% 
  mutate(e_fies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>% 
  mutate(e_fies_indicator=case_when(e_fies_sum>=4~1,
                                     T~0),
         e_fies_scale=as.factor(case_when(e_fies_sum<=3~0,
                                           e_fies_sum>=4 & e_fies_sum<=6~1,
                                           T~2)),
         female=case_when(female==2~1,
                          T~0)) %>% 
  select(-matches('fs[0-9]'))
  
e_lit<-e_child %>% 
  select(childid,
         childage,
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         matches("pa[0-9]"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~138,
                           T~138)) %>% 
  mutate(e_lit_per=sum_correct/total_q) %>% 
  select(childid,e_lit_per,enum_id,treatment)

m_lit<-m_child %>% 
  select(childid,
         childage=cr6,
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         matches("pa[0-9]"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~138,
                           T~138)) %>% 
  mutate(m_lit_per=sum_correct/total_q) %>% 
  select(childid,m_lit_per,enum_id,treatment)

e_num<-e_child %>% 
  select(childid,
         childage,
         matches("co[0-9]"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         matches("su[0-9]"),
         starts_with("mu"),
         matches("di[0-9]"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~53,
                           T~47)) %>% 
  mutate(e_num_per=sum_correct/total_q) %>% 
  select(childid,e_num_per,enum_id,treatment)

m_num<-m_child %>% 
  select(childid,
         childage=cr6,
         matches("co[0-9]"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         matches("su[0-9]"),
         starts_with("mu"),
         matches("di[0-9]"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~53,
                           T~47)) %>% 
  mutate(m_num_per=sum_correct/total_q) %>% 
  select(childid,m_num_per,enum_id,treatment)

e_ef<-e_child %>% 
  select(childid,
         childage,
         starts_with("wm"),
         starts_with("sm"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~17,
                           T~11)) %>% 
  mutate(e_ef_per=sum_correct/total_q) %>% 
  select(childid,e_ef_per,enum_id,treatment)

m_ef<-m_child %>% 
  select(childid,
         childage=cr6,
         starts_with("wm"),
         starts_with("sm"),
         enum_id,
         treatment) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>% 
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~17,
                           T~11)) %>% 
  mutate(m_ef_per=sum_correct/total_q) %>% 
  select(childid,m_ef_per,enum_id,treatment)


m_sel<-m_child %>% 
  select(childid,
         childage=cr6,
         cr1:re11,
         enum_id,
         treatment) %>% 
  dplyr::select(-re5,-re8) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~13,
                           T~10)) %>% 
  mutate(m_sel_per=sum_correct/total_q) %>% 
  mutate(m_sel_per=case_when(m_sel_per>1~m_sel_per*10/13,
                            T~m_sel_per)) %>% 
  select(childid,m_sel_per,enum_id,treatment)

e_sel<-e_child %>% 
  select(childid,
         childage,
         cr1:re11,
         enum_id,
         treatment) %>% 
  dplyr::select(-re5,-re8) %>% 
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~13,
                           T~10)) %>% 
  mutate(e_sel_per=sum_correct/total_q) %>% 
  mutate(e_sel_per=case_when(e_sel_per>1~e_sel_per*10/13,
                            T~e_sel_per)) %>% 
  select(childid,e_sel_per,enum_id,treatment)

############################################### Putting outcome data together ######################################

# Run z-score function
m_lit_z=std_outcome('m_lit')
m_sel_z=std_outcome('m_sel')
m_num_z=std_outcome('m_num')
m_ef_z=std_outcome('m_ef')
e_lit_z=std_outcome('e_lit')
e_sel_z=std_outcome('e_sel')
e_num_z=std_outcome('e_num')
e_ef_z=std_outcome('e_ef')

############################################### Regression ######################################
df<-e_child_fs %>% 
  left_join(e_cg_fs,
            by=c('childid','careid')) %>% 
  left_join(e_lit_z,
            by=c('childid')) %>% 
  left_join(m_lit_z,
            by=c('childid')) %>% 
  left_join(e_num_z,
            by=c('childid')) %>% 
  left_join(m_num_z,
            by=c('childid')) %>% 
  left_join(e_ef_z,
            by=c('childid')) %>% 
  left_join(m_ef_z,
            by=c('childid')) %>% 
  left_join(e_sel_z,
            by=c('childid')) %>% 
  left_join(m_sel_z,
            by=c('childid')) %>% 
  filter(!is.na(female),
         !is.na(childage),
         !is.na(e_cfies_sum),
         !is.na(e_fies_sum),
         !is.na(treatment),
         !is.na(m_lit_per),
         !is.na(m_num_per),
         !is.na(e_lit_per),
         !is.na(e_num_per),
         !is.na(region_upper_west),
         !is.na(region_north_east),
         !is.na(region_northern),
         !is.na(region_savannah),
         !is.na(region_upper_east)) %>% 
  mutate(age_group=case_when(childage>=10~1,
                             T~0))



# Reg
cat='sel'
fi='scale'
int='female'

formula=as.formula(glue('e_{cat}_per~ m_{cat}_per+e_cfies_{fi}+e_fies_{fi}+female+childage+treatment+region_north_east+region_northern+region_upper_east+region_upper_west'))
reg=feols(formula,
          data=df,
          cluster=~careid)
summary(reg)






