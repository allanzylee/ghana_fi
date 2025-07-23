###################################### Introduction ############################################

# Author: Allan Lee
# Date: Feb 12th, 2023
# Purpose: clean outcome data for analysis. Replace SEL with SDQ data to measure child behavioral outcomes.

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Load header
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) 
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.)))

#####################################################################################################
#####################################################################################################
#####################################################################################################

############################################### Midline: SEL ######################################

m_sel <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         child_age=cr6,
         cr1:re11,
         enum_id,
         treatment) %>%
  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.numeric(.))) %>% 
  # Change CR SEL questions to be 1/0 binary
  mutate(across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_sel_total=(rowSums(across(-c(careid,childid,child_age, enum_id, treatment)),na.rm=T)),
         total_q = ifelse(child_age<10,11,14),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_sel_per=case_when(answered_q==0~NA_real_,
                             T~m_sel_total/total_q)) %>% 
  mutate(m_sel_per=case_when(m_sel_per>1~1,
                             T~m_sel_per)) %>% 
  # Filter out participants whose question count don't match their age
  # filter(!(child_age<10 & (!is.na(re9)|!is.na(re10)|!is.na(re11)))) %>% 
  dplyr::select(careid,childid,child_age,m_sel_total,answered_q_perc,m_sel_per, enum_id, treatment)
  

############################################### Midline Literacy ######################################
# Note that within the literacy category, there are 168 questions.

m_lit <- m_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
         childid,
         child_age=cr6,
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         matches("pa[0-9]"),
         enum_id,
         treatment) %>%
  dplyr::select(-oc5) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # # Change any negative values to 0
  # mutate(ov2=ifelse(ov2<0,0,ov2)) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_lit_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_lit_per=case_when(answered_q==0~NA_real_,
                             T~m_lit_total/total_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_lit_total,answered_q_perc,m_lit_per,enum_id,treatment)
  
############################################### Midline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

m_num <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         matches("co[0-9]"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         matches("su[0-9]"),
         starts_with("mu"),
         matches("di[0-9]"),
         child_age=cr6,
         enum_id,
         treatment) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_num_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_per=case_when(answered_q==0~NA_real_,
                             T~m_num_total/total_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_num_total,answered_q_perc,m_num_per,enum_id,treatment)

############################################### Midline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

m_ef <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         starts_with("wm"),
         starts_with("sm"),
         child_age=cr6,
         enum_id,
         treatment) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # # Add child age variable
  # left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_ef_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ifelse(child_age<10,15,17),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         m_ef_per=m_ef_total/total_q) %>%
  # Filter out participants whose question count don't match their age
  # filter(!(child_age<10 & (!is.na(sm6)|!is.na(sm7)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_ef_total,answered_q_perc,m_ef_per,enum_id,treatment)

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         cr1:re11,
         child_age=childage,
         enum_id,
         treatment) %>%
  # Dedplyr::select friends question
  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Change CR SEL questions to be 1/0 binary
  mutate(across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_sel_total=(rowSums(across(-c(careid,childid,child_age, enum_id, treatment)),na.rm=T)),
         total_q = ifelse(child_age<10,10,13),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         e_sel_per=e_sel_total/total_q) %>%
  # filter(!(child_age<10 & (!is.na(re9)|!is.na(re10)|!is.na(re11)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_sel_total,answered_q_perc,e_sel_per, enum_id, treatment)

############################################### Endline Literacy ######################################

e_lit <- e_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
                childid,
                # starts_with("ov"),
                starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]"),
                child_age=childage,
                enum_id,
                treatment) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_lit_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_lit_per=case_when(answered_q==0~NA_real_,
                             T~e_lit_total/total_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_lit_total,answered_q_perc,e_lit_per,enum_id,treatment)

############################################### Endline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

e_num <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"),
                child_age=childage,
                enum_id,
                treatment) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_num_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_num_per=case_when(answered_q==0~NA_real_,
                             T~e_num_total/total_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_num_total,answered_q_perc,e_num_per,enum_id,treatment)

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         child_age=childage,
         starts_with("wm"),
         starts_with("sm"),
         enum_id,
         treatment) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Add child age data
  # left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_ef_total=(rowSums(across(-c(careid,childid,child_age,enum_id,treatment)),na.rm=T)),
         total_q = ifelse(child_age<10,15,17),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         e_ef_per=e_ef_total/total_q) %>%
  # filter(!(child_age<10 & (!is.na(sm6)|!is.na(sm7)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_ef_total,answered_q_perc,e_ef_per,enum_id,treatment)

##########################################################################################
################################## Standardize outcome data ###############################
##########################################################################################

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
    dplyr::select(careid,
           childid,
           !!quo_name(outcome) := z_score)
  
  return(out)
  
}

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

# Create data for export
outcome <- m_lit_z %>% 
  left_join(m_sel_z,by=c("childid","careid")) %>% 
  left_join(m_num_z,by=c("childid","careid")) %>% 
  left_join(m_ef_z,by=c("childid","careid")) %>% 
  left_join(e_sel_z,by=c("childid","careid")) %>% 
  left_join(e_lit_z,by=c("childid","careid")) %>% 
  left_join(e_num_z,by=c("childid","careid")) %>% 
  left_join(e_ef_z,by=c("childid","careid"))

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

saveRDS(outcome, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_zscore_checker.rds")

outcome_b<-read_rds("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome_zscore.rds")

comp=rCompare(outcome,
         outcome_b,
         keys=c('childid',
                'careid'))
summary(comp)
