###################################### Introduction ############################################

# Author: Allan Lee
# Date: Feb 12th, 2023
# Purpose: clean outcome data for analysis. Replace SEL with SDQ data to measure child behavioral outcomes.

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)


# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.)))
# child_order <-read_dta("import/Child Order Dataset_12.15.22.dta") %>% 
#   mutate(across(contains('id'),~as.double(.)))

# Create child age variable
child_age <- e_child %>% 
  dplyr::select(careid,childid,childage) %>% 
  rename(child_age=childage)

##########################################################################################
###################################### Outcome data cleaning #############################
##########################################################################################

############################################### Midline: SEL ######################################

m_sel <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         cr1:re11) %>%

  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Change CR SEL questions to be 1/0 binary
  mutate(cr1=ifelse(cr1==2,0,1),
         cr2=ifelse(cr2==2,0,1),
         cr3=ifelse(cr3==2,0,1),
         cr4=ifelse(cr4==2,0,1)) %>% 
  # Add child age variable
  left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_sel_total=(rowSums(across(c(cr1:re11)),na.rm=T)),
         m_num_sel_questions=ifelse(child_age<10,10,13),
         m_sel_per=m_sel_total/m_num_sel_questions) %>%
  # Modify the question count for those whose age do not match their question count
  mutate(m_num_sel_questions=ifelse(m_sel_per>1,13,m_num_sel_questions),
         m_sel_per=m_sel_total/m_num_sel_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,child_age,m_sel_total,m_num_sel_questions,m_sel_per)

############################################### Midline Literacy ######################################
# Note that within the literacy category, there are 168 questions.

m_lit <- m_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
         childid,
         starts_with("ov"),
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         starts_with("pa"),
         -starts_with("paa")) %>% 
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>% 
  # Change any negative values to 0
  mutate(ov2=ifelse(ov2<0,0,ov2)) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_lit_total=(rowSums(across(c(ov1:pa3)),na.rm=T)),
         m_num_lit_questions=168,
         m_lit_per=m_lit_total/m_num_lit_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_lit_total,m_num_lit_questions,m_lit_per)
  
############################################### Midline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

m_num <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         starts_with("co"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         starts_with("su"),
         starts_with("mu"),
         starts_with("di"),
         -community,
         -submissiondate,
         -district) %>%
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_num_total=(rowSums(across(c(co1:di1)),na.rm=T)),
         m_num_num_questions=53,
         m_num_per=m_num_total/m_num_num_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_num_total,m_num_num_questions,m_num_per)

############################################### Midline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

m_ef <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         starts_with("wm"),
         starts_with("sm")) %>%
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Add child age variable
  left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_ef_total=(rowSums(across(c(wm1:sm7)),na.rm=T)),
         m_num_ef_questions=ifelse(child_age<10,15,17),
         m_ef_per=m_ef_total/m_num_ef_questions) %>%
  # Keep relevant variables
  dplyr::select(careid,childid,child_age,m_ef_total,m_num_ef_questions,m_ef_per)

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         cr1:re11) %>%
  # Dedplyr::select friends question
  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Change CR SEL questions to be 1/0 binary
  mutate(cr1=ifelse(cr1==2,0,1),
         cr2=ifelse(cr2==2,0,1),
         cr3=ifelse(cr3==2,0,1),
         cr4=ifelse(cr4==2,0,1)) %>% 
  # Add child age variable
  left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_sel_total=(rowSums(across(c(cr1:re11)),na.rm=T)),
         e_num_sel_questions=ifelse(child_age<10,10,13),
         e_sel_per=e_sel_total/e_num_sel_questions) %>%
  # Modify the question count for those whose age do not match their question count
  mutate(e_num_sel_questions=ifelse(e_sel_per>1,13,e_num_sel_questions),
         e_sel_per=e_sel_total/e_num_sel_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,child_age,e_sel_total,e_num_sel_questions,e_sel_per)

############################################### Endline Literacy ######################################
# Note that within the literacy category, there are 168 questions.

e_lit <- e_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
         childid,
         starts_with("ov"),
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         starts_with("pa"),
         -starts_with("paa")) %>% 
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_lit_total=(rowSums(across(c(ov1:pa3)),na.rm=T)),
         e_num_lit_questions=168,
         e_lit_per=e_lit_total/e_num_lit_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_lit_total,e_num_lit_questions,e_lit_per)

############################################### Endline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

e_num <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         starts_with("co"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         starts_with("su"),
         starts_with("mu"),
         starts_with("di"),
         -submissiondate,
         -district) %>%
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_num_total=(rowSums(across(c(co1:di1)),na.rm=T)),
         e_num_num_questions=53,
         e_num_per=e_num_total/e_num_num_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_num_total,e_num_num_questions,e_num_per)

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         childage,
         starts_with("wm"),
         starts_with("sm")) %>%
  # Change all columns to numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Add child age data
  left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_ef_total=(rowSums(across(c(wm1:sm7)),na.rm=T)),
         e_num_ef_questions=ifelse(child_age<10,15,17),
         e_num_ef_questions=ifelse(childid==5234501201,17,e_num_ef_questions),
         e_ef_per=e_ef_total/e_num_ef_questions) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,child_age,e_ef_total,e_num_ef_questions,e_ef_per)

############################################### Putting outcome data together ######################################
  
outcome <- m_lit %>% 
  inner_join(m_sel,by=c("childid","careid")) %>% 
  inner_join(m_num,by=c("childid","careid")) %>% 
  inner_join(m_ef,by=c("childid","careid")) %>% 
  inner_join(e_sel,by=c("childid","careid")) %>% 
  inner_join(e_lit,by=c("childid","careid")) %>% 
  inner_join(e_num,by=c("childid","careid")) %>% 
  inner_join(e_ef,by=c("childid","careid"))

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

saveRDS(outcome, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome.rds")


