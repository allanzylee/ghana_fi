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
library(haven)

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(io2==1)

###################################### Create function to calculate percentage accuracy ######
accuracy_func<-function(df){
  
  total_col_name=glue("{df}_total")
  per_col_name=glue("{df}_per")
  
  # Determine which age variable to us dependent on midline vs endline
  age_var=case_when(grepl('m_',df)~'cr6',
                    T~'childage')
  
  if(grepl('sel')==T){
    out<-get(df) %>% 
      # Change all columns to numeric
      mutate(across(everything(),~as.numeric(.))) %>% 
      # Change CR SEL questions to be 1/0 binary
      mutate(across(contains('cr'),~case_when(.==2~0,
                                              .==1~1,
                                              T~NA_real_))) %>% 
      # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
      mutate(total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
             total_q = ifelse(!!sym(age_var)<10,11,14),
             answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
             answered_q_perc=answered_q/total_q,
             per=case_when(answered_q==0~NA_real_,
                           T~m_sel_total/answered_q)) %>%
      dplyr::select(careid,childid,child_age,total,answered_q_perc,per) %>% 
      rename(total_col_name=total,
             per_col_name=per)
  }else if(grepl('ef'==T)){
    
    
  } else{
    
  }
  
}

##########################################################################################
###################################### Outcome data cleaning #############################
##########################################################################################

# Set threshold to filter out low response kids
threshold<-0

############################################### Midline: SEL ######################################

m_sel <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                child_age=cr6,
                cr1:re11,
                paa2) %>%
  dplyr::select(-re5,-re8)

############################################### Midline Literacy ######################################

m_lit <- m_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(careid,
                childid,
                child_age=cr6,
                starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) %>%
  dplyr::select(-oc5) 

############################################### Midline: Numeracy ######################################

m_num <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                child_age=cr6,
                matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"))

############################################### Midline: EF ######################################

m_ef <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                child_age=cr6,
                starts_with("wm"),
                starts_with("sm"))

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
                childid,
                child_age=cr6,
                cr1:re11) %>%
  # Dedplyr::select friends question
  dplyr::select(-re5,-re8)

############################################### Endline Literacy ######################################

e_lit <- e_child %>% 
  # dplyr::select relevant literacy questions
  dplyr::select(starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) 

############################################### Endline: Numeracy ######################################
# Note that within the literacy category, there are 53 questions.

e_num <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]"))

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  dplyr::select(starts_with("wm"),
                starts_with("sm"))

#####################################################################################################
#####################################################################################################
#####################################################################################################

############################################### Midline: SEL ######################################

m_sel <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         child_age=cr6,
         cr1:re11) %>%
  dplyr::select(-re5,-re8) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.numeric(.))) %>% 
  # Change CR SEL questions to be 1/0 binary
  mutate(across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_sel_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ifelse(child_age<10,11,14),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_sel_per=case_when(answered_q==0~NA_real_,
                             T~m_sel_total/answered_q)) %>%
  # Filter out participants whose question count don't match their age
  filter(!(child_age<10 & (!is.na(re9)|!is.na(re10)|!is.na(re11)))) %>% 
  dplyr::select(careid,childid,child_age,m_sel_total,answered_q_perc,m_sel_per) %>% 
  filter(answered_q_perc>=threshold)
  

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
         matches("pa[0-9]")) %>%
  dplyr::select(-oc5) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # # Change any negative values to 0
  # mutate(ov2=ifelse(ov2<0,0,ov2)) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_lit_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_lit_per=case_when(answered_q==0~NA_real_,
                             T~m_lit_total/answered_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_lit_total,answered_q_perc,m_lit_per)%>% 
  filter(answered_q_perc>=threshold)
  
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
         child_age=cr6) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_num_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_per=case_when(answered_q==0~NA_real_,
                             T~m_num_total/answered_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_num_total,answered_q_perc,m_num_per)%>% 
  filter(answered_q_perc>=threshold)

############################################### Midline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

m_ef <- m_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         starts_with("wm"),
         starts_with("sm"),
         child_age=cr6) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # # Add child age variable
  # left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(m_ef_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ifelse(child_age<10,15,17),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         m_ef_per=m_ef_total/answered_q) %>%
  # Filter out participants whose question count don't match their age
  filter(!(child_age<10 & (!is.na(sm6)|!is.na(sm7)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,m_ef_total,answered_q_perc,m_ef_per)%>% 
  filter(answered_q_perc>=threshold)

############################################### Endline: SEL ######################################

e_sel <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         cr1:re11,
         child_age=childage) %>%
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
  mutate(e_sel_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ifelse(child_age<10,10,13),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         m_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         e_sel_per=e_sel_total/answered_q) %>%
  filter(!(child_age<10 & (!is.na(re9)|!is.na(re10)|!is.na(re11)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_sel_total,answered_q_perc,e_sel_per)%>% 
  filter(answered_q_perc>=threshold)

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
                child_age=childage) %>% 
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all literacy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_lit_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_lit_per=case_when(answered_q==0~NA_real_,
                             T~e_lit_total/answered_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_lit_total,answered_q_perc,e_lit_per)%>% 
  filter(answered_q_perc>=threshold)

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
                child_age=childage) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_num_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ncol(dplyr::select(., -c(careid,childid,child_age))),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_num_per=case_when(answered_q==0~NA_real_,
                             T~e_num_total/answered_q)) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_num_total,answered_q_perc,e_num_per)%>% 
  filter(answered_q_perc>=threshold)

############################################### Endline: EF ######################################
# Note that within the executive function category, there are 15 questions for 5-9 year olds and 17 questions for 10-17 year olds.

e_ef <- e_child %>% 
  # dplyr::select relevant numeracy questions
  dplyr::select(careid,
         childid,
         child_age=childage,
         starts_with("wm"),
         starts_with("sm")) %>%
  # Change all columns to numeric
  mutate(across(everything(),~as.double(.))) %>% 
  # Turn negatives into NA
  mutate_all((~ifelse(. < 0, NA, .))) %>% 
  # Add child age data
  # left_join(child_age,by=c("childid","careid")) %>% 
  # Given that all numeracy variables are binary, they can be added together and calculated as a percentage.
  mutate(e_ef_total=(rowSums(across(-c(careid,childid,child_age)),na.rm=T)),
         total_q = ifelse(child_age<10,15,17),
         answered_q = rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         answered_q_perc=answered_q/total_q,
         e_num_ef_questions=rowSums(!is.na(dplyr::select(., -c(careid,childid,child_age)))),
         e_ef_per=e_ef_total/answered_q) %>%
  filter(!(child_age<10 & (!is.na(sm6)|!is.na(sm7)))) %>% 
  # Keep relevant variables
  dplyr::select(careid,childid,e_ef_total,answered_q_perc,e_ef_per)%>% 
  filter(answered_q_perc>=threshold)

# ############################################### Determine the distribution of amount of questions answered ######################################
# 
# plot<-function(data){
#   
#   for_plot<-get(data) %>% 
#     dplyr::select()
# }

############################################### Putting outcome data together ######################################
  
outcome <- m_lit %>% 
  inner_join(m_sel,by=c("childid","careid")) %>% 
  inner_join(m_num,by=c("childid","careid")) %>% 
  inner_join(m_ef,by=c("childid","careid")) %>% 
  inner_join(e_sel,by=c("childid","careid")) %>% 
  inner_join(e_lit,by=c("childid","careid")) %>% 
  inner_join(e_num,by=c("childid","careid")) %>% 
  inner_join(e_ef,by=c("childid","careid")) %>% 
  dplyr::select(childid,careid,
                contains('_per')) %>% 
  dplyr::select(!contains('answer'))

# Test for instances when child age has a large disparity
child_exclude <- m_child %>% 
  left_join(e_child,by=c('childid','careid')) %>% 
  mutate(age_diff=as.double(childage)-as.double(cr6)) %>% 
  filter(!(age_diff==1|age_diff==0)) %>% 
  dplyr::select(childid) %>% 
  pull()

# Create data for export
out<-outcome 
# %>% 
# filter(!(childid %in% child_exclude))
  
test<-as.data.frame(table(e_ef$answered_q_perc)) %>% clean_names() %>% 
  ggplot()+
  geom_col(aes(x=factor(var1),y=freq))+
  scale_x_discrete(breaks=seq(0,1,0.1),
                   labels=seq(0,1,0.1))
plot(test)

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

saveRDS(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/outcome.rds")


############## Age Investigation


# Endline 
test<-e_child %>% 
  mutate(under_10=case_when(is.na(sm6)&is.na(sm7)~1,T~0)) %>% 
  select(childid,childage,under_10) %>% 
  mutate(age_under_10=case_when(as.double(childage)<10~1,T~0)) %>% 
  left_join(e_cg %>% 
              select(childid, cr6),
            by=c('childid')) %>% 
  mutate(cg_age_under_10=case_when(as.double(cr6)<10~1,T~0)) %>% 
  filter(cg_age_under_10!=under_10)




