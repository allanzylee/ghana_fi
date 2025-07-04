###################################### Introduction ############################################

# Author: Allan Lee
# Date: July 4, 2025
# Purpose: Calculate FI scale difference by group

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

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>% 
  fastDummies::dummy_cols(select_columns=c('e_ch_fies',
                                           'e_fies_scale'))

#########################################################################################
######################################## Overall Summary Statistics ##############################
##########################################################################################

summary_stat<-full_data_w %>% 
  dplyr::select(
    e_ch_fies,
    e_fies_scale
                ) %>% 
  fastDummies::dummy_cols(select_columns=c('e_ch_fies',
                                           'e_fies_scale')) %>% 
  select(matches('[0-9]'))

######################################## Overall Summary Statistics ##############################

overall<-summary_stat %>% 
  summarise(across(everything(),
                .fns = list(
    mean = mean, 
    sd = sd))) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(statistic = str_match(name, pattern = ".+_(.+)")[,2],
         var = str_match(name, pattern = "(.+)_.+")[,2]) %>% 
  select(-name) %>% 
  pivot_wider(names_from = statistic, values_from = value)

######################################## T-Test: Create function for group based summary statistics ##############################

t_test_func<-function(var,
                      group){
  
  exp <- expr(!!ensym(var) ~ !!ensym(group))
  
  test <- t.test(formula = eval(exp),
         data=full_data_w)
  
  if(group=='female'){
    
    out<-tribble(~var,~male,~female,~pval_female,
                 var,test$estimate[1],test$estimate[2],test$p.value)
    
  } else {
    
    out<-tribble(~var,~younger,~older,~pval_age,
                 var,test$estimate[1],test$estimate[2],test$p.value)
    
  }
  
  return(out)
  
}

# Create function input
input<-names(summary_stat) %>% 
  as_tibble() %>% 
  rename(var=value) %>% 
  mutate(female='female',
         age='age'
         )

# Run function
female_summary_stat<-pmap_dfr(input %>% select(-age) %>% rename(group=female) %>% filter(var!='female'),
                             t_test_func)

age_summary_stat<-pmap_dfr(input %>% select(-female) %>% rename(group=age) %>% filter(var!='age_num',var!='age'),
                              t_test_func)

# Combine
out<-overall %>% 
  filter(var!='age',
         !str_detect(var,'^m_.*.per$')) %>% 
  left_join(female_summary_stat,
            by=c('var')) %>% 
  left_join(age_summary_stat,
            by=c('var')) %>% 
  rename(
         'Mean'=mean,
         'St. Dev.'=sd,
         'Male'=male,
         'Female'=female,
         'P-Value: Child Sex'=pval_female,
         '5-9 Years'=younger,
         '10-17 Years'=older,
         'P-Value: Child Age Group'=pval_age) %>% 
  mutate(var=case_when(
    var=='e_ch_fies_0'~'CFIES: No Expereinces',
    var=='e_ch_fies_1'~"CFIES: Few Experiences",
    var=='e_ch_fies_2'~"CFIES: Several Experiences",
    var=='e_ch_fies_3'~"CFIES: Many Experiences",
    var=='e_fies_scale_1'~"FIES: Mild",
    var=='e_fies_scale_2'~"FIES: Moderate",
    var=='e_fies_scale_3'~"FIES: Severe",
    T~'FIES: None'
                       )
         ) %>% 
  rename('Statistic'=var)

# Create Latex Table
latex=xtable(out, 
             type = "latex")

names(latex)=c("\\multicolumn{1}{p{2.5in}}{Statistic}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Mean}",
               "\\multicolumn{1}{p{0.25in}}{\\centering St. Dev}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Male}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Female}",
               "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Sex}",
               "\\multicolumn{1}{p{0.25in}}{\\centering 5-9 Years}",
               "\\multicolumn{1}{p{0.25in}}{\\centering 10-17 Years}",
               "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Age Group}"
               )

print(latex, sanitize.colnames.function=function(x){x},
      include.rownames=FALSE)

write_xlsx(out,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/11_sum_stat_cfies_fies.xlsx")
