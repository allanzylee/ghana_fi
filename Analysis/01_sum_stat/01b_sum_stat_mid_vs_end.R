###################################### Introduction ############################################

# Author: Allan Lee
# Date: June 30, 2025
# Purpose: Calculate relevant summary statistics for midline vs endline

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

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

#########################################################################################
######################################## Overall Summary Statistics ##############################
##########################################################################################

summary_stat<-full_data_w %>% 
  dplyr::select(
                contains('ch_fs_dummy'),
                contains('cg_fs_dummy'),
                contains('per')
                )

######################################## T-Test: Create function for group based summary statistics ##############################

t_test_func<-function(var){
  
  df<-summary_stat %>% 
    dplyr::select(contains(var)) %>% 
    pivot_longer(everything(),
                 names_to='group',
                 values_to='var') %>% 
    mutate(group=case_when(startsWith(group,'e_')~1,
                           T~0))
  
  test <- t.test(var~group,
         data=df)
    
    out<-tribble(~var,~midline,~endline,~pval,
                 var,test$estimate[1],test$estimate[2],test$p.value)
  
  return(out)
  
}

# Create function input
input<-c('ch',
         'cg',
         'lit',
         'num',
         'ef',
         'sel')

# Run function
end_vs_mid_sum_stat<-map_dfr(input,
                             t_test_func)

# Combine
out<-end_vs_mid_sum_stat %>% 
  rename(
         'Midline Mean'=midline,
         'Endline Mean'=endline,
         'P-Value'=pval) %>% 
  mutate(var=case_when(var=='ch'~'Child-Reported Severe FI',
                       var=='cg'~'Caregiver-Reported Severe FI',
                       var=='lit'~'Literacy Z-Score',
                       var=='num'~'Numeracy Z-Score',
                       var=='ef'~'EF Z-Score',
                       T~'SEL Z-Score',
                       )
         ) %>% 
  rename('Statistic'=var)

# Create Latex Table
latex=xtable(out, 
             type = "latex")

# names(latex)=c("\\multicolumn{1}{p{2.5in}}{Statistic}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering Mean}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering St. Dev}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering Male}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering Female}",
#                "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Sex}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering 5-9 Years}",
#                "\\multicolumn{1}{p{0.25in}}{\\centering 10-17 Years}",
#                "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Age Group}"
#                )

print(latex, sanitize.colnames.function=function(x){x},
      include.rownames=FALSE)


