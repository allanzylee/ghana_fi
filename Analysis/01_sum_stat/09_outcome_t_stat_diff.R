###################################### Introduction ############################################

# Author: Allan Lee
# Date: July 1st, 2025
# Purpose: Calculate outcome T-statistic for FI vs NOT FI

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
                contains('e_ch_fs_dummy'),
                contains('e_cg_fs_dummy'),
                matches('^e_.*per$')
                )

######################################## T-Test: Create function for group based summary statistics ##############################

t_test_func<-function(var,
                      fi){
  
  df<-summary_stat %>% 
    select(contains(var),
           contains(fi))
  
  var_name=paste0('e_',var,'_per')
  group=paste0('e_',fi,'_fs_dummy')
  
  exp <- expr(!!ensym(var_name) ~ !!ensym(group))
  
  test <- t.test(formula=eval(exp),
         data=df)
    
  out<-tribble(~var,~not_fi,~fi,~pval,~group,
               var,test$estimate[1],test$estimate[2],test$p.value,fi)
  
  return(out)
  
}

# Create function input
input<-expand.grid(c('lit',
                     'num',
                     'ef',
                     'sel'),
                   c('ch',
                     'cg')) %>% 
  clean_names() %>% 
  rename(var=var1,
         fi=var2) %>% 
  mutate(across(everything(),~as.character(.)))

# Run function
sum_stat<-pmap_dfr(input,
                  t_test_func)
# 
# # Combine
# out<-end_vs_mid_sum_stat %>% 
#   rename(
#          'Midline Mean'=midline,
#          'Endline Mean'=endline,
#          'P-Value'=pval) %>% 
#   mutate(var=case_when(var=='ch'~'Child-Reported Severe FI',
#                        var=='cg'~'Caregiver-Reported Severe FI',
#                        var=='lit'~'Literacy Z-Score',
#                        var=='num'~'Numeracy Z-Score',
#                        var=='ef'~'EF Z-Score',
#                        T~'SEL Z-Score',
#                        )
#          ) %>% 
#   rename('Statistic'=var)
# 
# # Create Latex Table
# latex=xtable(out, 
#              type = "latex")
# 
# # names(latex)=c("\\multicolumn{1}{p{2.5in}}{Statistic}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering Mean}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering St. Dev}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering Male}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering Female}",
# #                "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Sex}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering 5-9 Years}",
# #                "\\multicolumn{1}{p{0.25in}}{\\centering 10-17 Years}",
# #                "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Child Age Group}"
# #                )
# 
# print(latex, sanitize.colnames.function=function(x){x},
#       include.rownames=FALSE)
# 
# 
