###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: May 27, 2025
# Purpose: Run Baseline OLS Regression with each Mechanism

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
  # Filter age group to 10--17 year old since many of the investment mechanisms are NA for younger children
  filter(age==1,
         !is.na(e_ch_health),
         !is.na(e_private_school),
         !is.na(e_cg_edu_engagement),
         !is.na(e_ch_motiv),
         !is.na(e_ch_edu_asp),
         !is.na(e_cg_emotional_engagement),
         !is.na(e_attend),
         !is.na(cg_mh_scale)) %>% 
  mutate(e_attend=case_when(as.double(e_attend)>3~1,
                            T~0))

##########################################################################################
######################################## Regression Functions ############################
##########################################################################################

mech_reg_func<-function(category){
  
  # Define terms
  fi<-'e_cfies_indicator+e_fies_indicator'
  category_text<-case_when(category=='lit'~'Literacy',
                           category=='num'~'Numeracy',
                           category=='ef'~'EF',
                           T~'SEL')
  
  # Define mechanisms of investments
  health_input='e_ch_health'
  edu_input='e_attend+e_private_school+e_cg_edu_engagement'
  # Child self esteem is not included due to 54% of respondents missing data
  child_psyc_input='e_ch_motiv+e_ch_edu_asp'
  cg_psyc_input='e_cg_emotional_engagement+cg_mh_scale'
  
  # Define base OLS functions
  reg_func <- function(category, model){
    e_category_str<-paste0("e_",category,"_per")
    m_category_str<-paste0("m_",category,"_per")
    
    for_reg<-full_data_w %>% 
      rename(lagged_outcome=m_category_str)
    
    fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
    reg <- feols(fm,
                 data=for_reg,
                 cluster=~careid)
    return(reg)
  }
  
  # # Define function for showing F-stat DF in two lines
  # show_F_in_two_lines <- function(stargazer) {
  #   # `Stringr` works better than base's regex 
  #   require(stringr)
  #   
  #   # If you remove `capture.output()`, not only the modified LaTeX code 
  #   # but also the original code would show up
  #   stargazer <- stargazer |>
  #     capture.output()
  #   
  #   # Reuse the index in which F-statistics are displayed
  #   position_F <- str_which(stargazer, "F Statistic")
  #   
  #   # Extract only F-statistics
  #   Fs <- stargazer[position_F] |>
  #     str_replace_all("\\(.*?\\)", "")
  #   
  #   # Extract only df values and make a new line for them
  #   dfs <- stargazer[position_F] |>
  #     str_extract_all("\\(.*?\\)") |>
  #     unlist() |>
  #     (
  #       \(dfs)
  #       paste0(" & ", dfs, collapse = "")
  #     )() |>
  #     paste0(" \\\\")
  #   
  #   # Reuse table elements that are specified
  #   # after the index of F-statistics
  #   after_Fs <- stargazer[-seq_len(position_F)]
  #   
  #   c(
  #     stargazer[seq_len(position_F - 1)],
  #     Fs,
  #     dfs,
  #     after_Fs
  #   ) |>
  #     cat(sep = "\n")
  # }
  # 
  # # Define function for showing Res. SE DF in two lines
  # show_res_se_in_two_lines <- function(stargazer) {
  #   # `Stringr` works better than base's regex 
  #   require(stringr)
  #   
  #   # If you remove `capture.output()`, not only the modified LaTeX code 
  #   # but also the original code would show up
  #   stargazer <- stargazer |>
  #     capture.output()
  #   
  #   # Reuse the index in which F-statistics are displayed
  #   position_res_se <- str_which(stargazer, "Residual Std. Error")
  #   
  #   # Extract only F-statistics
  #   res_ses <- stargazer[position_res_se] |>
  #     str_replace_all("\\(.*?\\)", "")
  #   
  #   # Extract only df values and make a new line for them
  #   dfs <- stargazer[position_res_se] |>
  #     str_extract_all("\\(.*?\\)") |>
  #     unlist() |>
  #     (
  #       \(dfs)
  #       paste0(" & ", dfs, collapse = "")
  #     )() |>
  #     paste0(" \\\\")
  #   
  #   # Reuse table elements that are specified
  #   # after the index of F-statistics
  #   after_res_ses <- stargazer[-seq_len(position_res_se)]
  #   
  #   c(
  #     stargazer[seq_len(position_res_se - 1)],
  #     res_ses,
  #     dfs,
  #     after_res_ses
  #   ) |>
  #     cat(sep = "\n")
  # }
  
  # Define all regression inputs
  input<- expand.grid(category=c(category),
                               model=c(
                                 # glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{health_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{edu_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{child_psyc_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{cg_psyc_input}+')))
  
  # Regression results
  ols_results<- pmap(input,
                          reg_func) %>% 
    set_names(
      # 'Base',
              'Health Input',
              'Edu. Input',
              'Child Pysc. Input',
              'Caregiver Psyc. Input')
  
}

# Run function
the_models<-map(c('lit',
           'num',
           'ef',
           'sel'),
         mech_reg_func) %>% 
  set_names(c('Literacy',
              'Numeracy',
              'EF',
              'SEL'))

# Create empty doc
all_tables_doc <- read_docx()

# Create tables
names(the_models) |>
  Map(f = \(name){
    file_name <- file.path(tempdir(), paste0(name,'.docx'))
    modelsummary(the_models[[name]],
                 title=glue('Value-Added Model: {name}'),
                 fmt=f,
                 cluster='careid',
                 coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*health|.*attend|.*school|.*engagement|.*motiv|.*asp|.*scale|.*treatment)",
                 coef_rename=c('e_cfies_indicator'="Child-Reported Food Insecurity",
                               'e_fies_indicator'="Caregiver-Reported Food Insecurity",
                               'female'='Child is Female',
                               'lagged_outcome'="Lagged Outcome",
                               'e_ch_health2'="Poor Health",
                               'e_ch_health3'="Average Health",
                               'e_ch_health4'="Good Health",
                               'e_ch_health5'="Very Good Health",
                               'e_attend'="Attended School",
                               'e_private_school'="Private Shool",
                               'e_cg_edu_engagement'="Caregiver Edu. Engagement",
                               'e_ch_motiv'='Child Motivation',
                               'e_ch_edu_asp'='Child Edu. Aspiration',
                               'e_cg_emotional_engagement'='Caregiver Emo. Engagement',
                               'cg_mh_scale'='Caregiver Mental Health',
                               'treatment'='Treatment',
                               '(Intercept)'='(Intercept)'),
                 gof_omit = 'AIC|BIC|Std.Errors',
                 gof_map=gm,
                 stars = c('*' = .05, 
                           '**' = .01,
                           '***' = .001),
                 notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment. The child-reported health covariates are measured relative to children who reported very poor health. Attended School is a binary variable indicating whether the child attended school most of the 
 time. Private school is a binary variable indicating whether the child attended private school. Caregiver Edu. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's education. Child Motivation is the sum of a child's degree of agreement with statements related to whether they are motivated. Child Edu. Aspiration is a binary variable of whether a child aspires to complete high school. Caregiver Emo. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's emotional well-being. Caregiver Mental Health is the sum of caregivers' degree of experience with poor mental health.",
                 file_name,
                 escape = FALSE)
    all_tables_doc <- all_tables_doc |>
      body_add_docx(file_name) %>% 
      body_add_break()
    name
  }) 

# Put into word doc
print(all_tables_doc, '/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/06_va_ols_mechs_sep.docx')

# Create Latex
latex_func<-function(name){
  
  modelsummary(the_models[[name]],
               # title=glue('{preamble}Value-Added Model: {name}'),
               fmt=f,
               cluster='careid',
               coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*health|.*attend|.*school|.*engagement|.*motiv|.*asp|.*scale|.*treatment)",
               coef_map=c('e_cfies_indicator'="Child-Reported FI",
                             'e_fies_indicator'="Caregiver-Reported FI",
                             'female'='Child is Female',
                             'e_ch_health2'="Poor Health",
                             'e_ch_health3'="Average Health",
                             'e_ch_health4'="Good Health",
                             'e_ch_health5'="Very Good Health",
                             'e_attend'="Attended School",
                             'e_private_school'="Private School",
                             'e_cg_edu_engagement'="Caregiver Edu. Engagement",
                             'e_ch_motiv'='Child Motivation',
                             'e_ch_edu_asp'='Child Edu. Aspiration',
                             'e_cg_emotional_engagement'='Caregiver Emo. Engagement',
                             'cg_mh_scale'='Caregiver Mental Health',
                          'lagged_outcome'="Lagged Outcome",
                          'treatment'='Treatment',
                          '(Intercept)'='(Intercept)'),
               gof_omit = 'AIC|BIC|Std.Errors',
               gof_map=gm,
               stars = c('*' = .05, 
                         '**' = .01,
                         '***' = .001),
               notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment. The child-reported health covariates are measured relative to children who reported very poor health. Attended School is a binary variable indicating whether the child attended school most of the 
 time. Private school is a binary variable indicating whether the child attended private school. Caregiver Edu. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's education. Child Motivation is the sum of a child's degree of agreement with statements related to whether they are motivated. Child Edu. Aspiration is a binary variable of whether a child aspires to complete high school. Caregiver Emo. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's emotional well-being. Caregiver Mental Health is the sum of caregivers' degree of experience with poor mental health.",
               out=glue("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/06_va_ols_mechs_sep_{str_to_lower(name)}.tex"),
               # latex_options = c("booktabs", "scale_down"),
               escape = FALSE)
}

latex_func('Literacy')
latex_func('Numeracy')
