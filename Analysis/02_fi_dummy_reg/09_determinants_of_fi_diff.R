###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: Jan 13, 2026
# Purpose: Run Regression assessing the determinants of disparity in CFIES vs FIES indicator reports

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
  mutate(diff=case_when(e_cfies_indicator!=e_fies_indicator~1,
                        T~0))

##########################################################################################
############################## Multivariate OLS Regression w/ Region and household randomized treatment + Age and Gender ##############################
##########################################################################################

# Define base OLS input
reg<-glm(diff ~ age + female + ch_rank+num_kids+ cg_female+cg_primary+cg_age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west ,
         data = full_data_w)
summary(reg)

# Export results
modelsummary(reg,
             title='Regression of Disparity in Child-/Caregiver-Reported FI on Child/Caregiver Characteristics',
             fmt=f,
             cluster='careid',
             # coef_omit = "^(?!.*tercept|.*dummy|.*outcome)",
             coef_map=c('e_cfies_indicator'="Child-Reported FI",
                        'e_fies_indicator'="Caregiver-Reported FI",
                        'lagged_outcome'="Lagged Outcome",
                        "female"="Child is Female",
                        'age'='Child is 10-17',
                        'ch_rank'='Child Rank',
                        'num_kids'='No. Kids',
                        'cg_primary'="Caregiver Attended Primary School",
                        'cg_age'='Caregiver Age',
                        'cg_female'='Caregiver is Female',
                        'poverty'='Poverty',
                        'treatment'='PNP Treatment',
                        'region_north_east'="Region: North East",
                        'region_northern'="Region: Northern",
                        'region_upper_east'="Region: Upper East",
                        'region_upper_west'="Region: Upper West",
                        'e_ch_health2'='Child Reported Poor Health',
                        'e_ch_health3'='Child Reported Average Health',
                        'e_ch_health4'="Child Reported Good Health",
                        'e_ch_health5'="Child Reported Very Good Health",
                        "e_private_school"="Private School",
                        'e_cg_edu_engagement'="Caregiver Edu. Engagement Scale",
                        'e_ch_motiv'='Child Motivation Scale',
                        'e_ch_edu_asp'='Child Aspires Complete High School',
                        'e_cg_emotional_engagement'='Caregiver Emo. Engagement Scale',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates.",
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/09_determinants_of_fi_diff.html",
             escape = FALSE)

modelsummary(reg,
             title='\\label{appendix:missing}Regression of Disparity in Child-/Caregiver-Reported FI on Child/Caregiver Characteristics',
             fmt=f,
             cluster='careid',
             # coef_omit = "^(?!.*tercept|.*dummy|.*outcome)",
             coef_map=c('e_cfies_indicator'="Child-Reported FI",
                        'e_fies_indicator'="Caregiver-Reported FI",
                        'lagged_outcome'="Lagged Outcome",
                        "female"="Child is Female",
                        'age'='Child is 10-17',
                        'cg_primary'="Caregiver Attended Primary School",
                        'cg_age'='Caregiver Age',
                        'cg_female'='Caregiver is Female',
                        'poverty'='Poverty',
                        'treatment'='PNP Treatment',
                        'region_north_east'="Region: North East",
                        'region_northern'="Region: Northern",
                        'region_upper_east'="Region: Upper East",
                        'region_upper_west'="Region: Upper West",
                        'e_ch_health2'='Child Reported Poor Health',
                        'e_ch_health3'='Child Reported Average Health',
                        'e_ch_health4'="Child Reported Good Health",
                        'e_ch_health5'="Child Reported Very Good Health",
                        "e_private_school"="Private School",
                        'e_cg_edu_engagement'="Caregiver Edu. Engagement Scale",
                        'e_ch_motiv'='Child Motivation Scale',
                        'e_ch_edu_asp'='Child Aspires Complete High School',
                        'e_cg_emotional_engagement'='Caregiver Emo. Engagement Scale',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05, 
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates.",
             out='latex',
             latex_options = c("booktabs", "scale_down"),
             escape = FALSE)

# Within household versions here
reg<-glm(diff ~ age + female + ch_rank+factor(careid),
         data = full_data_w)
summary(reg)

