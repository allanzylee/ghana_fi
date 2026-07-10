###################################### Ghana FI Introduction ############################################

# Author: Allan Lee
# Date: Jan 13, 2026 (updated: subgroup interaction models)
# Purpose: Run Regression assessing the determinants of disparity in CFIES vs FIES indicator reports,
#          with subgroup interactions (gender x poverty/education, age x poverty/education)
#
# NOTE: Outcome variable renamed from `FI Report Mismatch` (space-containing, backtick-quoted)
# to fi_report_mismatch to avoid backtick-corruption issues when copying/editing this file.
# If your editor/paste path stripped backticks and asterisks last time, downloading this file
# directly (rather than copy-pasting from chat) should avoid that.

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
  mutate(fi_report_mismatch=case_when(e_cfies_indicator!=e_fies_indicator~1,
                                      T~0),
         attend=case_when(as.double(e_attend)>3~1,
                          T~0),
         # Poverty indicator: `poverty` is assumed to already exist as a probability
         # (higher = poorer). Split at the sample median.
         # NOTE: adjust the cutoff below if you want a different threshold
         # (e.g., top tercile) instead of a median split.
         poverty_high=case_when(poverty > median(poverty, na.rm=TRUE) ~ 1,
                                T~0),
         # Flip caregiver education: cg_primary=1 means completed primary+.
         # cg_no_primary=1 means caregiver did NOT complete primary education.
         cg_no_primary=case_when(cg_primary==1 ~ 0,
                                 cg_primary==0 ~ 1,
                                 T~NA_real_))

##########################################################################################
################################# Shared coefficient map ##################################
##########################################################################################

subgroup_coef_map <- c('e_cfies_indicator'="Child-Reported FI",
                       'e_fies_indicator'="Caregiver-Reported FI",
                       'lagged_outcome'="Lagged Outcome",
                       "female"="Child is Female",
                       'age'='Child is 10-17',
                       'age_pct_rank'='Child Rank',
                       'attend'='Child Attends School',
                       'num_kids'='No. Kids',
                       'cg_primary'="Caregiver Attended Primary School",
                       'cg_no_primary'="Caregiver Did Not Attend Primary School",
                       'cg_age'='Caregiver Age',
                       'cg_female'='Caregiver is Female',
                       'poverty'='Poverty (Probability)',
                       'poverty_high'='Poverty (Above Median)',
                       'female:poverty_high'='Female x Poverty (Above Median)',
                       'female:cg_no_primary'='Female x Caregiver Did Not Attend Primary',
                       'age:poverty_high'='Child 10-17 x Poverty (Above Median)',
                       'age:cg_no_primary'='Child 10-17 x Caregiver Did Not Attend Primary',
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
                       '(Intercept)'='(Intercept)')

subgroup_notes <- "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. FI Report Mismatch=1 if children and caregivers of a given household did not match in their food insecurity indicator reports. Child rank is constructed as a percentile rank of all children in a given household from oldest to youngest. Poverty (Above Median)=1 if the household's poverty probability is above the sample median. Caregiver Did Not Attend Primary School=1 if the caregiver did not complete primary education or more. Model includes month fixed effect. Robust standard errors clustered by caregiver are reported."

subgroup_stars <- c('*' = .05, '**' = .01, '***' = .001)

##########################################################################################
############################## Model 1: Female x Poverty (median split) #################
##########################################################################################

reg1 <- glm(fi_report_mismatch ~ age + female*poverty_high + age_pct_rank + attend + cg_female + cg_primary + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg1)

modelsummary(reg1,
             title='Regression of Disparity in Child-/Caregiver-Reported FI: Female x Poverty',
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15a_determinants_of_fi_diff_subgroup.html",
             escape = FALSE)

modelsummary(dvnames(reg1),
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             latex_options = c("booktabs", "scale_down"),
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15a_determinants_of_fi_diff_subgroup.tex",
             escape = FALSE)

##########################################################################################
######################## Model 2: Female x Caregiver Education (flipped) ################
##########################################################################################

reg2 <- glm(fi_report_mismatch ~ age + female*cg_no_primary + age_pct_rank + attend + cg_female + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg2)

modelsummary(reg2,
             title='Regression of Disparity in Child-/Caregiver-Reported FI: Female x Caregiver Education',
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15b_determinants_of_fi_diff_subgroup.html",
             escape = FALSE)

modelsummary(dvnames(reg2),
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             latex_options = c("booktabs", "scale_down"),
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15b_determinants_of_fi_diff_subgroup.tex",
             escape = FALSE)

##########################################################################################
############################## Model 3: Child Age x Poverty (median split) ###############
##########################################################################################

reg3 <- glm(fi_report_mismatch ~ age*poverty_high + female + age_pct_rank + attend + cg_female + cg_primary + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg3)

modelsummary(reg3,
             title='Regression of Disparity in Child-/Caregiver-Reported FI: Child Age x Poverty',
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15c_determinants_of_fi_diff_subgroup.html",
             escape = FALSE)

modelsummary(dvnames(reg3),
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             latex_options = c("booktabs", "scale_down"),
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15c_determinants_of_fi_diff_subgroup.tex",
             escape = FALSE)

##########################################################################################
###################### Model 4: Child Age x Caregiver Education (flipped) ################
##########################################################################################

reg4 <- glm(fi_report_mismatch ~ age*cg_no_primary + female + age_pct_rank + attend + cg_female + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg4)

modelsummary(reg4,
             title='Regression of Disparity in Child-/Caregiver-Reported FI: Child Age x Caregiver Education',
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15d_determinants_of_fi_diff_subgroup.html",
             escape = FALSE)

modelsummary(dvnames(reg4),
             fmt=f,
             cluster='careid',
             coef_map=subgroup_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = subgroup_stars,
             modelnames = "FI Report Mismatch",
             notes = subgroup_notes,
             latex_options = c("booktabs", "scale_down"),
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15d_determinants_of_fi_diff_subgroup.tex",
             escape = FALSE)

##########################################################################################
######################## Optional: all 4 models side-by-side #############################
##########################################################################################

# Shorter labels just for the combined table, to keep it narrow enough for one page.
# (Full labels are still used in the individual 15a-15d tables above.)
combined_coef_map <- c("female"="Female",
                       'age'='Age 10-17',
                       'poverty_high'='Poverty (Hi)',
                       'cg_no_primary'='Cg. No Primary',
                       'female:poverty_high'='Female x Poverty',
                       'female:cg_no_primary'='Female x Cg. No Primary',
                       'age:poverty_high'='Age 10-17 x Poverty',
                       'age:cg_no_primary'='Age 10-17 x Cg. No Primary',
                       'age_pct_rank'='Child Rank',
                       'attend'='Attends School',
                       'cg_primary'="Cg. Primary",
                       'cg_age'='Cg. Age',
                       'cg_female'='Cg. Female',
                       'treatment'='PNP Treatment',
                       'region_north_east'="Reg: NE",
                       'region_northern'="Reg: North",
                       'region_upper_east'="Reg: UE",
                       'region_upper_west'="Reg: UW",
                       '(Intercept)'='Intercept')

combined_notes <- "Note: Poverty (Hi)=1 if household poverty probability is above the sample median. Cg. No Primary=1 if the caregiver did not complete primary education or more. All models include region and month fixed effects (omitted here for space); see Tables 15a-15d for full specifications. Robust SEs clustered by caregiver."

modelsummary(list("Fem. x Pov."=reg1,
                  "Fem. x Educ."=reg2,
                  "Age x Pov."=reg3,
                  "Age x Educ."=reg4),
             fmt=f,
             cluster='careid',
             coef_map=combined_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors|RMSE',
             gof_map=gm,
             stars = subgroup_stars,
             notes = combined_notes,
             latex_options = c("booktabs", "scale_down", "HOLD_position"),
             out="/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/15_determinants_of_fi_diff_subgroup_combined.tex",
             escape = FALSE)

# If it's still too wide/tall once compiled in Overleaf, open the .tex file above and
# manually wrap the \resizebox{...}{...}{...} block with \scriptsize immediately before it,
# e.g.:
#   \scriptsize
#   \resizebox{\linewidth}{!}{
#   ...
#   }
# This shrinks the font directly rather than relying on resizebox alone, which can make
# scale_down-only tables blurry/hard to read once shrunk to fit page width.

