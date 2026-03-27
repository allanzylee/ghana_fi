###################################### Ghana FI - Mechanisms as Outcomes (VA) ############################################

# Author: Allan Lee
# Purpose: VA OLS Regressions with Each Mechanism Variable as Outcome

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")
library(MASS)
library(sandwich)
library(lmtest)
options(modelsummary_output = "string")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>%
  filter(age == 1,
         !is.na(e_ch_health),               !is.na(m_ch_health),
         !is.na(e_private_school),           !is.na(m_private_school),
         !is.na(e_cg_edu_engagement),        !is.na(m_cg_edu_engagement),
         !is.na(e_ch_motiv),                 !is.na(m_ch_motiv),
         !is.na(e_ch_edu_asp),               !is.na(m_ch_edu_asp),
         !is.na(e_cg_emotional_engagement),  !is.na(m_cg_emotional_engagement),
         !is.na(e_attend),                   !is.na(m_attend)) %>%
  mutate(
    e_attend = case_when(as.double(e_attend) > 3 ~ 1, T ~ 0),
    m_attend = case_when(as.double(m_attend) > 3 ~ 1, T ~ 0)
  )

##########################################################################################
######################################## Define Mechanism Outcomes #######################
##########################################################################################

mech_outcomes <- list(
  # Health
  list(e_var = "e_ch_health",               m_var = "m_ch_health",               label = "Child Health"),
  # Educational
  list(e_var = "e_attend",                  m_var = "m_attend",                  label = "Attended School"),
  list(e_var = "e_private_school",          m_var = "m_private_school",          label = "Private School"),
  list(e_var = "e_cg_edu_engagement",       m_var = "m_cg_edu_engagement",       label = "Caregiver Edu. Engagement"),
  # Child Psychological
  list(e_var = "e_ch_motiv",                m_var = "m_ch_motiv",                label = "Child Motivation"),
  list(e_var = "e_ch_edu_asp",              m_var = "m_ch_edu_asp",              label = "Child Edu. Aspiration"),
  # Caregiver Psychological
  list(e_var = "e_cg_emotional_engagement", m_var = "m_cg_emotional_engagement", label = "Caregiver Emo. Engagement")
)

##########################################################################################
######################################## Regression Function #############################
##########################################################################################

fi        <- 'e_cfies_indicator + e_fies_indicator'
base_ctrl <- 'female + region_north_east + region_northern + region_upper_east + region_upper_west + treatment + age_pct_rank + factor(month)'

mech_outcome_reg <- function(mech) {
  
  e_var     <- mech$e_var
  m_var     <- mech$m_var
  is_factor <- is.factor(full_data_w[[e_var]])
  is_binary <- e_var %in% c("e_attend", "e_private_school")
  
  rhs <- glue("~ {fi} + {m_var} + {base_ctrl}")
  fm  <- as.formula(paste(e_var, rhs))
  
  if (is_factor) {
    reg <- polr(fm, data = full_data_w, Hess = TRUE)
  } else if (is_binary) {
    reg <- glm(fm, data = full_data_w, family = "binomial")
  } else {
    reg <- feols(fm, data = full_data_w, cluster = ~careid)
  }
  
  return(reg)
}

# Run all regressions
mech_results <- map(mech_outcomes, mech_outcome_reg) %>%
  set_names(map_chr(mech_outcomes, ~ .x$label))

##########################################################################################
######################################## Clustered SEs for polr models ##################
##########################################################################################

vcov_list <- map(mech_results, ~ {
  if (inherits(.x, "polr") | inherits(.x, "glm")) {
    vcovCL(.x, cluster = full_data_w$careid)
  } else {
    NULL
  }
})

##########################################################################################
######################################## Output ##########################################
##########################################################################################

latex_note <- "Note: Child- and Caregiver-Reported Food Insecurity are binary indicators
(CFIES $>$ 7 and FIES $>$ 4, respectively). Robust standard errors clustered by caregiver
are reported. Each column regresses the listed mechanism variable on the food insecurity
indicators, the corresponding midline outcome, and base controls. Base controls include
child sex, region fixed effects, treatment status, child percentile rank by age, and month
fixed effects. Columns are grouped into four mechanism categories: Health Input, Educational
Input, Child Psychological Input, and Caregiver Psychological Input. The Child Health column
uses an ordered logit (polr); Attended School and Private School use logistic regression (glm);
all other columns use OLS estimated via feols."

coef_rename_vec <- c(
  'e_cfies_indicator' = "Child-Reported FI",
  'e_fies_indicator'  = "Caregiver-Reported FI",
  'treatment'         = "Treatment"
)

ms_args <- list(
  models      = mech_results,
  fmt         = f,
  vcov        = vcov_list,
  coef_omit   = "^(?!.*indicator|.*treatment)|\\.\\|\\.",  # show only FI indicators and treatment
  coef_rename = coef_rename_vec,
  gof_omit    = 'AIC|BIC|Std.Errors',
  gof_map     = gm,
  stars       = c('*' = .05, '**' = .01, '***' = .001),
  notes       = latex_note,
  escape      = FALSE
)

# ---- LaTeX output ----
out_tex <- "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/12_mech_outcome.tex"

if (file.exists(out_tex)) file.remove(out_tex)
do.call(modelsummary, c(ms_args, list(output = out_tex)))

# Read back as single string for easier regex
latex_content <- paste(readLines(out_tex), collapse = "\n")

# Break long column headers onto two lines
latex_content <- gsub("Caregiver Edu\\. Engagement",  "\\\\shortstack{Caregiver\\\\\\\\Edu. Engagement}",  latex_content)
latex_content <- gsub("Caregiver Emo\\. Engagement",  "\\\\shortstack{Caregiver\\\\\\\\Emo. Engagement}",  latex_content)
latex_content <- gsub("Child Edu\\. Aspiration",       "\\\\shortstack{Child Edu.\\\\\\\\Aspiration}",      latex_content)

# Wrap tabular in resizebox
latex_content <- gsub(
  "(\\\\begin\\{tabular\\})",
  "\\\\resizebox{\\\\linewidth}{!}{%\n\\1",
  latex_content
)
latex_content <- gsub(
  "(\\\\end\\{tabular\\})",
  "\\1\n}% end resizebox",
  latex_content
)

# Wrap in landscape + footnotesize
latex_landscape <- paste0(
  "\\begin{landscape}\n",
  "{\\footnotesize\n",
  latex_content,
  "\n}% end footnotesize\n",
  "\\end{landscape}"
)

writeLines(latex_landscape, out_tex)

# ---- Word output ----
out_path <- '/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/12_mech_outcome.docx'
if (file.exists(out_path)) file.remove(out_path)
file_name_docx <- tempfile(fileext = ".docx")

do.call(modelsummary, c(ms_args, list(output = file_name_docx)))

landscape_props <- prop_section(
  page_size = page_size(orient = "landscape")
)

doc <- read_docx() %>%
  body_add_docx(file_name_docx) %>%
  body_end_block_section(block_section(landscape_props))

print(doc, out_path)
