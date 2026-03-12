###################################### Introduction ############################################

# Author: Allan Lee
# Date: Apr 3rd, 2024
# Purpose: Calculate Cronbach's Alpha for each of the questions related to outcome categories

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(psych)
library(haven)
library(writexl)

# Load relevant data
full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(childid %in% unique(full_data_w$childid))
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate(across(contains('id'),~as.double(.))) %>% 
  filter(childid %in% unique(full_data_w$childid))


##########################################################################################
###################################### Helpers ###########################################
##########################################################################################

# Coerces all columns to double and replaces negatives with NA
clean_items <- function(df) {
  df %>%
    mutate(across(everything(), ~as.double(.))) %>%
    mutate(across(everything(), ~ifelse(. < 0, NA, .)))
}

# Safely runs alpha() — returns NA with a warning if fewer than 2 items or alpha errors out
safe_alpha <- function(df) {
  tryCatch({
    if (ncol(df) < 2) {
      warning("Fewer than 2 items — alpha cannot be computed. Returning NA.")
      return(NA_real_)
    }
    alpha(df, check.keys = TRUE)$total$raw_alpha
  }, error = function(e) {
    warning(paste("alpha() failed:", e$message, "— returning NA."))
    NA_real_
  })
}


##########################################################################################
###################################### Outcome Data: Cronbach's Alpha ####################
##########################################################################################

############################################### Endline: SEL ######################################
# CR items: binary (recode 2->0, 1->1)
# RE items: ordinal; 6 items for age < 10, 9 items for age >= 10
# Strategy: pool all SEL items per age group for a single category-level alpha.

e_sel_all <- e_child %>%
  dplyr::select(childid, child_age = childage,
                starts_with("cr"),
                re1, re2, re3, re4, re6, re7, re9, re10, re11) %>%
  clean_items() %>%
  mutate(across(starts_with("cr"), ~case_when(. == 1 ~ 1,
                                              . == 2 ~ 0,
                                              TRUE   ~ NA_real_)))

# Age < 10: CR + 6 RE items (re9/re10/re11 not administered)
alpha_sel_young <- e_sel_all %>%
  filter(child_age < 10) %>%
  dplyr::select(-childid, -child_age, -re9, -re10, -re11) %>%
  safe_alpha()

# Age >= 10: CR + 9 RE items
alpha_sel_old <- e_sel_all %>%
  filter(child_age >= 10) %>%
  dplyr::select(-childid, -child_age) %>%
  safe_alpha()


############################################### Endline: Literacy ######################################
# All literacy items are binary. Single alpha across all literacy items (age-invariant).

alpha_lit <- e_child %>%
  dplyr::select(starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) %>%
  clean_items() %>%
  safe_alpha()


############################################### Endline: Numeracy ######################################
# Binary items. NU items only given to age >= 10, so split by age group.

# Age < 10: all numeracy items except nu
alpha_num_young <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age < 10) %>%
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]")) %>%
  clean_items() %>%
  safe_alpha()

# Age >= 10: all numeracy items including nu
alpha_num_old <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age >= 10) %>%
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]")) %>%
  clean_items() %>%
  safe_alpha()


############################################### Endline: EF ######################################
# Pool WM + SM items per age group for a single category-level alpha.

# Age < 10: 6 WM + 5 SM items
alpha_ef_young <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age < 10) %>%
  dplyr::select(starts_with("wm"), starts_with("sm")) %>%
  clean_items() %>%
  safe_alpha()

# Age >= 10: 10 WM + 7 SM items
alpha_ef_old <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age >= 10) %>%
  dplyr::select(starts_with("wm"), starts_with("sm")) %>%
  clean_items() %>%
  safe_alpha()


##########################################################################################
################################## Compile Results #######################################
##########################################################################################

# Literacy is age-invariant so has one row.
# SEL, Numeracy, and EF are split by age group due to different item sets.

alpha_results <- tibble(
  category  = c("SEL",           "SEL",
                "Literacy",
                "Numeracy",      "Numeracy",
                "EF",            "EF"),
  age_group = c("age < 10",      "age >= 10",
                "all ages",
                "age < 10",      "age >= 10",
                "age < 10",      "age >= 10"),
  raw_alpha = c(alpha_sel_young,  alpha_sel_old,
                alpha_lit,
                alpha_num_young,  alpha_num_old,
                alpha_ef_young,   alpha_ef_old)
) %>% 
  group_by(category) %>% 
  summarise(alpha=mean(raw_alpha))

print(alpha_results)


##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

write_xlsx(alpha_results, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/03_cronbach_alpha/cronbach_alpha.xlsx")