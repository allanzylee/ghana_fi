###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Summary statistics for outcomes by two-way subgroups, exported to CSV:
#          Poverty x Gender, Poverty x Age, Caregiver Education x Gender, Caregiver Education x Age

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- readRDS('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>%
  mutate(
    poverty_group = if_else(poverty >= median(poverty, na.rm = TRUE),
                            "above_poverty", "below_poverty"),
    edu_group     = case_when(
      cg_primary == 1 ~ "cg_primary_yes",
      cg_primary == 0 ~ "cg_primary_no",
      TRUE            ~ NA_character_
    ),
    gender_group  = if_else(female == 1, "female", "male"),
    age_group     = if_else(age == 1, "age_10_17", "age_under_10")
  )

##########################################################################################
###################################### Helper Functions ##################################
##########################################################################################

cell_mean <- function(data, var_name, group_col_1, val_1, group_col_2, val_2) {
  data %>%
    filter(.data[[group_col_1]] == val_1, .data[[group_col_2]] == val_2) %>%
    pull(.data[[var_name]]) %>%
    mean(na.rm = TRUE)
}

cell_n <- function(data, group_col_1, val_1, group_col_2, val_2) {
  sum(data[[group_col_1]] == val_1 & data[[group_col_2]] == val_2, na.rm = TRUE)
}

within_pval <- function(data, var_name, group_col_1, val_1a, val_1b, group_col_2, val_2) {
  x <- data %>% filter(.data[[group_col_1]] == val_1a, .data[[group_col_2]] == val_2) %>% pull(.data[[var_name]])
  y <- data %>% filter(.data[[group_col_1]] == val_1b, .data[[group_col_2]] == val_2) %>% pull(.data[[var_name]])
  tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
}

##########################################################################################
###################################### Outcome Variables ##################################
##########################################################################################

outcome_label_pairs <- list(
  list(var = 'e_lit_per', label = 'Literacy'),
  list(var = 'e_num_per', label = 'Numeracy'),
  list(var = 'e_ef_per',  label = 'Executive Function'),
  list(var = 'e_sel_per', label = 'Socioemotional Learning')
)

##########################################################################################
###################################### Build Long-Format Function ########################
##########################################################################################

# Produces one row per outcome x subgroup-cell, in long/tidy format
build_2x2_long <- function(data, design_label, group_col_1, val_1_label, vals_1,
                           group_col_2, val_2_label, vals_2) {
  
  cells <- expand.grid(val_1 = vals_1, val_2 = vals_2, stringsAsFactors = FALSE)
  
  map_dfr(outcome_label_pairs, function(oc) {
    
    map_dfr(seq_len(nrow(cells)), function(i) {
      
      v1 <- cells$val_1[i]
      v2 <- cells$val_2[i]
      
      mean_val <- cell_mean(data, oc$var, group_col_1, v1, group_col_2, v2)
      n_val    <- cell_n(data, group_col_1, v1, group_col_2, v2)
      
      # p-value: compare the two levels of group_col_1, within this level of group_col_2
      pval <- within_pval(data, oc$var, group_col_1, vals_1[1], vals_1[2], group_col_2, v2)
      
      tibble(
        design       = design_label,
        outcome      = oc$label,
        row_group    = group_col_1,
        row_value    = v1,
        col_group    = group_col_2,
        col_value    = v2,
        mean         = mean_val,
        n            = n_val,
        pval_row_diff = pval
      )
    })
  })
}

##########################################################################################
###################################### Run All Four Designs ##############################
##########################################################################################

out_poverty_gender <- build_2x2_long(
  full_data_w, design_label = "Poverty x Gender",
  group_col_1 = "poverty_group", val_1_label = "Poverty", vals_1 = c("above_poverty", "below_poverty"),
  group_col_2 = "gender_group",  val_2_label = "Gender",  vals_2 = c("male", "female")
)

out_poverty_age <- build_2x2_long(
  full_data_w, design_label = "Poverty x Age",
  group_col_1 = "poverty_group", val_1_label = "Poverty", vals_1 = c("above_poverty", "below_poverty"),
  group_col_2 = "age_group",     val_2_label = "Age",     vals_2 = c("age_under_10", "age_10_17")
)

out_edu_gender <- build_2x2_long(
  full_data_w, design_label = "Caregiver Education x Gender",
  group_col_1 = "edu_group",    val_1_label = "Caregiver Education", vals_1 = c("cg_primary_yes", "cg_primary_no"),
  group_col_2 = "gender_group", val_2_label = "Gender",              vals_2 = c("male", "female")
)

out_edu_age <- build_2x2_long(
  full_data_w, design_label = "Caregiver Education x Age",
  group_col_1 = "edu_group", val_1_label = "Caregiver Education", vals_1 = c("cg_primary_yes", "cg_primary_no"),
  group_col_2 = "age_group", val_2_label = "Age",                 vals_2 = c("age_under_10", "age_10_17")
)

##########################################################################################
###################################### Combine and Export #################################
##########################################################################################

outcome_by_wealth_subgroup <- bind_rows(
  out_poverty_gender,
  out_poverty_age,
  out_edu_gender,
  out_edu_age
) %>%
  mutate(
    mean = round(mean, 4),
    pval_row_diff = round(pval_row_diff, 4)
  )

fwrite(outcome_by_wealth_subgroup,
          "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/13_outcome_by_wealth_subgroup.csv")
