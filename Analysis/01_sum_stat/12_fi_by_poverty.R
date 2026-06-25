###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Summary statistics for FI measures by poverty and caregiver education subgroups

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>%
  fastDummies::dummy_columns(c('e_cfies_scale', 'e_fies_scale')) %>%
  dplyr::select(-e_cfies_scale, -e_fies_scale) %>%
  mutate(
    poverty_group = if_else(poverty >= median(poverty, na.rm = TRUE),
                            "above_poverty", "below_poverty"),
    edu_group     = case_when(
      cg_primary == 1 ~ "cg_primary_yes",
      cg_primary == 0 ~ "cg_primary_no",
      TRUE            ~ NA_character_
    )
  )

##########################################################################################
###################################### Helper Functions ##################################
##########################################################################################

subgroup_mean <- function(data, var_name, group_col, group_val) {
  data %>%
    filter(.data[[group_col]] == group_val) %>%
    pull(.data[[var_name]]) %>%
    mean(na.rm = TRUE)
}

# T-test p-value between above vs below median poverty
poverty_pval <- function(var_name) {
  x <- full_data_w %>% filter(poverty_group == "above_poverty") %>% pull(.data[[var_name]])
  y <- full_data_w %>% filter(poverty_group == "below_poverty") %>% pull(.data[[var_name]])
  tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
}

edu_pval <- function(var_name) {
  x <- full_data_w %>% filter(edu_group == "cg_primary_yes") %>% pull(.data[[var_name]])
  y <- full_data_w %>% filter(edu_group == "cg_primary_no")  %>% pull(.data[[var_name]])
  tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
}

# Format p-value with stars
format_pval <- function(p) {
  if (is.na(p)) return("")
  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
  paste0(formatC(p, format = "f", digits = 3), stars)
}

sum_stat_subgroup_func <- function(var_name, label) {
  tibble(
    var            = label,
    above_poverty  = subgroup_mean(full_data_w, var_name, "poverty_group", "above_poverty"),
    below_poverty  = subgroup_mean(full_data_w, var_name, "poverty_group", "below_poverty"),
    pval_poverty   = poverty_pval(var_name),
    cg_primary_yes = subgroup_mean(full_data_w, var_name, "edu_group", "cg_primary_yes"),
    cg_primary_no  = subgroup_mean(full_data_w, var_name, "edu_group", "cg_primary_no"),
    pval_edu       = edu_pval(var_name)          # <-- new
  )
}

##########################################################################################
###################################### Run Summary Stats #################################
##########################################################################################

var_label_pairs <- list(
  list(var = 'e_cfies_indicator',  label = 'Child-Reported FI (\\%)'),
  list(var = 'e_fies_indicator',   label = 'Caregiver-Reported FI (\\%)'),
  list(var = 'e_cfies_scale_0',    label = 'Child: No FI (CFIES=0)'),
  list(var = 'e_cfies_scale_1',    label = 'Child: Few Exp. (CFIES=1--6)'),
  list(var = 'e_cfies_scale_2',    label = 'Child: Several Exp. (CFIES=7--10)'),
  list(var = 'e_cfies_scale_3',    label = 'Child: Many Exp. (CFIES=11--20)'),
  list(var = 'e_fies_scale_0',     label = 'Caregiver: Food Secure (FIES=0--3)'),
  list(var = 'e_fies_scale_1',     label = 'Caregiver: Moderately Food Insecure (FIES=4--6)'),
  list(var = 'e_fies_scale_2',     label = 'Caregiver: Severely Food Insecure (FIES=7--8)')
)

summed_raw <- map_dfr(var_label_pairs, ~ sum_stat_subgroup_func(var_name = .x$var, label = .x$label))

summed <- summed_raw %>%
  mutate(
    across(c(above_poverty, below_poverty, cg_primary_yes, cg_primary_no),
           ~ formatC(.x, format = "f", digits = 3)),
    pval_poverty = map_chr(pval_poverty, format_pval),
    pval_edu     = map_chr(pval_edu,     format_pval)   # <-- new
  )

# Subgroup Ns
n_above  <- sum(full_data_w$poverty_group == "above_poverty",  na.rm = TRUE)
n_below  <- sum(full_data_w$poverty_group == "below_poverty",  na.rm = TRUE)
n_cg_yes <- sum(full_data_w$edu_group     == "cg_primary_yes", na.rm = TRUE)
n_cg_no  <- sum(full_data_w$edu_group     == "cg_primary_no",  na.rm = TRUE)

##########################################################################################
###################################### Build LaTeX Table #################################
##########################################################################################
make_row <- function(df, i) {
  paste0(
    df$var[i],            " & ",
    df$above_poverty[i],  " & ",
    df$below_poverty[i],  " & ",
    df$pval_poverty[i],   " & ",
    df$cg_primary_yes[i], " & ",
    df$cg_primary_no[i],  " & ",
    df$pval_edu[i],       " \\\\\n"   # <-- new
  )
}

panel_a_rows <- paste0(
  paste0(sapply(1:2, function(i) make_row(summed, i)), collapse = ""),
  "\\addlinespace\n",
  paste0(sapply(3:6, function(i) make_row(summed, i)), collapse = ""),
  "\\addlinespace\n",
  paste0(sapply(7:9, function(i) make_row(summed, i)), collapse = "")
)

n_row <- paste0(
  "Total Observations & ",
  formatC(n_above,  big.mark = ","), " & ",
  formatC(n_below,  big.mark = ","), " & ",
  " & ",
  formatC(n_cg_yes, big.mark = ","), " & ",
  formatC(n_cg_no,  big.mark = ","), " \\\\\n"
)

latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Food Insecurity Measures by Poverty and Caregiver Education}\n",
  "\\label{tab:fi_subgroup}\\small\n",
  "\\begin{tabular}{@{}lcccccc@{}}\n",   # 7 cols now
  "\\toprule\n",
  " & \\multicolumn{3}{c}{Poverty} & \\multicolumn{3}{c}{Caregiver Education} \\\\\n",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n",
  " & Above Median & Below Median & p-value & Completed Primary & Did Not Complete Primary & p-value \\\\\n",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\\n",
  "\\midrule \\addlinespace\n",
  
  "\\multicolumn{6}{@{}l}{\\emph{Panel A: Food Insecurity Experiences}} \\\\ \\addlinespace\n",
  panel_a_rows,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  n_row <- paste0(
    "Total Observations & ",
    formatC(n_above,  big.mark = ","), " & ",
    formatC(n_below,  big.mark = ","), " & ",
    " & ",
    formatC(n_cg_yes, big.mark = ","), " & ",
    formatC(n_cg_no,  big.mark = ","), " & ",
    " \\\\\n"   # blank p-value cell for N row
  ),
  
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Notes:} This table reports unweighted means by subgroup.",
  " The p-value column reports the p-value from a two-sided t-test of equality of means between above- and below-median poverty groups.",
  " $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
  " FI = Food Insecure. CFIES = Child Food Insecurity Experience Scale. FIES = Food Insecurity Experience Scale.",
  " Poverty groups are defined by the sample median. Caregiver education groups reflect whether the caregiver",
  " completed primary school (\\texttt{cg\\_primary} = 1) or not (\\texttt{cg\\_primary} = 0).\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/12_fi_by_poverty.tex")