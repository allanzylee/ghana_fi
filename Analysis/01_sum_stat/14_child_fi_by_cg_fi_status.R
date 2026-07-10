###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Child FI reporting by gender and age, within caregiver-reported FI status

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

##########################################################################################
###################################### Helper Functions ##################################
##########################################################################################

# Mean and t-test p-value comparing group_val_a vs group_val_b on var,
# within a subsample defined by filter_var == filter_val
cell_mean_ttest <- function(data, var, filter_var, filter_val, group_var, group_val_a, group_val_b) {
  
  sub <- data %>% filter(.data[[filter_var]] == filter_val)
  
  mean_a <- sub %>% filter(.data[[group_var]] == group_val_a) %>% pull(.data[[var]]) %>% mean(na.rm = TRUE)
  mean_b <- sub %>% filter(.data[[group_var]] == group_val_b) %>% pull(.data[[var]]) %>% mean(na.rm = TRUE)
  
  x <- sub %>% filter(.data[[group_var]] == group_val_a) %>% pull(.data[[var]])
  y <- sub %>% filter(.data[[group_var]] == group_val_b) %>% pull(.data[[var]])
  pval <- tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
  
  list(mean_a = mean_a, mean_b = mean_b, pval = pval)
}

format_pval <- function(p) {
  if (is.na(p)) return("")
  stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
  paste0(formatC(p, format = "f", digits = 3), stars)
}

make_row <- function(label, mean_a, mean_b, pval) {
  paste0(
    label, " & ",
    formatC(mean_a, format = "f", digits = 3), " & ",
    formatC(mean_b, format = "f", digits = 3), " & ",
    format_pval(pval), " \\\\\n"
  )
}

##########################################################################################
###################################### Build Table Data ##################################
##########################################################################################

# e_fies_indicator == 1: caregiver-reported food insecure
# e_fies_indicator == 0: caregiver-reported food secure

build_panel <- function(filter_val) {
  
  # --- Gender: Girls (female==1) vs Boys (female==0) ---
  gender_cfies   <- cell_mean_ttest(full_data_w, "e_cfies_indicator", "e_fies_indicator", filter_val, "female", 1, 0)
  gender_cfies0  <- cell_mean_ttest(full_data_w, "e_cfies_scale_0",   "e_fies_indicator", filter_val, "female", 1, 0)
  gender_cfies1  <- cell_mean_ttest(full_data_w, "e_cfies_scale_1",   "e_fies_indicator", filter_val, "female", 1, 0)
  gender_cfies2  <- cell_mean_ttest(full_data_w, "e_cfies_scale_2",   "e_fies_indicator", filter_val, "female", 1, 0)
  gender_cfies3  <- cell_mean_ttest(full_data_w, "e_cfies_scale_3",   "e_fies_indicator", filter_val, "female", 1, 0)
  
  # --- Age: Older (age==1) vs Younger (age==0) ---
  age_cfies      <- cell_mean_ttest(full_data_w, "e_cfies_indicator", "e_fies_indicator", filter_val, "age", 1, 0)
  age_cfies0     <- cell_mean_ttest(full_data_w, "e_cfies_scale_0",   "e_fies_indicator", filter_val, "age", 1, 0)
  age_cfies1     <- cell_mean_ttest(full_data_w, "e_cfies_scale_1",   "e_fies_indicator", filter_val, "age", 1, 0)
  age_cfies2     <- cell_mean_ttest(full_data_w, "e_cfies_scale_2",   "e_fies_indicator", filter_val, "age", 1, 0)
  age_cfies3     <- cell_mean_ttest(full_data_w, "e_cfies_scale_3",   "e_fies_indicator", filter_val, "age", 1, 0)
  
  list(
    gender = list(cfies = gender_cfies, s0 = gender_cfies0, s1 = gender_cfies1, s2 = gender_cfies2, s3 = gender_cfies3),
    age    = list(cfies = age_cfies,    s0 = age_cfies0,    s1 = age_cfies1,    s2 = age_cfies2,    s3 = age_cfies3)
  )
}

# Also need dummy columns for cfies_scale
full_data_w <- full_data_w %>%
  fastDummies::dummy_columns('e_cfies_scale') %>%
  dplyr::select(-e_cfies_scale)

fi_hh    <- build_panel(filter_val = 1)   # caregiver FI == 1
non_fi_hh <- build_panel(filter_val = 0)  # caregiver FI == 0

# Ns
n_fi_girl   <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$female == 1, na.rm = TRUE)
n_fi_boy    <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$female == 0, na.rm = TRUE)
n_fi_older  <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$age   == 1, na.rm = TRUE)
n_fi_younger<- sum(full_data_w$e_fies_indicator == 1 & full_data_w$age   == 0, na.rm = TRUE)

n_nfi_girl   <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$female == 1, na.rm = TRUE)
n_nfi_boy    <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$female == 0, na.rm = TRUE)
n_nfi_older  <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$age   == 1, na.rm = TRUE)
n_nfi_younger<- sum(full_data_w$e_fies_indicator == 0 & full_data_w$age   == 0, na.rm = TRUE)

##########################################################################################
###################################### Build LaTeX Table #################################
##########################################################################################

build_panel_rows <- function(panel, group_a_label, group_b_label) {
  paste0(
    make_row('Child-Reported FI (\\%)',                    panel$cfies$mean_a, panel$cfies$mean_b, panel$cfies$pval),
    "\\addlinespace\n",
    make_row('Child: No FI (CFIES=0)',                     panel$s0$mean_a,    panel$s0$mean_b,    panel$s0$pval),
    make_row('Child: Few Exp. (CFIES=1--6)',               panel$s1$mean_a,    panel$s1$mean_b,    panel$s1$pval),
    make_row('Child: Several Exp. (CFIES=7--10)',          panel$s2$mean_a,    panel$s2$mean_b,    panel$s2$pval),
    make_row('Child: Many Exp. (CFIES=11--20)',            panel$s3$mean_a,    panel$s3$mean_b,    panel$s3$pval)
  )
}

n_row_fi <- paste0(
  "Total Observations & ",
  "\\multicolumn{2}{c}{Gender: ", formatC(n_fi_girl, big.mark=","), " (Girls) / ", formatC(n_fi_boy, big.mark=","), " (Boys)} & ",
  " \\\\\n",
  " & \\multicolumn{2}{c}{Age: ", formatC(n_fi_older, big.mark=","), " (10--17) / ", formatC(n_fi_younger, big.mark=","), " (5--9)} & \\\\\n"
)

n_row_nfi <- paste0(
  "Total Observations & ",
  "\\multicolumn{2}{c}{Gender: ", formatC(n_nfi_girl, big.mark=","), " (Girls) / ", formatC(n_nfi_boy, big.mark=","), " (Boys)} & ",
  " \\\\\n",
  " & \\multicolumn{2}{c}{Age: ", formatC(n_nfi_older, big.mark=","), " (10--17) / ", formatC(n_nfi_younger, big.mark=","), " (5--9)} & \\\\\n"
)

latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Child Food Insecurity Reporting by Gender and Age, Within Caregiver-Reported FI Status}\n",
  "\\label{tab:child_fi_by_cg_fi}\\small\n",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lccc@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} \\\\\n",
  "\\midrule \\addlinespace\n",
  
  # Panel A: Caregiver FI == 1, by gender
  "\\multicolumn{4}{@{}l}{\\emph{Panel A: Caregiver-Reported Food Insecure Households --- By Child Gender}} \\\\ \\addlinespace\n",
  " & Girls & Boys & p-value \\\\\n",
  build_panel_rows(fi_hh$gender, "Girls", "Boys"),
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel B: Caregiver FI == 1, by age
  "\\multicolumn{4}{@{}l}{\\emph{Panel B: Caregiver-Reported Food Insecure Households --- By Child Age}} \\\\ \\addlinespace\n",
  " & Age 10--17 & Age 5--9 & p-value \\\\\n",
  build_panel_rows(fi_hh$age, "Age 10-17", "Age 5-9"),
  "\\addlinespace \\midrule \\addlinespace\n",
  
  n_row_fi,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel C: Caregiver FI == 0, by gender
  "\\multicolumn{4}{@{}l}{\\emph{Panel C: Caregiver-Reported Food Secure Households --- By Child Gender}} \\\\ \\addlinespace\n",
  " & Girls & Boys & p-value \\\\\n",
  build_panel_rows(non_fi_hh$gender, "Girls", "Boys"),
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel D: Caregiver FI == 0, by age
  "\\multicolumn{4}{@{}l}{\\emph{Panel D: Caregiver-Reported Food Secure Households --- By Child Age}} \\\\ \\addlinespace\n",
  " & Age 10--17 & Age 5--9 & p-value \\\\\n",
  build_panel_rows(non_fi_hh$age, "Age 10-17", "Age 5-9"),
  "\\addlinespace \\midrule \\addlinespace\n",
  
  n_row_nfi,
  
  "\\bottomrule\n",
  "\\end{tabular*}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Notes:} This table reports unweighted means of child-reported food insecurity",
  " measures, stratified by caregiver-reported food insecurity status (FI = Food Insecure).",
  " Panels A and B restrict to households where the caregiver reported food insecurity (\\texttt{e\\_fies\\_indicator}=1).",
  " Panels C and D restrict to households where the caregiver did not report food insecurity (\\texttt{e\\_fies\\_indicator}=0).",
  " p-values come from two-sided t-tests of equality of means between the two groups.",
  " $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
  " CFIES = Child Food Insecurity Experience Scale.\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/14_child_fi_by_cg_fi_status.tex")