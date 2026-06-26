###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Repeat key analyses using poverty QUARTILES instead of above/below median:
#          (1) FI summary stats table by poverty quartile
#          (2) 3-way interaction regressions (sex x FI x poverty quartile, age x FI x poverty quartile)
#          (3) Outcome means plot by poverty quartile x gender/age

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")
library(marginaleffects)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds') %>%
  fastDummies::dummy_columns(c('e_cfies_scale', 'e_fies_scale')) %>%
  mutate(
    poverty_quartile = ntile(poverty, 4),
    poverty_quartile_label = factor(
      poverty_quartile,
      levels = 1:4,
      labels = c("Q1 (Least Poor)", "Q2", "Q3", "Q4 (Most Poor)")
    ),
    cg_primary = case_when(cg_primary == 1 ~ 0, T ~ 1),  # 1 = did not complete primary
    gender_group = if_else(female == 1, "Female", "Male"),
    age_group    = if_else(age == 1, "Age 10-17", "Under 10")
  )

##########################################################################################
###################################### 1. FI Summary Stats by Poverty Quartile ###########
##########################################################################################

subgroup_mean <- function(data, var_name, group_col, group_val) {
  data %>%
    filter(.data[[group_col]] == group_val) %>%
    pull(.data[[var_name]]) %>%
    mean(na.rm = TRUE)
}

format_pval <- function(p) {
  if (is.na(p)) return("")
  stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
  paste0(formatC(p, format = "f", digits = 3), stars)
}

# F-test across all 4 quartiles (ANOVA), since pairwise t-tests don't summarize 4 groups well
quartile_anova_pval <- function(var_name) {
  tryCatch({
    aov_fit <- aov(reformulate("factor(poverty_quartile)", response = var_name), data = full_data_w)
    summary(aov_fit)[[1]][["Pr(>F)"]][1]
  }, error = function(e) NA_real_)
}

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

sum_stat_quartile_func <- function(var_name, label) {
  tibble(
    var = label,
    q1  = subgroup_mean(full_data_w, var_name, "poverty_quartile", 1),
    q2  = subgroup_mean(full_data_w, var_name, "poverty_quartile", 2),
    q3  = subgroup_mean(full_data_w, var_name, "poverty_quartile", 3),
    q4  = subgroup_mean(full_data_w, var_name, "poverty_quartile", 4),
    pval_anova = quartile_anova_pval(var_name)
  )
}

summed_quartile_raw <- map_dfr(var_label_pairs, ~ sum_stat_quartile_func(var_name = .x$var, label = .x$label))

summed_quartile <- summed_quartile_raw %>%
  mutate(
    across(c(q1, q2, q3, q4), ~ formatC(.x, format = "f", digits = 3)),
    pval_anova = map_chr(pval_anova, format_pval)
  )

n_q1 <- sum(full_data_w$poverty_quartile == 1, na.rm = TRUE)
n_q2 <- sum(full_data_w$poverty_quartile == 2, na.rm = TRUE)
n_q3 <- sum(full_data_w$poverty_quartile == 3, na.rm = TRUE)
n_q4 <- sum(full_data_w$poverty_quartile == 4, na.rm = TRUE)

make_row_quartile <- function(df, i) {
  paste0(
    df$var[i], " & ", df$q1[i], " & ", df$q2[i], " & ", df$q3[i], " & ", df$q4[i],
    " & ", df$pval_anova[i], " \\\\\n"
  )
}

panel_a_rows_q <- paste0(
  paste0(sapply(1:2, function(i) make_row_quartile(summed_quartile, i)), collapse = ""),
  "\\addlinespace\n",
  paste0(sapply(3:6, function(i) make_row_quartile(summed_quartile, i)), collapse = ""),
  "\\addlinespace\n",
  paste0(sapply(7:9, function(i) make_row_quartile(summed_quartile, i)), collapse = "")
)

n_row_q <- paste0(
  "Total Observations & ",
  formatC(n_q1, big.mark = ","), " & ", formatC(n_q2, big.mark = ","), " & ",
  formatC(n_q3, big.mark = ","), " & ", formatC(n_q4, big.mark = ","), " & \\\\\n"
)

latex_table_quartile <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Food Insecurity Measures by Poverty Quartile}\n",
  "\\label{tab:fi_quartile}\\small\n",
  "\\begin{tabular}{@{}lccccc@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{4}{c}{Poverty Quartile} & \\\\\n",
  "\\cmidrule(lr){2-5}\n",
  " & Q1 (Least Poor) & Q2 & Q3 & Q4 (Most Poor) & p-value \\\\\n",
  " & (1) & (2) & (3) & (4) & (5) \\\\\n",
  "\\midrule \\addlinespace\n",
  "\\multicolumn{6}{@{}l}{\\emph{Panel A: Food Insecurity Experiences}} \\\\ \\addlinespace\n",
  panel_a_rows_q,
  "\\addlinespace \\midrule \\addlinespace\n",
  n_row_q,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Notes:} This table reports unweighted means by poverty quartile.",
  " Q1 is the least poor quartile and Q4 is the most poor quartile.",
  " The p-value column reports the p-value from a one-way ANOVA F-test of equality of means across all four quartiles.",
  " $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
  " FI = Food Insecure. CFIES = Child Food Insecurity Experience Scale. FIES = Food Insecurity Experience Scale.\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table_quartile,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10a_poverty_quartiles.tex")

##########################################################################################
###################################### 2a. FI x Sex x Poverty Quartile (3-way) ############
##########################################################################################

va_ols_input_18a <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*female + e_fies_indicator*female +
                  e_cfies_indicator*factor(poverty_quartile) + e_fies_indicator*factor(poverty_quartile) +
                  e_cfies_indicator*female*factor(poverty_quartile) + e_fies_indicator*female*factor(poverty_quartile) +
                  female + factor(poverty_quartile) + age + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_18a <- pmap(va_ols_input_18a, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

notes_18a <- "Note: Poverty quartiles are constructed using ntile(), where Q1 is the least poor quartile and Q4 is the most poor quartile. Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child age group, child rank in percentile by age, region, and month fixed effects."

modelsummary(va_ols_results_18a,
             title     = 'Value-Added Model: Heterogeneity by Child Sex and Poverty Quartile',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*quartile)",
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_18a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10b_poverty_quartiles.html",
             escape    = FALSE)

modelsummary(va_ols_results_18a,
             title     = '\\label{reg:sex_pquart}Value-Added Model: Heterogeneity by Child Sex and Poverty Quartile',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*quartile)",
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_18a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10b_poverty_quartiles.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
###################################### 2b. FI x Age x Poverty Quartile (3-way) ############
##########################################################################################

va_ols_input_18b <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*age + e_fies_indicator*age +
                  e_cfies_indicator*factor(poverty_quartile) + e_fies_indicator*factor(poverty_quartile) +
                  e_cfies_indicator*age*factor(poverty_quartile) + e_fies_indicator*age*factor(poverty_quartile) +
                  female + age + factor(poverty_quartile) + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_18b <- pmap(va_ols_input_18b, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

notes_18b <- "Note: Poverty quartiles are constructed using ntile(), where Q1 is the least poor quartile and Q4 is the most poor quartile. Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child sex, child rank in percentile by age, region, and month fixed effects."

modelsummary(va_ols_results_18b,
             title     = 'Value-Added Model: Heterogeneity by Child Age and Poverty Quartile',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*quartile)",
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_18b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10c_poverty_quartiles.html",
             escape    = FALSE)

modelsummary(va_ols_results_18b,
             title     = '\\label{reg:age_pquart}Value-Added Model: Heterogeneity by Child Age and Poverty Quartile',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*quartile)",
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_18b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10c_poverty_quartiles.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
###################################### 3. Outcome Means Plot by Poverty Quartile #########
##########################################################################################

build_quartile_means <- function(data, design_label, col_var) {
  data %>%
    dplyr::select(poverty_quartile_label, !!sym(col_var), matches('^e_.*_per$')) %>%
    filter(!is.na(poverty_quartile_label), !is.na(!!sym(col_var))) %>%
    group_by(row_group = poverty_quartile_label, col_group = !!sym(col_var)) %>%
    summarize(
      across(matches('per'), ~ mean(.[is.finite(.)], na.rm = TRUE), .names = "mean.{col}"),
      .groups = "drop"
    ) %>%
    mutate(design = design_label)
}

for_ex_quartile <- bind_rows(
  build_quartile_means(full_data_w, "Poverty Quartile x Gender", "gender_group"),
  build_quartile_means(full_data_w, "Poverty Quartile x Age",     "age_group")
) %>%
  pivot_longer(c(-row_group, -col_group, -design), names_to = "category", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(group_label = paste(row_group, "-", col_group))

category_levels <- c('mean.e_lit_per', 'mean.e_num_per', 'mean.e_ef_per', 'mean.e_sel_per')
category_labels <- c('Literacy', 'Numeracy', 'Executive Function', 'Socioemotional Learning')

make_quartile_plot <- function(design_label) {
  
  plot_data <- for_ex_quartile %>%
    filter(design == design_label) %>%
    mutate(category = factor(category, levels = category_levels))
  
  plot_data %>%
    ggplot(aes(x = category, y = value, fill = group_label)) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = round(value, 2)),
              position = position_dodge(0.9), vjust = -0.5, size = 2.2) +
    labs(y = 'Mean Standardized Outcome', x = '', title = design_label) +
    scale_x_discrete(breaks = category_levels, labels = category_labels) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    theme_classic() +
    theme(
      axis.text  = element_text(color = 'black', size = 9),
      axis.ticks = element_line(color = 'black'),
      axis.line  = element_line(color = 'black'),
      legend.position = 'bottom',
      legend.title = element_blank()
    )
}

plot_quartile_gender <- make_quartile_plot("Poverty Quartile x Gender")
plot_quartile_age    <- make_quartile_plot("Poverty Quartile x Age")

plot_quartile_gender
plot_quartile_age

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10d_poverty_quartiles.png",
       plot = plot_quartile_gender, width = 28, height = 20, units = 'cm')

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/00_investigation/10e_poverty_quartiles.png",
       plot = plot_quartile_age, width = 28, height = 20, units = 'cm')