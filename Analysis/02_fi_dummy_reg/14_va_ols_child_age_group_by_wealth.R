###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Run Value-Added OLS with 3-way interactions:
#          Child Age x FI x Poverty (14a) and Child Age x FI x Caregiver Education (14b)
#          + Plot triple-interaction coefficients and marginal effects

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
  mutate(poverty_std = as.numeric(scale(poverty)),
         cg_primary = case_when(cg_primary == 1 ~ 0,
                                T ~ 1))
# NOTE: cg_primary is flipped: 1 = Caregiver Did NOT Complete Primary, 0 = Completed Primary

##########################################################################################
########### 14a: FI x Child Age x Poverty (3-way) ######################################
##########################################################################################

va_ols_input_14a <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*age + e_fies_indicator*age +
                  e_cfies_indicator*poverty_std + e_fies_indicator*poverty_std +
                  e_cfies_indicator*age*poverty_std + e_fies_indicator*age*poverty_std +
                  female + age + poverty_std + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_14a <- pmap(va_ols_input_14a, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_14a <- c(
  'e_cfies_indicator'                  = "Child-Reported FI",
  'e_fies_indicator'                   = "Caregiver-Reported FI",
  'age'                                = "Child is 10--17",
  'poverty_std'                        = "Poverty (Std.)",
  'lagged_outcome'                     = "Lagged Outcome",
  'e_cfies_indicator:age'              = "Child-Reported FI $\\times$ Child is 10--17",
  'e_fies_indicator:age'               = "Caregiver-Reported FI $\\times$ Child is 10--17",
  'e_cfies_indicator:poverty_std'      = "Child-Reported FI $\\times$ Poverty",
  'e_fies_indicator:poverty_std'       = "Caregiver-Reported FI $\\times$ Poverty",
  'e_cfies_indicator:age:poverty_std'  = "Child-Reported FI $\\times$ Child is 10--17 $\\times$ Poverty",
  'e_fies_indicator:age:poverty_std'   = "Caregiver-Reported FI $\\times$ Child is 10--17 $\\times$ Poverty",
  'treatment'                          = "Treatment",
  '(Intercept)'                        = "(Intercept)"
)

notes_14a <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Poverty is standardized to have mean zero and standard deviation one. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child sex, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_14a,
             title     = 'Value-Added Model: Heterogeneity by Child Age and Poverty',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*poverty)",
             coef_map  = coef_map_14a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14a_va_ols_child_age_group_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_14a,
             title     = '\\label{reg:age_poverty}Value-Added Model: Heterogeneity by Child Age and Poverty',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*poverty)",
             coef_map  = coef_map_14a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14a_va_ols_child_age_group_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
########### 14b: FI x Child Age x Caregiver Primary Education (3-way) ##################
##########################################################################################

va_ols_input_14b <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*age + e_fies_indicator*age +
                  e_cfies_indicator*cg_primary + e_fies_indicator*cg_primary +
                  e_cfies_indicator*age*cg_primary + e_fies_indicator*age*cg_primary +
                  female + age + cg_primary + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_14b <- pmap(va_ols_input_14b, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_14b <- c(
  'e_cfies_indicator'                   = "Child-Reported FI",
  'e_fies_indicator'                    = "Caregiver-Reported FI",
  'age'                                 = "Child is 10--17",
  'cg_primary'                          = "Caregiver Did Not Complete Primary",
  'lagged_outcome'                      = "Lagged Outcome",
  'e_cfies_indicator:age'               = "Child-Reported FI $\\times$ Child is 10--17",
  'e_fies_indicator:age'                = "Caregiver-Reported FI $\\times$ Child is 10--17",
  'e_cfies_indicator:cg_primary'        = "Child-Reported FI $\\times$ Caregiver Did Not Complete Primary",
  'e_fies_indicator:cg_primary'         = "Caregiver-Reported FI $\\times$ Caregiver Did Not Complete Primary",
  'e_cfies_indicator:age:cg_primary'    = "Child-Reported FI $\\times$ Child is 10--17 $\\times$ Caregiver Did Not Complete Primary",
  'e_fies_indicator:age:cg_primary'     = "Caregiver-Reported FI $\\times$ Child is 10--17 $\\times$ Caregiver Did Not Complete Primary",
  'treatment'                           = "Treatment",
  '(Intercept)'                         = "(Intercept)"
)

notes_14b <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child sex, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_14b,
             title     = 'Value-Added Model: Heterogeneity by Child Age and Caregiver Education',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*cg_primary)",
             coef_map  = coef_map_14b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14b_va_ols_child_age_group_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_14b,
             title     = '\\label{reg:age_edu}Value-Added Model: Heterogeneity by Child Age and Caregiver Education',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*age|.*treatment|.*cg_primary)",
             coef_map  = coef_map_14b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_14b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14b_va_ols_child_age_group_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
###################################### Plot Setup #########################################
##########################################################################################

model_sets_14 <- list(
  list(results = va_ols_results_14a, het_var = "age", sub_var = "poverty_std",
       het_label = "Child Age", sub_label = "Poverty"),
  list(results = va_ols_results_14b, het_var = "age", sub_var = "cg_primary",
       het_label = "Child Age", sub_label = "Caregiver Education")
)

##########################################################################################
###################################### Extract Triple-Interaction Coefficients ###########
##########################################################################################

extract_triple_coef <- function(model_set) {
  
  het_var <- model_set$het_var
  sub_var <- model_set$sub_var
  
  cfies_term <- paste0("e_cfies_indicator:", het_var, ":", sub_var)
  fies_term  <- paste0("e_fies_indicator:",  het_var, ":", sub_var)
  
  map_dfr(model_set$results, function(m) {
    broom::tidy(m) %>% filter(term %in% c(cfies_term, fies_term))
  }, .id = "outcome") %>%
    mutate(
      fi_source     = if_else(str_detect(term, "e_cfies"), "Child-Reported FI", "Caregiver-Reported FI"),
      heterogeneity = model_set$het_label,
      subgroup_dim  = model_set$sub_label
    )
}

triple_coefs_14 <- map_dfr(model_sets_14, extract_triple_coef) %>%
  mutate(
    outcome = factor(outcome, levels = c("Literacy", "Numeracy", "EF", "SEL")),
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

##########################################################################################
###################################### Plot 1: Triple-Interaction Coefficients ###########
##########################################################################################

plot_triple_coefs_14 <- triple_coefs_14 %>%
  ggplot(aes(x = outcome, y = estimate, color = fi_source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.5), size = 0.6) +
  facet_wrap(~ subgroup_dim) +
  labs(
    x = NULL,
    y = "Triple-Interaction Coefficient (95% CI)",
    color = "FI Measure",
    title = "Triple-Interaction Effects: FI x Child Age x Sub-group"
  ) +
  scale_color_manual(values = c("Child-Reported FI" = "#4a90c4", "Caregiver-Reported FI" = "#c44a4a")) +
  theme_classic() +
  theme(
    axis.text   = element_text(color = "black", size = 10),
    axis.ticks  = element_line(color = "black"),
    axis.line   = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.spacing = unit(1, "lines")
  )

plot_triple_coefs_14

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14c_triple_interaction_coefs.png",
       plot = plot_triple_coefs_14, width = 26, height = 16, units = "cm")

##########################################################################################
###################################### Extract Marginal Effects by Sub-group #############
##########################################################################################

extract_margins <- function(model_set) {
  
  het_var <- model_set$het_var
  sub_var <- model_set$sub_var
  
  het_vals         <- c(0, 1)
  het_value_labels <- c("Under 10", "Age 10-17")
  
  if (sub_var == "poverty_std") {
    sub_vals         <- c(-1, 1)
    sub_value_labels <- c("Below Median Poverty", "Above Median Poverty")
  } else {
    sub_vals         <- c(0, 1)
    sub_value_labels <- c("Caregiver Completed Primary", "Caregiver Did Not Complete Primary")
  }
  
  grid <- expand.grid(het_val = het_vals, sub_val = sub_vals)
  
  map_dfr(model_set$results, function(m) {
    
    map_dfr(seq_len(nrow(grid)), function(i) {
      
      newdata_vals <- setNames(list(grid$het_val[i], grid$sub_val[i]), c(het_var, sub_var))
      
      cfies_mfx <- avg_slopes(m, variables = "e_cfies_indicator",
                              newdata = datagrid(model = m, !!!newdata_vals)) %>%
        as_tibble() %>%
        mutate(fi_source = "Child-Reported FI")
      
      fies_mfx <- avg_slopes(m, variables = "e_fies_indicator",
                             newdata = datagrid(model = m, !!!newdata_vals)) %>%
        as_tibble() %>%
        mutate(fi_source = "Caregiver-Reported FI")
      
      bind_rows(cfies_mfx, fies_mfx) %>%
        mutate(
          het_value = het_value_labels[match(grid$het_val[i], het_vals)],
          sub_value = sub_value_labels[match(grid$sub_val[i], sub_vals)]
        )
    })
    
  }, .id = "outcome") %>%
    mutate(heterogeneity = model_set$het_label, subgroup_dim = model_set$sub_label)
}

margins_14 <- map_dfr(model_sets_14, extract_margins) %>%
  mutate(
    outcome     = factor(outcome, levels = c("Literacy", "Numeracy", "EF", "SEL")),
    ci_low      = estimate - 1.96 * std.error,
    ci_high     = estimate + 1.96 * std.error,
    group_label = paste(het_value, "x", sub_value)
  )

##########################################################################################
###################################### Plot 2: Marginal Effects by Sub-group #############
##########################################################################################

plot_margins_14 <- margins_14 %>%
  ggplot(aes(x = outcome, y = estimate, color = group_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.6), size = 0.5) +
  facet_grid(fi_source ~ subgroup_dim, scales = "free_x") +
  labs(
    x = NULL,
    y = "Marginal Effect of FI on Outcome (95% CI)",
    color = "Sub-group",
    title = "Marginal Effects of Food Insecurity by Child Age and Sub-group"
  ) +
  theme_classic() +
  theme(
    axis.text   = element_text(color = "black", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks  = element_line(color = "black"),
    axis.line   = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.spacing = unit(1, "lines")
  )

plot_margins_14

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14d_marginal_effects_subgroups.png",
       plot = plot_margins_14, width = 32, height = 18, units = "cm")

##########################################################################################
###################################### Save Underlying Data ##############################
##########################################################################################

write_csv(triple_coefs_14, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14c_triple_interaction_coefs.csv")
write_csv(margins_14,      "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/14d_marginal_effects_subgroups.csv")