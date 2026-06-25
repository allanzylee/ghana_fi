###################################### Introduction ############################################

# Author: Allan Lee
# Date: [Date]
# Purpose: Run Value-Added OLS with 3-way interactions:
#          Child Sex x FI x Poverty (13a) and Child Sex x FI x Caregiver Education (13b)
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
# NOTE: cg_primary is now flipped: 1 = Caregiver Did NOT Complete Primary, 0 = Completed Primary

##########################################################################################
########### 13a: FI x Child Sex x Poverty (3-way) ######################################
##########################################################################################

va_ols_input_13a <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*female + e_fies_indicator*female +
                  e_cfies_indicator*poverty_std + e_fies_indicator*poverty_std +
                  e_cfies_indicator*female*poverty_std + e_fies_indicator*female*poverty_std +
                  female + poverty_std + age + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_13a <- pmap(va_ols_input_13a, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_13a <- c(
  'e_cfies_indicator'                    = "Child-Reported FI",
  'e_fies_indicator'                     = "Caregiver-Reported FI",
  'female'                               = "Child is Female",
  'poverty_std'                          = "Poverty (Std.)",
  'lagged_outcome'                       = "Lagged Outcome",
  'e_cfies_indicator:female'             = "Child-Reported FI $\\times$ Female",
  'e_fies_indicator:female'              = "Caregiver-Reported FI $\\times$ Female",
  'e_cfies_indicator:poverty_std'        = "Child-Reported FI $\\times$ Poverty",
  'e_fies_indicator:poverty_std'         = "Caregiver-Reported FI $\\times$ Poverty",
  'e_cfies_indicator:female:poverty_std' = "Child-Reported FI $\\times$ Female $\\times$ Poverty",
  'e_fies_indicator:female:poverty_std'  = "Caregiver-Reported FI $\\times$ Female $\\times$ Poverty",
  'treatment'                            = "Treatment",
  '(Intercept)'                          = "(Intercept)"
)

notes_13a <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Poverty is standardized to have mean zero and standard deviation one. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child age group, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_13a,
             title     = 'Value-Added Model: Heterogeneity by Child Sex and Poverty',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*poverty)",
             coef_map  = coef_map_13a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13a_va_ols_child_sex_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_13a,
             title     = '\\label{reg:sex_poverty}Value-Added Model: Heterogeneity by Child Sex and Poverty',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*poverty)",
             coef_map  = coef_map_13a,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13a,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13a_va_ols_child_sex_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
########### 13b: FI x Child Sex x Caregiver Primary Education (3-way) ##################
##########################################################################################

va_ols_input_13b <- expand.grid(
  category = c('lit', 'num', 'ef', 'sel'),
  model    = c('~ e_cfies_indicator + e_fies_indicator +
                  e_cfies_indicator*female + e_fies_indicator*female +
                  e_cfies_indicator*cg_primary + e_fies_indicator*cg_primary +
                  e_cfies_indicator*female*cg_primary + e_fies_indicator*female*cg_primary +
                  female + cg_primary + age + treatment +
                  region_north_east + region_northern + region_upper_east + region_upper_west +
                  age_pct_rank + factor(month) +')
)

va_ols_results_13b <- pmap(va_ols_input_13b, reg_func) %>%
  set_names('Literacy', 'Numeracy', 'EF', 'SEL')

coef_map_13b <- c(
  'e_cfies_indicator'                    = "Child-Reported FI",
  'e_fies_indicator'                     = "Caregiver-Reported FI",
  'female'                               = "Child is Female",
  'cg_primary'                           = "Caregiver Did Not Complete Primary",
  'lagged_outcome'                       = "Lagged Outcome",
  'e_cfies_indicator:female'             = "Child-Reported FI $\\times$ Female",
  'e_fies_indicator:female'              = "Caregiver-Reported FI $\\times$ Female",
  'e_cfies_indicator:cg_primary'         = "Child-Reported FI $\\times$ Caregiver Did Not Complete Primary",
  'e_fies_indicator:cg_primary'          = "Caregiver-Reported FI $\\times$ Caregiver Did Not Complete Primary",
  'e_cfies_indicator:female:cg_primary'  = "Child-Reported FI $\\times$ Female $\\times$ Caregiver Did Not Complete Primary",
  'e_fies_indicator:female:cg_primary'   = "Caregiver-Reported FI $\\times$ Female $\\times$ Caregiver Did Not Complete Primary",
  'treatment'                            = "Treatment",
  '(Intercept)'                          = "(Intercept)"
)

notes_13b <- "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates not shown include child age group, child rank in percentile by age, region, and month fixed effects."

# HTML
modelsummary(va_ols_results_13b,
             title     = 'Value-Added Model: Heterogeneity by Child Sex and Caregiver Education',
             fmt       = f,
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*cg_primary)",
             coef_map  = coef_map_13b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             gof_map   = gm,
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13b_va_ols_child_sex_by_wealth.html",
             escape    = FALSE)

# LaTeX
modelsummary(va_ols_results_13b,
             title     = '\\label{reg:sex_edu}Value-Added Model: Heterogeneity by Child Sex and Caregiver Education',
             cluster   = 'careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*female|.*treatment|.*cg_primary)",
             coef_map  = coef_map_13b,
             gof_omit  = 'AIC|BIC|Std.Errors',
             stars     = c('*' = .05, '**' = .01, '***' = .001),
             notes     = notes_13b,
             out       = "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13b_va_ols_child_sex_by_wealth.tex",
             latex_options = "scale_down",
             escape    = FALSE)

##########################################################################################
###################################### Plot Setup #########################################
##########################################################################################

model_sets_13 <- list(
  list(results = va_ols_results_13a, het_var = "female", sub_var = "poverty_std",
       het_label = "Child Sex", sub_label = "Poverty"),
  list(results = va_ols_results_13b, het_var = "female", sub_var = "cg_primary",
       het_label = "Child Sex", sub_label = "Caregiver Education")
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

triple_coefs_13 <- map_dfr(model_sets_13, extract_triple_coef) %>%
  mutate(
    outcome = factor(outcome, levels = c("Literacy", "Numeracy", "EF", "SEL")),
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

##########################################################################################
###################################### Plot 1: Triple-Interaction Coefficients ###########
##########################################################################################

plot_triple_coefs_13 <- triple_coefs_13 %>%
  ggplot(aes(x = outcome, y = estimate, color = fi_source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.5), size = 0.6) +
  facet_wrap(~ subgroup_dim) +
  labs(
    x = NULL,
    y = "Triple-Interaction Coefficient (95% CI)",
    color = "FI Measure",
    title = "Triple-Interaction Effects: FI x Child Sex x Sub-group"
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

plot_triple_coefs_13

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13c_triple_interaction_coefs.png",
       plot = plot_triple_coefs_13, width = 26, height = 16, units = "cm")

##########################################################################################
###################################### Extract Marginal Effects by Sub-group #############
##########################################################################################

extract_margins <- function(model_set) {
  
  het_var <- model_set$het_var
  sub_var <- model_set$sub_var
  
  het_vals         <- c(0, 1)
  het_value_labels <- c("Male", "Female")
  
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

margins_13 <- map_dfr(model_sets_13, extract_margins) %>%
  mutate(
    outcome     = factor(outcome, levels = c("Literacy", "Numeracy", "EF", "SEL")),
    ci_low      = estimate - 1.96 * std.error,
    ci_high     = estimate + 1.96 * std.error,
    group_label = paste(het_value, "x", sub_value)
  )

##########################################################################################
###################################### Plot 2: Marginal Effects by Sub-group #############
##########################################################################################

plot_margins_13 <- margins_13 %>%
  ggplot(aes(x = outcome, y = estimate, color = group_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.6), size = 0.5) +
  facet_grid(fi_source ~ subgroup_dim, scales = "free_x") +
  labs(
    x = NULL,
    y = "Marginal Effect of FI on Outcome (95% CI)",
    color = "Sub-group",
    title = "Marginal Effects of Food Insecurity by Child Sex and Sub-group"
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

plot_margins_13

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13d_marginal_effects_subgroups.png",
       plot = plot_margins_13, width = 32, height = 18, units = "cm")

##########################################################################################
###################################### Save Underlying Data ##############################
##########################################################################################

write_csv(triple_coefs_13, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13c_triple_interaction_coefs.csv")
write_csv(margins_13,      "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/02_fi_dummy_reg/13d_marginal_effects_subgroups.csv")