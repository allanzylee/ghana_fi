###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 13th, 2024
# Purpose: Calculate outcome summary statistics by food insecurity group

##########################################################################################
############################################### Set up ###################################
##########################################################################################

rm(list=ls())
source("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Code/Analysis/header.R")

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

############################# Construct Data ###############################

data <- full_data_w %>% 
  filter(!is.na(e_cfies_scale), !is.na(e_fies_scale)) %>% 
  mutate(
    e_lit_per_raw = if_else(m_lit_per_raw == 0, NA_real_, e_lit_per_raw - m_lit_per_raw ),
    e_num_per_raw = if_else(m_num_per_raw == 0, NA_real_, e_num_per_raw - m_num_per_raw ),
    e_ef_per_raw  = if_else(m_ef_per_raw  == 0, NA_real_, e_ef_per_raw  - m_ef_per_raw  ),
    e_sel_per_raw = if_else(m_sel_per_raw == 0, NA_real_, e_sel_per_raw - m_sel_per_raw )
  ) %>% 
  dplyr::select(careid, childid, age, female,
                e_cfies_scale, e_fies_scale,
                matches('^e_.*per_raw$'))

test=full_data_w %>% 
  dplyr::select(careid, childid, age, female,
                e_cfies_scale, e_fies_scale,
                matches('^e_.*per_raw$'),
                matches('^m_.*per_raw$'))

############################# Summary Stats by Scale Group ###############################
outcome_sum_stat_scale_func <- function(var_group_str, label_map) {
  
  var_group <- ensym(var_group_str)
  
  data %>% 
    dplyr::select(!!var_group, matches('^e_.*per_raw$')) %>%
    filter(!is.na(!!var_group)) %>%
    group_by(group = !!var_group) %>% 
    summarize(
      across(
        matches('per'),
        ~ mean(.[is.finite(.)], na.rm = TRUE),
        .names = "mean.{col}"
      )
    ) %>%
    mutate(
      group = label_map[as.character(group)],
      scale = var_group_str
    )
}

# Label maps
cfies_labels <- c(
  "0" = "CFIES: No Experiences",
  "1" = "CFIES: Few Experiences",
  "2" = "CFIES: Several Experiences",
  "3" = "CFIES: Many Experiences"
)

fies_labels <- c(
  "0" = "FIES: Mild",
  "1" = "FIES: Moderate",
  "2" = "FIES: Severe"
)

# Factor levels for ordered display
cfies_levels <- c("CFIES: No Experiences", "CFIES: Few Experiences",
                  "CFIES: Several Experiences", "CFIES: Many Experiences")
fies_levels  <- c("FIES: Mild", "FIES: Moderate", "FIES: Severe")

# Combine
for_ex <- bind_rows(
  outcome_sum_stat_scale_func('e_cfies_scale', cfies_labels),
  outcome_sum_stat_scale_func('e_fies_scale',  fies_labels)
) %>%
  pivot_longer(c(-group, -scale), names_to = 'category', values_to = 'value') %>%
  filter(!is.na(value))

############################# Create Exhibit ###############################

plot <- for_ex %>% 
  mutate(
    category = factor(category,
                      levels = c('mean.e_lit_per_raw', 'mean.e_num_per_raw',
                                 'mean.e_ef_per_raw',  'mean.e_sel_per_raw')),
    group = factor(group, levels = c(cfies_levels, fies_levels))
  ) %>% 
  ggplot(aes(x = category, y = value, fill = group)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = glue('{round(value, 3) * 100}%')),
            position = position_dodge(0.9), vjust = -0.5, size = 2.5) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, .75),
  #                    labels = scales::percent, breaks = seq(0, .75, .25)) +
  labs(y = 'Mean Percentage Accuracy', x = '') +
  scale_x_discrete(
    breaks = c('mean.e_lit_per_raw', 'mean.e_num_per_raw',
               'mean.e_ef_per_raw',  'mean.e_sel_per_raw'),
    labels = c('Literacy', 'Numeracy', 'Executive Function', 'Socioemotional Learning')
  ) +
  scale_fill_manual(
    values = c(
      # CFIES: white -> dark blue (no FI -> many experiences)
      "CFIES: No Experiences"      = "#ddeef6",
      "CFIES: Few Experiences"     = "#a8c8e8",
      "CFIES: Several Experiences" = "#4a90c4",
      "CFIES: Many Experiences"    = "#1a4a7a",
      # FIES: white -> dark red (no FI -> severe)
      "FIES: Mild"   = "#f6dddd",
      "FIES: Moderate"             = "#c44a4a",
      "FIES: Severe"               = "#7a1a1a"
    )
  ) +
  theme_classic() +
  theme(
    axis.text  = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    axis.line  = element_line(color = 'black'),
    legend.position = 'bottom',
    legend.title = element_blank()
  )

plot

ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/06e_outcome_diff_fi_cat.png",
       width = 25, height = 25, units = 'cm')
