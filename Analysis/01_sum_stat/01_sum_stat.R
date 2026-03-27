###################################### Introduction ############################################

# Author: Allan Lee
# Date: December 30th, 2023
# Purpose: Calculate relevant summary statistics

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
  fastDummies::dummy_columns(c('e_cfies_scale', 'e_fies_scale')) %>%
  dplyr::select(-e_cfies_scale, -e_fies_scale) %>%
  mutate(disagree = as.integer(e_cfies_indicator != e_fies_indicator))


##########################################################################################
###################################### Helper: Summary Stat Function #####################
##########################################################################################

sum_stat_func <- function(var) {
  tribble(
    ~var, ~mean,                                                ~sd,
    var,  mean(full_data_w %>% pull(!!ensym(var)), na.rm = T), sd(full_data_w %>% pull(!!ensym(var)), na.rm = T)
  )
}


##########################################################################################
###################################### Run Summary Stats #################################
##########################################################################################

summed <- map_dfr(
  c(
    # Panel A: Food Insecurity Experiences
    'e_cfies_indicator',
    'e_fies_indicator',
    'disagree',
    'e_cfies_scale_0',
    'e_cfies_scale_1',
    'e_cfies_scale_2',
    'e_cfies_scale_3',
    'e_fies_scale_0',
    'e_fies_scale_1',
    'e_fies_scale_2',
    # Panel B: Child Characteristics
    'female',
    'age_num',
    'e_lit_per_raw',
    'e_num_per_raw',
    'e_ef_per_raw',
    'e_sel_per_raw',
    # Panel C: Region Characteristics
    'region_north_east',
    'region_northern',
    'region_savannah',
    'region_upper_east',
    'region_upper_west',
    # Panel D: Caregiver Characteristics
    'cg_female',
    'cg_age',
    'cg_primary'
  ),
  sum_stat_func
) %>%
  # Format numbers
  mutate(
    across(
      -var,
      ~ case_when(
        str_detect(var, 'age_num|cg_age') ~ formatC(.x, format = "f", big.mark = ",", digits = 1),
        TRUE                              ~ formatC(.x, format = "f", big.mark = ",", digits = 3)
      )
    )
  ) %>%
  # Relabel variables
  mutate(var = recode(
    var,
    'e_cfies_indicator'   = 'Child-Reported FI (\\%)',
    'e_fies_indicator'    = 'Caregiver-Reported FI (\\%)',
    'disagree'            = 'Child-Caregiver FI Disagreement (\\%)',
    'e_cfies_scale_0'     = 'Child: No FI (CFIES=0)',
    'e_cfies_scale_1'     = 'Child: Few Exp. (CFIES=1--6)',
    'e_cfies_scale_2'     = 'Child: Several Exp. (CFIES=7--10)',
    'e_cfies_scale_3'     = 'Child: Many Exp. (CFIES=11--20)',
    'e_fies_scale_0'      = 'Caregiver: Food Secure (FIES=0--3)',
    'e_fies_scale_1'      = 'Caregiver: Moderately Food Insecure (FIES=4--6)',
    'e_fies_scale_2'      = 'Caregiver: Severely Food Insecure (FIES=7--8)',
    'female'              = 'Child is Female (\\%)',
    'age_num'             = 'Child Age (Years)',
    'e_lit_per_raw'       = 'Literacy (\\% Correct)',
    'e_num_per_raw'       = 'Numeracy (\\% Correct)',
    'e_ef_per_raw'        = 'EF (\\% Correct)',
    'e_sel_per_raw'       = 'SEL (\\% Correct)',
    'region_north_east'   = 'North East',
    'region_northern'     = 'Northern',
    'region_savannah'     = 'Savannah',
    'region_upper_east'   = 'Upper East',
    'region_upper_west'   = 'Upper West',
    'cg_female'           = 'Caregiver is Female (\\%)',
    'cg_age'              = 'Caregiver Age (Years)',
    'cg_primary'          = 'Caregiver Completed Primary Education (\\%)'
  ))


##########################################################################################
###################################### Build LaTeX Table #################################
##########################################################################################

make_row <- function(df, i) {
  paste0(
    df$var[i],  " & ",
    df$mean[i], " & ",
    df$sd[i],   " \\\\\n"
  )
}

# Row index reference:
# Panel A (Food Insecurity):       rows 1-10
# Panel B (Child Characteristics): rows 11-16
# Panel C (Region):                rows 17-21
# Panel D (Caregiver):             rows 22-24

panel_a_rows <- paste0(
  # Indicator rates (rows 1-3: child FI, caregiver FI, disagreement)
  paste0(sapply(1:3, function(i) make_row(summed, i)), collapse = ""),
  "\\addlinespace\n",
  # Categorical FI definitions (rows 4-10)
  paste0(sapply(4:10, function(i) make_row(summed, i)), collapse = "")
)

panel_b_rows <- paste0(sapply(11:16, function(i) make_row(summed, i)), collapse = "")
panel_c_rows <- paste0(sapply(17:21, function(i) make_row(summed, i)), collapse = "")
panel_d_rows <- paste0(sapply(22:24, function(i) make_row(summed, i)), collapse = "")

# N row
n_row <- paste0(
  "Total Observations & ",
  formatC(nrow(full_data_w), big.mark = ","),
  " & \\\\\n"
)

latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Summary Statistics}\n",
  "\\label{tab:sumstats}\\small\n",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcc@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} \\\\\n",
  " & Mean & Std. Dev. \\\\\n",
  "\\midrule \\addlinespace\n",
  
  # Panel A
  "\\multicolumn{3}{@{}l}{\\emph{Panel A: Food Insecurity Experiences}} \\\\ \\addlinespace\n",
  panel_a_rows,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel B
  "\\multicolumn{3}{@{}l}{\\emph{Panel B: Child Characteristics}} \\\\ \\addlinespace\n",
  panel_b_rows,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel C
  
  "\\multicolumn{3}{@{}l}{\\emph{Panel C: Caregiver Characteristics}} \\\\ \\addlinespace\n",
  panel_d_rows,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  # Panel D
  
  "\\multicolumn{3}{@{}l}{\\emph{Panel D: Region}} \\\\ \\addlinespace\n",
  panel_c_rows,
  "\\addlinespace \\midrule \\addlinespace\n",
  
  

  # N
  n_row,
  
  "\\bottomrule\n",
  "\\end{tabular*}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Notes:} This table reports unweighted summary statistics.",
  " FI = Food Insecure. CFIES = Child Food Insecurity Experience Scale. FIES = Food Insecurity Experience Scale.",
  " Disagreement = 1 when child-reported and caregiver-reported food insecurity status differ.",
  " Outcome variables are reported as raw percentage correct.",
  " Region shares sum to one across the five northern regions of Ghana.\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table,
           "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/01_sum_stat.tex")