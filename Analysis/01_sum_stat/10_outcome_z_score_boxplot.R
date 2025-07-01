###################################### Introduction ############################################

# Author: Allan Lee
# Date: July 1, 2025
# Purpose: Calculate outcome summary static by food insecurity group

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

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')

############################# Create plot ###############################

data<-full_data_w %>% 
  dplyr::select(e_ch_fs_dummy,
                e_cg_fs_dummy,
                matches('^e_.*per$')) %>% 
  pivot_longer(matches('^e_.*per$'),
               names_to = 'outcome_cat',
               values_to = 'outcome') %>% 
  pivot_longer(matches('dummy'),
               names_to = 'reported_by',
               values_to='fi_status') %>% 
  mutate(report=paste0(reported_by,fi_status),
         outcome_cat=factor(outcome_cat,
                            levels=c('e_sel_per',
                                     'e_ef_per',
                                     'e_num_per',
                                     'e_lit_per')))

plot<-ggplot(data=data,
  aes(
    x = outcome,
    y = outcome_cat,
    fill = report
  )
) + 
  geom_boxplot(outlier.shape=NA) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(-2.5,2.5),
                     # labels = scales::percent,
                     breaks=seq(-2.5,2.5,.5)) +
  labs(
    # title="Endline Child Cognitive and Socioemotional Outcomes by Group",
    y='',
    x='Outcome Z-Score') +
  scale_y_discrete(breaks=c('e_lit_per',
                            'e_num_per',
                            'e_ef_per',
                            'e_sel_per'),
                   labels=c('Literacy',
                            'Numeracy',
                            'EF',
                            'SEL'))+
  scale_fill_manual(values=c('#c5c6d0',
                             '#7f7f7f',
                             '#404040',
                             'black'),
                    breaks=c('e_ch_fs_dummy1',
                             'e_ch_fs_dummy0',
                             'e_cg_fs_dummy1',
                             'e_cg_fs_dummy0'),
                    labels=c('Child Reported Severe FI',
                             'Child Did Not Report Severe FI',
                             'Household Reported Severe FI',
                             'Household Did Not Report Severe FI')) +
  theme_classic()+
  theme(
    axis.text = element_text(color='black',
                             size=10),
    axis.ticks = element_line(color='black'),
    axis.line = element_line(color='black'),
    legend.position = 'bottom',
    legend.title=element_blank(),
    plot.margin = margin(.25, .25, .25, .25, "cm")
  ) +
  geom_vline(xintercept = 0,
             color='red',
             linetype='dashed')


plot

# Export as PDF
ggsave("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/10_outcome_z_score_boxplot.png",
       width=25,
       height=25,
       units='cm')
