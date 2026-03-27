###################################### Introduction ############################################

# Author: Allan Lee
# Date: May 5th, 2025
# Purpose: Calculate the percentage of FI for overlapping items

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environments
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(tidyverse)
library(dplyr)
library(stargazer)
library(glue)
library(ltm)
library(xtable)
library(writexl)
library(knitr)
library(kableExtra)
library(flextable)

##########################################################################################
###################################### Load relevant data ################################
##########################################################################################

full_data_w <- read_rds('/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/regression_build_w.rds')
  
# Calculate overlap percentage ------------------------------------------------------
overlap_perc_raw<-full_data_w %>% 
  dplyr::select(childid,
         careid,
         matches("^fs\\d+"),
         female,
         age) %>% 
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>% 
  dplyr::select(childid,
                careid,
                female,
                age,
                # Worry about lack of food
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                # Size of meal cut
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                # Skipped meal
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                # Hungry but didn't eat
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
                )

# Create input
perc_diff_input<-expand.grid(cat=c('overall','female','male','older','younger','older_female','older_male','younger_female','younger_male'),
                             outcome=c('worry','cut','skip','hungry'))

# Create function to calculate percentage difference and pval
perc_diff_func<-function(cat,outcome){
  
  if(cat=='overall'){
    
    df=overlap_perc_raw
    
  } else if(cat=='female'){
    
    df=overlap_perc_raw %>% filter(female==1)
    
  } else if(cat=='male'){
    
    df=overlap_perc_raw %>% filter(female==0)
    
  } else if(cat=='older'){
    
    df=overlap_perc_raw %>% filter(age==1)
    
  } else if(cat=='younger'){
    
    df=overlap_perc_raw %>% filter(age==0)
    
  } else if(cat=='older_female'){
    
    df=overlap_perc_raw %>% filter(age==1, female==1)
    
  } else if(cat=='older_male'){
    
    df=overlap_perc_raw %>% filter(age==1,female==0)
     
  } else if(cat=='younger_female'){
    
    df=overlap_perc_raw %>% filter(age==0, female==1)
    
  } else {
    
    df=overlap_perc_raw %>% filter(age==0,female==0)
    
  }
  
  # Create outcome string
  ch_outcome=paste0(outcome,'_child')
  cg_outcome=paste0(outcome,'_cg')
  
  tab <- table(
    Child  = overlap_perc_raw[[ch_outcome]],
    Parent = overlap_perc_raw[[cg_outcome]]
  )
  
  mc=mcnemar.test(tab,correct = F)
  
  out <- df %>% 
    summarise(
      child_pct  = mean(!!ensym(ch_outcome), na.rm = TRUE) * 100,
      cg_pct = mean(!!ensym(cg_outcome),    na.rm = TRUE) * 100,
      N_pairs    = sum(!is.na(!!ensym(ch_outcome)) & !is.na(!!ensym(cg_outcome))),
      discordant_pct = mean(!!ensym(ch_outcome) != !!ensym(cg_outcome), na.rm = TRUE) * 100
    ) %>% 
    mutate(
      diff_pp = child_pct - cg_pct,
      pval    = mc$p.value,
      cat=cat,
      outcome=outcome
    )
  
}

# run function
overlap_perc<-pmap_dfr(perc_diff_input,
              perc_diff_func)

# perc_gender<-overlap_perc_raw%>% 
#   group_by(category,
#            female)%>% 
#   summarise(across(c("worry",
#                      'cut',
#                      'skip',
#                      'hungry'),
#                    ~mean(.,
#                          na.rm=T))) %>% 
#   mutate(type=case_when(female==1 ~ 'Child is Female',
#                          T~'Child is Male')) %>% 
#   ungroup() %>% 
#   dplyr::select(-female) %>% 
#   arrange(type)
# 
# perc_age<-overlap_perc_raw%>% 
#   group_by(category,
#            age)%>% 
#   summarise(across(c("worry",
#                      'cut',
#                      'skip',
#                      'hungry'),
#                    ~mean(.,
#                          na.rm=T))) %>% 
#   mutate(type=case_when(age==1 ~ 'Child is 10-17',
#                         T~'Child is 5-9')) %>% 
#   ungroup() %>% 
#   dplyr::select(-age)%>% 
#   arrange(type)
# 
# perc_gender_age<-overlap_perc_raw%>% 
#   group_by(category,
#            age,
#            female)%>% 
#   summarise(across(c("worry",
#                      'cut',
#                      'skip',
#                      'hungry'),
#                    ~mean(.,
#                          na.rm=T))) %>% 
#   mutate(type=case_when(age==1 & female==1 ~ 'Child is Female (10-17)',
#                         age==1 & female==0 ~ 'Child is Male (10-17)',
#                         age==0 & female==1 ~ 'Child is Female (5-9)',
#                         T~'Child is Male (5-9)')) %>% 
#   ungroup() %>% 
#   dplyr::select(-age,
#                 -female) %>% 
#   arrange(type)
# 
# overlap_perc=bind_rows(perc_overall,
#                        perc_gender,
#                        perc_age,
#                        perc_gender_age)

# Create function for overlap cor ------------------------------------------------------

# Create data cor correlation 
overlap_cor_df<-full_data_w %>% 
  dplyr::select(childid,
                careid,
                age,
                female,
                matches("^fs\\d+")) %>% 
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>% 
  dplyr::select(childid,
                careid,
                age,
                female,
                # Worry about lack of food
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                # Size of meal cut
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                # Skipped meal
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                # Hungry but didn't eat
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
  )

cor_func<-function(by_var=NULL){
  
  cor<-overlap_cor_df %>% 
    group_by(across({{by_var}})) %>% 
    summarise(worry=list(cor.test(worry_child, worry_cg)),
              cut=list(cor.test(cut_child, cut_cg)),
              skip=list(cor.test(skip_child, skip_cg)),
              hungry=list(cor.test(hungry_child, hungry_cg)))
  
  if(length(by_var)<=1){
    
    if(is.null(by_var)){
      
      out<-tribble(~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']]) %>% 
        mutate(category='Overall')
      
    } else if(by_var=='female') {
      
      out<-tribble(~female, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$female[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                   cor$female[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']]) %>% 
        mutate(category=case_when(female==1 ~ 'Child is Female',
                                  T~'Child is Male')) %>% 
        dplyr::select(-female)
      
    } else{
      
      out<-tribble(~age, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$age[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                   cor$age[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']]) %>% 
        mutate(category=case_when(age==1 ~ 'Child is 10-17',
                                  T~'Child is 5-9')) %>% 
        dplyr::select(-age)
      
    }
    
  } else {
    
    out<-tribble(~age,~female, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                 cor$age[1],cor$female[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                 cor$age[2],cor$female[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']],
                 cor$age[3],cor$female[3],cor$worry[[3]][['estimate']], cor$worry[[3]][['p.value']], cor$cut[[3]][['estimate']], cor$cut[[3]][['p.value']],cor$skip[[3]][['estimate']], cor$skip[[3]][['p.value']], cor$hungry[[3]][['estimate']], cor$hungry[[3]][['p.value']],
                 cor$age[4],cor$female[4],cor$worry[[4]][['estimate']], cor$worry[[4]][['p.value']], cor$cut[[4]][['estimate']], cor$cut[[4]][['p.value']],cor$skip[[4]][['estimate']], cor$skip[[4]][['p.value']], cor$hungry[[4]][['estimate']], cor$hungry[[4]][['p.value']])%>% 
      mutate(category=case_when(age==1 & female==1 ~ 'Child is Female (10-17)',
                                age==1 & female==0 ~ 'Child is Male (10-17)',
                                age==0 & female==1 ~ 'Child is Female (5-9)',
                                T~'Child is Male (5-9)')) %>% 
      dplyr::select(-age,
                    -female)
  }
}

overlap_cor=map_dfr(list(c(NULL),
                 c('age'),
                 c('female'),
                 c('age','female')
                 ),
            cor_func)

# Export
to_export<-list('cor'=as_tibble(overlap_cor),
             'perc'=as_tibble(overlap_perc))

write_xlsx(to_export,
          "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/08_fi_overlap_items.xlsx")

#####################################################################################################################
####################################### LaTex Version ###############################################################
#####################################################################################################################

### Correlation

latex_cor<-xtable(overlap_cor %>% 
         dplyr::select('Category'=category,
                'Worry'=worry,
                'Cut'=cut,
                'Skip'=skip,
                'Hungry'=hungry))

print(latex_cor, type = "latex", "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/08_fi_overlap_item_cor.tex")

### Percentage

latex_perc=overlap_perc %>% 
  mutate(
    group = factor(cat, levels = unique(as.character(perc_diff_input$cat)))
  ) %>%
  mutate(
    group_label = recode(
      group,
      "overall"        = "Overall",
      "female"         = "Child is Female",
      "male"           = "Child is Male",
      "older"          = "Child is 10-17",
      "younger"        = "Child is 5-9",
      "older_female"   = "Child is Female (10-17)",
      "older_male"     = "Child is male (10-17)",
      "younger_female" = "Child is Female (5-9)",
      "younger_male"   = "Child is Male (5-9)"
    )
  ) %>% 
  pivot_wider(
    names_from  = cat,
    values_from = c(child_pct, cg_pct, pval)
  ) %>%
  arrange(group) %>% 
  mutate(child_pct = coalesce(!!!dplyr::select(., dplyr::starts_with("child_pct"))),
         cg_pct = coalesce(!!!dplyr::select(., dplyr::starts_with("cg_pct"))),
         pval = coalesce(!!!dplyr::select(., dplyr::starts_with("pval")))) %>% 
  dplyr::select(group_label, outcome, child_pct,cg_pct,pval) %>% 
  pivot_wider(
    names_from  = outcome,
    values_from = c(child_pct, cg_pct, pval)
  ) %>% 
  dplyr::select(group_label,contains('worry'),contains('cut'),contains('skip'),contains('hungry')) %>% 
  mutate(across(contains("pval"),
                ~ sprintf("%.2f", .)),
         across(
           matches("child|cg"),
           ~ paste0(sprintf("%.2f", .), "\\%"))) %>% 
  select(-contains('pval'))
  
out<-kable(
  latex_perc,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  col.names = c(
    "Group",
    rep(c("Child (\\%)", "Caregiver (\\%)"), 4)
  ),
  align = "lcccccccccccc",
  escape = FALSE
) %>%
  add_header_above(c(
    " "      = 1,
    "Worry"  = 2,
    "Cut"    = 2,
    "Skip"   = 2,
    "Hungry" = 2
  )) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  )

writeLines(out, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Output/01_sum_stat/08_fi_overlap_item_perc.tex")


# 
#   xtable(overlap_perc %>% 
#          dplyr::select('Category'=type,
#                        'Worry'=worry,
#                        'Cut'=cut,
#                        'Skip'=skip,
#                        'Hungry'=hungry,
#                        'Reported by'=category) %>% 
#   mutate(`Reported by`=case_when(`Reported by`=='cg'~"Caregiver",
#                                  T~'Child')))
# 
# print(latex_perc,
#       include.rownames=FALSE)
# 
# # Calculate percentage difference mean
# perc_diff_mean<-overlap_perc %>% 
#   group_by(type) %>% 
#   mutate(across(c(worry,
#                   cut,
#                   skip,
#                   hungry),~.[category=='cg']-.[category=='child'])) %>% 
#   ungroup() %>% 
#   distinct(worry,
#            cut,
#            skip,
#            hungry) %>% 
#   summarise(across(c(worry,
#                      cut,
#                      skip,
#                      hungry),~mean(.,na.rm=T))) %>% 
#   rowwise() %>% 
#   summarise(
#             mean = mean(c_across(worry:hungry)),
#             median = median(c_across(worry:hungry))) 
