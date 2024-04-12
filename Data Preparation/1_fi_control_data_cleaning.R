###################################### Introduction ############################################

# Author: Allan Lee
# Date: Feb 4th, 2023
# Purpose: Create child reported and caregiver reported food insecurity variables

##########################################################################################
############################################### Set up ###################################
##########################################################################################

# Clear the environment
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
# library(foreign)
library(haven)
# library(tidyverse)
library(dplyr)
# library(stargazer)
# library(psych)
# library(corrr)
# library(tibble)
# library(writexl)
# library(timechange)
# library(rnoaa)
# library(base)
# library(arsenal)
# library(labelled)
# library(zoo)
library(janitor)

##########################################################################################
###################################### FS data cleaning ##################################
##########################################################################################
### Change the ordinal form of the FS data and replace NAs with row means

fs_cols_child<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8","fs9","fs10")
fs_cols_cg<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8")

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate_at(fs_cols_child,as.numeric) %>% 
  mutate_at(fs_cols_child,funs(new=case_when(. == 1 ~ 2,
                                             . == 2 ~ 1,
                                             . == 3 ~ 0,
                                             TRUE ~ NA_real_))) %>% 
  dplyr::select(-all_of(fs_cols_child))
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta")
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta") %>% 
  rename(careid=caseid) %>% 
  mutate_at(fs_cols_child,as.numeric) %>% 
  mutate_at(fs_cols_child,funs(new=case_when(. == 1 ~ 2,
                                             . == 2 ~ 1,
                                             . == 3 ~ 0,
                                             TRUE ~ NA_real_))) %>% 
  dplyr::select(-all_of(fs_cols_child))
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta") %>% 
  mutate_at(fs_cols_cg,as.numeric)

baseline_enrollment_reg<-read_dta("import/Enrolment & Caregiver Survey_depii.dta") %>% 
  mutate(careid=as.double(careid))

# # Denote the relevant fs variables
# fs_cols_child<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8","fs9","fs10")
# fs_cols_cg<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8")
# 
# # Change the categorical response of FS for each set of data such that higher numbers means more food insecurity
# 
# m_child <- m_child %>% 
#   mutate_at(fs_cols_child,as.numeric) %>% 
#   mutate_at(fs_cols_child,funs(new=case_when(. == 1 ~ 2,
#                                        . == 2 ~ 1,
#                                        . == 3 ~ 0,
#                                        TRUE ~ NA_real_))) %>% 
#   dplyr::select(-all_of(fs_cols_child))
# 
# m_cg <- m_cg %>% 
#   mutate_at(fs_cols_cg,as.numeric)

# Change column names back to the original
names(m_child) = gsub(pattern = "_new", replacement = "", x = names(m_child))
names(e_child) = gsub(pattern = "_new", replacement = "", x = names(e_child))

# # dplyr::select relevant variables in the food security data and convert them to numeric.
# 
m_ch_fs <- m_child %>%
  dplyr::select(careid,childid,starts_with("fs"))
m_cg_fs <- m_cg %>%
  dplyr::select(childid,careid,starts_with("fs")) %>%
  distinct(.keep_all = T)
e_ch_fs <- e_child %>%
  dplyr::select(careid,childid,starts_with("fs")) %>%
  dplyr::select(-fs_id)
e_cg_fs <- e_cg %>%
  dplyr::select(childid,careid,starts_with("fs")) %>%
  distinct(.keep_all = T) %>%
  dplyr::select(-fsid)

# I will replace the NAs with an average of all other FS values.
# m_ch_fs[fs_cols_child] <- apply(m_ch_fs[fs_cols_child], 2, function(x) ifelse(is.na(x), rowMeans(m_ch_fs[fs_cols_child], na.rm = TRUE), x))
# m_cg_fs[fs_cols_cg] <- apply(m_cg_fs[fs_cols_cg], 2, function(x) ifelse(is.na(x), rowMeans(m_cg_fs[fs_cols_cg], na.rm = TRUE), x))
# e_ch_fs[fs_cols_child] <- apply(e_ch_fs[fs_cols_child], 2, function(x) ifelse(is.na(x), rowMeans(e_ch_fs[fs_cols_child], na.rm = TRUE), x))
# e_cg_fs[fs_cols_cg] <- apply(e_cg_fs[fs_cols_cg], 2, function(x) ifelse(is.na(x), rowMeans(e_cg_fs[fs_cols_cg], na.rm = TRUE), x))

# Drop the remaining rows in ch_fs that have only NAs.
m_ch_fs<-m_ch_fs %>%
  na.omit()
m_cg_fs<-m_cg_fs %>%
  na.omit()
e_ch_fs<-e_ch_fs %>%
  na.omit()
e_cg_fs<-e_cg_fs %>%
  na.omit()

# Join the data by midline and endline
# m_fs <- m_ch_fs %>% 
#   left_join(m_cg_fs,by=c("careid")) %>%
#   distinct(.keep_all = TRUE) %>% 
#   rename(fs9_child=fs9,
#          fs10_child=fs10)
# e_fs <- e_ch_fs %>% 
#   left_join(e_cg_fs,by=c("careid")) %>% 
#   distinct(.keep_all = TRUE) %>% 
#   rename(fs9_child=fs9,
#          fs10_child=fs10)

# Rename FS variables to note child and parent
# names(m_fs) = gsub(pattern = ".x", replacement = "_child", x = names(m_fs))
# names(m_fs) = gsub(pattern = ".y", replacement = "_cg", x = names(m_fs))
# names(e_fs) = gsub(pattern = ".x", replacement = "_child", x = names(e_fs))
# names(e_fs) = gsub(pattern = ".y", replacement = "_cg", x = names(e_fs))

# ##########################################################################################
# ############################################## PCA #######################################
# ##########################################################################################
# 
# ############################################## Child: Midline #######################################
# 
# # Conduct PCA for food insecurity and evaluate the Screeplot to determine significant features.
# m_ch_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8+fs9+fs10,
#                   data=m_ch_fs,
#                   scale=T)
# summary(m_ch_fs_pca)
# screeplot(m_ch_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Child Midline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, only the first principal component is significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# m_ch_fs_pc<-m_ch_fs_pca$x[,1]
# m_ch_fs_pc<-cbind(m_ch_fs,m_ch_fs_pc) 
# 
# ############################################## Child: Endline #######################################
# 
# # Conduct PCA for food insecurity and evaluate the Screeplot to determine significant features.
# e_ch_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8+fs9+fs10,
#                     data=e_ch_fs,
#                     scale=T)
# summary(e_ch_fs_pca)
# screeplot(e_ch_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Child Endline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, only the first principal component is significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# e_ch_fs_pc<-e_ch_fs_pca$x[,1]
# e_ch_fs_pc<-cbind(e_ch_fs,e_ch_fs_pc) 
# 
# ############################################## Caregiver: Midline #######################################
# 
# m_cg_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8,
#                     data=m_cg_fs,
#                     scale=T)
# summary(m_cg_fs_pca)
# screeplot(m_cg_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Caregiver Midline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, only the first principal component is significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# m_cg_fs_pc<-m_cg_fs_pca$x[,1]
# m_cg_fs_pc<-cbind(m_cg_fs,m_cg_fs_pc) 
# 
# ############################################## Caregiver: Endline #######################################
# 
# e_cg_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8,
#                     data=e_cg_fs,
#                     scale=T)
# summary(e_cg_fs_pca)
# screeplot(e_cg_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Caregiver Endline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, only the first principal component is significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# e_cg_fs_pc<-e_cg_fs_pca$x[,1]
# e_cg_fs_pc<-cbind(e_cg_fs,e_cg_fs_pc) 
# 
# ############################################## All: Midline #######################################
# 
# m_fs_pca<-prcomp(~fs1_child+fs2_child+fs3_child+fs4_child+fs5_child+fs6_child+fs7_child+fs8_child+fs9_child+fs10_child+
#                  fs1_cg+fs2_cg+fs3_cg+fs4_cg+fs5_cg+fs6_cg+fs7_cg+fs8_cg,
#                     data=m_fs,
#                     scale=T)
# summary(m_fs_pca)
# screeplot(m_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Midline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, the first principal components are significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# m_fs_pc<-m_fs_pca$x[,1:3]
# m_fs_pc<-cbind(m_fs %>% na.omit(),m_fs_pc) 
# 
# # Calculate correlation
# m_fs_cor<-cor(m_fs_pc[,3:20],m_fs_pc[,21:23])
# stargazer(m_fs_cor,
#           header=FALSE, 
#           type='latex',
#           title = "Table 2.6: Midline Food Insecurity PCA Correlation Matrix",
#           column.labels=c("FS1: Child","FS2: Child","FS3: Child", "FS4: Child","FS5: Child","FS6: Child","FS7: Child","FS8: Child","FS9: Child","FS10: Child",
#                              "FS1: Caregiver","FS2: Caregiver","FS3: Caregiver","FS4: Caregiver","FS5: Caregiver","FS6: Caregiver","FS7: Caregiver","FS8: Caregiver"))
# 
# ############################################## All: Endline #######################################
# 
# e_fs_pca<-prcomp(~fs1_child+fs2_child+fs3_child+fs4_child+fs5_child+fs6_child+fs7_child+fs8_child+fs9_child+fs10_child+
#                    fs1_cg+fs2_cg+fs3_cg+fs4_cg+fs5_cg+fs6_cg+fs7_cg+fs8_cg,
#                  data=e_fs,
#                  scale=T)
# summary(e_fs_pca)
# screeplot(e_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Endline")
# 
# # Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# # In this case, the first principal components are significant.
# 
# # Now, extract the principal components and combine with the rest of the FS data.
# e_fs_pc<-e_fs_pca$x[,1:3]
# e_fs_pc<-cbind(e_fs %>% na.omit(),e_fs_pc) 
# 
# # Calculate correlation
# e_fs_cor<-cor(e_fs_pc[,3:20],e_fs_pc[,21:23])
# stargazer(e_fs_cor,
#           header=FALSE, 
#           type='latex',
#           title = "Table 2.7: Endline Food Insecurity PCA Correlation Matrix",
#           column.labels=c("FS1: Child","FS2: Child","FS3: Child", "FS4: Child","FS5: Child","FS6: Child","FS7: Child","FS8: Child","FS9: Child","FS10: Child",
#                           "FS1: Caregiver","FS2: Caregiver","FS3: Caregiver","FS4: Caregiver","FS5: Caregiver","FS6: Caregiver","FS7: Caregiver","FS8: Caregiver"))


############################################################################################################
###################################### Dummy Variable ######################################################
############################################################################################################

###################################### Child ######################################################

### The FIES reduces the dimensionality of the FS questions by creating a dummy variable that =1 if the sum of FS questions >7 (Frongillo Page 2139)
# 
# Midline
m_cfies<-m_ch_fs %>% 
  mutate(fs_sum=rowSums(dplyr::select(.,fs_cols_child))) %>% 
  mutate(m_ch_fs_dummy=if_else(fs_sum>=7,1,0),
         m_ch_fies=as.factor(case_when(fs_sum==0 ~ 0,
                                       fs_sum>=1 & fs_sum<=6~1,
                                       fs_sum>=7 & fs_sum<=10~2,
                                       T~3))
  ) %>% 
  dplyr::select(careid,childid,m_ch_fs_dummy,m_ch_fies)

# Endline
e_cfies<-e_ch_fs %>% 
  mutate(fs_sum=rowSums(dplyr::select(.,fs_cols_child))) %>% 
  mutate(e_ch_fs_dummy=if_else(fs_sum>=7,1,0),
         e_ch_fies=as.factor(case_when(fs_sum==0 ~ 0,
                                       fs_sum>=1 & fs_sum<=6~1,
                                       fs_sum>=7 & fs_sum<=10~2,
                                       T~3))
         ) %>% 
  dplyr::select(careid,childid,e_ch_fs_dummy,e_ch_fies)

###################################### Caregiver ######################################################

### The FIES is coded in a few ways
# 1. Dummy variable if any of Q5-Q8 is 1
# 2. A sum of the scores

# Midline
m_cg_fies<-m_cg_fs %>% 
  mutate(m_cg_fs_dummy=case_when(fs5==1|fs6==1|fs7==1|fs8==1~1,
                                 T~0),
         m_fies_sum=rowSums(dplyr::select(.,fs_cols_cg)),
         m_fies_scale=as.factor(case_when(rowSums(dplyr::select(.,fs_cols_cg)) == 0 ~ 0,
                                          (fs1==1|fs2==1|fs3==1|fs4==1) & (fs5!=1& fs6!=1& fs7!=1& fs8!=1) ~ 1,
                                          (fs5==1|fs6==1|fs7==1) & (fs8!=1) ~ 2,
                                          fs8==1 ~ 3,
                                          T~NA_real_
         )),
         across(matches("fs[0-9]"),~as.double(.))
         
         ) %>%
  dplyr::select(-matches("fs[0-9]"))

# Endline
e_cg_fies<-e_cg_fs %>% 
  mutate(e_cg_fs_dummy=case_when(fs5==1|fs6==1|fs7==1|fs8==1~1,
                                 T~0),
         e_fies_sum=rowSums(dplyr::select(.,fs_cols_cg)),
         e_fies_scale=as.factor(case_when(rowSums(dplyr::select(.,fs_cols_cg)) == 0 ~ 0,
                                          (fs1==1|fs2==1|fs3==1|fs4==1) & (fs5!=1& fs6!=1& fs7!=1& fs8!=1) ~ 1,
                                          (fs5==1|fs6==1|fs7==1) & (fs8!=1) ~ 2,
                                          fs8==1 ~ 3,
                                          T~NA_real_
         )),
         across(matches("fs[0-9]"),~as.double(.))
         
  ) %>% 
  dplyr::select(-matches("fs[0-9]"))

###################################### Putting all treatment data together #############################

fi <- e_cfies %>% 
  inner_join(e_cg_fies,by=c("careid","childid")) %>% 
  left_join(m_cfies,by=c("careid","childid")) %>% 
  left_join(m_cg_fies,by=c("careid","childid"))

###################################################################################################
#################### Caregiver-Reported Parental Engagement data cleaning #########################
###################################################################################################

# Midline
m_cg_pe <- m_cg %>% 
  dplyr::select(careid,childid,starts_with("pe")) %>% 
  # select the variables regarding which caregiver engaged the child and also PE7
  dplyr::select(matches("^[^_]+$"),-pe7,-ends_with("b"),pe10b) %>% 
  # Change the ordinal form of PE8 and PE9
  mutate_at(vars(pe8,pe9),funs(new=case_when(. == 4 ~ 3,
                                             . == 3 ~ 2,
                                             . == 2 ~ 1,
                                             . == 1 ~ 0,
                                             TRUE ~ NA_real_))) %>% 
  # Drop the old pe8 and pe9; replace with the new ones and then reorder
  # dplyr::select(-pe8,-pe9) %>% 
  # rename("pe8"="pe8_new",
  #        "pe9"="pe9_new") %>% 
  dplyr::select(careid,childid,pe1a:pe6a,pe8=pe8_new,pe9=pe9_new,pe10a,pe10b,pe10c,pe10d,pe10e) %>% 
  # Ensure that all columns are numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  mutate(m_hh_engagement=dplyr::select(., contains("pe")) %>% rowSums())

# Endline
e_cg_pe <- e_cg %>% 
  dplyr::select(careid,childid,starts_with("pe")) %>% 
  # select the variables regarding which caregiver engaged the child and also PE7
  dplyr::select(matches("^[^_]+$"),-pe7,-ends_with("b"),pe10b) %>% 
  # Change the ordinal form of PE8 and PE9
  mutate_at(vars(pe8,pe9),funs(new=case_when(. == 4 ~ 3,
                                       . == 3 ~ 2,
                                       . == 2 ~ 1,
                                       . == 1 ~ 0,
                                       TRUE ~ NA_real_))) %>% 
  # Drop the old pe8 and pe9; replace with the new ones and then reorder
  # dplyr::select(-pe8,-pe9) %>% 
  # rename("pe8"="pe8_new",
  #        "pe9"="pe9_new") %>% 
  dplyr::select(careid,childid,pe1a:pe6a,pe8=pe8_new,pe9=pe9_new,pe10a,pe10b,pe10c,pe10d,pe10e) %>% 
  # Ensure that all columns are numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  mutate(e_hh_engagement=dplyr::select(., contains("pe")) %>% rowSums())
# The correlation between parental engagement variables are approximately 0.5 or lower, suggesting a moderate linear relationship.
# Multicollinearity is not a significant issue and the parental engagement variables can be kept in their current form.
# However, I will conduct PCA and linear combinations of PE variables still.

###################################### PCA #############################

# Replace the NAs for children from their siblings and then omit the NAs that don't have any data
cg_pe <- e_cg_pe %>%
  # Group by family
  group_by(careid) %>%
  # Replace NAs with data from their siblings
  mutate_at(vars(pe1a:pe10e), funs(ifelse(is.na(.)==T, first(.)[!is.na(.)], .))) %>% 
  ungroup() %>% 
  na.omit()

# Conduct PCA for PE and evaluate the Screeplot to determine significant features.
cg_pe_pca<-prcomp(~pe1a+pe2a+pe3a+pe4a+pe5a+pe6a+pe8+pe9+pe10a+pe10b+pe10c+pe10d+pe10e,
                  data=cg_pe,
                  scale=T)
summary(cg_pe_pca)
screeplot(cg_pe_pca, type="l", main="Screeplot for Caregiver-Reported Parental Engagement Factors")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, the first FOUR principal components are significant.

# Now, extract the principal components and combine with the rest of the FS data.
cg_pe_pc<-cg_pe_pca$x[,1:4]
cg_pe<-cbind(cg_pe,cg_pe_pc) %>% 
  clean_names() %>% 
  dplyr::select(childid,careid,e_hh_engagement,matches('pc[0-9]')) %>% 
  inner_join(m_cg_pe %>% dplyr::select(-matches('pe[0-9]')),
             by=c('childid','careid'))

# Create a correlation matrix for caregiver engagement PC and original data
# stargazer::stargazer(cor(cg_pe[,3:15],cg_pe[,16:19]))

###################################################################################################
#################### Clean HH Size, CG_Schooling, Motivation, and Self-Esteem #########################
###################################################################################################

# Household Size and Caregiver Schooling
baseline_char <- baseline_enrollment_reg %>% 
  dplyr::select(careid,
         hh_size=ps1,
         cg_schooling=hr10)

# Midline Child Motivation and Esteem
m_ch_motiv_esteem <- m_child %>% 
  mutate(across(contains("mo"),~as.double(.)),
         across(contains("mo"),~case_when(.<0~0,T~.))) %>% 
  mutate(across(matches("se[0-9]"),~as.double(.)),
         across(matches("se[0-9]"),~case_when(.<0~0,T~.))) %>% 
  mutate(m_ch_motiv=dplyr::select(., contains("mo")) %>% rowSums()) %>% 
  mutate(across(c(se2,se5,se8,se9),~case_when(. == 4 ~ 1,
                                              . == 3 ~ 2,
                                              . == 2 ~ 3,
                                              . == 1 ~ 4,
                                              TRUE ~ NA_real_))
  ) %>% 
  mutate(m_ch_esteem=dplyr::select(., matches("se[0-9]")) %>% rowSums()) %>% 
  dplyr::select(childid,careid,m_ch_motiv,m_ch_esteem)

# Endline Child Motivation and Esteem
e_ch_motiv_esteem <- e_child %>% 
  mutate(across(contains("mo"),~as.double(.)),
         across(contains("mo"),~case_when(.<0~0,T~.))) %>% 
  mutate(across(matches("se[0-9]"),~as.double(.)),
         across(matches("se[0-9]"),~case_when(as.double(.)<0~0,T~as.double(.)))) %>% 
  mutate(e_ch_motiv=dplyr::select(., contains("mo")) %>% rowSums()) %>% 
  mutate(across(c(se2,se5,se8,se9),~case_when(. == 4 ~ 1,
                                             . == 3 ~ 2,
                                             . == 2 ~ 3,
                                             . == 1 ~ 4,
                                             TRUE ~ NA_real_))
         ) %>% 
  mutate(e_ch_esteem=dplyr::select(., matches("se[0-9]")) %>% rowSums()) %>% 
  dplyr::select(childid,careid,e_ch_motiv,e_ch_esteem)

################################### Create control data for export ######################
controls<-e_ch_motiv_esteem %>% 
  left_join(m_ch_motiv_esteem,by=c('childid','careid')) %>% 
  mutate(across(c(childid,careid),~as.double(.))) %>% 
  left_join(cg_pe,by=c('childid','careid'))
  
##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

saveRDS(fi, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/fi.rds")
#saveRDS(cg_pe, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/cg_pe.rds")
saveRDS(controls, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/controls.rds")

