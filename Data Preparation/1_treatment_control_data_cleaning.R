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
library(foreign)
library(haven)
library(tidyverse)
library(stargazer)
library(psych)
library(corrr)
library(tibble)
library(writexl)
library(timechange)
library(rnoaa)
library(base)
library(arsenal)
library(labelled)
library(zoo)

##########################################################################################
###################################### FS data cleaning ##################################
##########################################################################################
### Change the ordinal form of the FS data and replace NAs with row means

# Load relevant data
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta")
m_cg <- read_dta("import/02_PNP_Midline_CaregiverSurvey.dta")
e_child <- read_dta("import/03_PNP_Endline_ChildSurvey.dta")
e_cg <- read_dta("import/02_PNP_Endline_CaregiverSurvey.dta")

# Denote the relevant fs variables
fs_cols_child<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8","fs9","fs10")
fs_cols_cg<-c("fs1","fs2","fs3","fs4","fs5","fs6","fs7","fs8")

# Change the categorical response of FS for each set of data such that higher numbers means more food insecurity

m_child <- m_child %>% 
  mutate_at(fs_cols_child,as.numeric) %>% 
  mutate_at(fs_cols_child,funs(new=case_when(. == 1 ~ 2,
                                       . == 2 ~ 1,
                                       . == 3 ~ 0,
                                       TRUE ~ NA_real_))) %>% 
  dplyr::select(-all_of(fs_cols_child))

m_cg <- m_cg %>% 
  mutate_at(fs_cols_cg,as.numeric)

e_child <- e_child %>% 
  mutate_at(fs_cols_child,as.numeric) %>% 
  mutate_at(fs_cols_child,funs(new=case_when(. == 1 ~ 2,
                                       . == 2 ~ 1,
                                       . == 3 ~ 0,
                                       TRUE ~ NA_real_))) %>% 
  dplyr::select(-all_of(fs_cols_child))

e_cg <- e_cg %>% 
  mutate_at(fs_cols_cg,as.numeric)

# Change column names back to the original
names(m_child) = gsub(pattern = "_new", replacement = "", x = names(m_child))
names(e_child) = gsub(pattern = "_new", replacement = "", x = names(e_child))

# dplyr::select relevant variables in the food security data and convert them to numeric.

m_ch_fs <- m_child %>% 
  dplyr::select(careid,childid,starts_with("fs"))
m_cg_fs <- m_cg %>% 
  dplyr::select(careid,starts_with("fs")) %>% 
  distinct(.keep_all = T)
e_ch_fs <- e_child %>% 
  dplyr::select(caseid,childid,starts_with("fs")) %>% 
  rename(careid=caseid) %>% 
  dplyr::select(-fs_id)
e_cg_fs <- e_cg %>% 
  dplyr::select(careid,starts_with("fs")) %>% 
  distinct(.keep_all = T) %>% 
  dplyr::select(-fsid)

# I will replace the NAs with an average of all other FS values.
m_ch_fs[fs_cols_child] <- apply(m_ch_fs[fs_cols_child], 2, function(x) ifelse(is.na(x), rowMeans(m_ch_fs[fs_cols_child], na.rm = TRUE), x))
m_cg_fs[fs_cols_cg] <- apply(m_cg_fs[fs_cols_cg], 2, function(x) ifelse(is.na(x), rowMeans(m_cg_fs[fs_cols_cg], na.rm = TRUE), x))
e_ch_fs[fs_cols_child] <- apply(e_ch_fs[fs_cols_child], 2, function(x) ifelse(is.na(x), rowMeans(e_ch_fs[fs_cols_child], na.rm = TRUE), x))
e_cg_fs[fs_cols_cg] <- apply(e_cg_fs[fs_cols_cg], 2, function(x) ifelse(is.na(x), rowMeans(e_cg_fs[fs_cols_cg], na.rm = TRUE), x))

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
m_fs <- m_ch_fs %>% 
  left_join(m_cg_fs,by=c("careid")) %>%
  distinct(.keep_all = TRUE) %>% 
  rename(fs9_child=fs9,
         fs10_child=fs10)
e_fs <- e_ch_fs %>% 
  left_join(e_cg_fs,by=c("careid")) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(fs9_child=fs9,
         fs10_child=fs10)

# Rename FS variables to note child and parent
names(m_fs) = gsub(pattern = ".x", replacement = "_child", x = names(m_fs))
names(m_fs) = gsub(pattern = ".y", replacement = "_cg", x = names(m_fs))
names(e_fs) = gsub(pattern = ".x", replacement = "_child", x = names(e_fs))
names(e_fs) = gsub(pattern = ".y", replacement = "_cg", x = names(e_fs))

##########################################################################################
############################################## PCA #######################################
##########################################################################################

############################################## Child: Midline #######################################

# Conduct PCA for food insecurity and evaluate the Screeplot to determine significant features.
m_ch_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8+fs9+fs10,
                  data=m_ch_fs,
                  scale=T)
summary(m_ch_fs_pca)
screeplot(m_ch_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Child Midline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, only the first principal component is significant.

# Now, extract the principal components and combine with the rest of the FS data.
m_ch_fs_pc<-m_ch_fs_pca$x[,1]
m_ch_fs_pc<-cbind(m_ch_fs,m_ch_fs_pc) 

############################################## Child: Endline #######################################

# Conduct PCA for food insecurity and evaluate the Screeplot to determine significant features.
e_ch_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8+fs9+fs10,
                    data=e_ch_fs,
                    scale=T)
summary(e_ch_fs_pca)
screeplot(e_ch_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Child Endline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, only the first principal component is significant.

# Now, extract the principal components and combine with the rest of the FS data.
e_ch_fs_pc<-e_ch_fs_pca$x[,1]
e_ch_fs_pc<-cbind(e_ch_fs,e_ch_fs_pc) 

############################################## Caregiver: Midline #######################################

m_cg_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8,
                    data=m_cg_fs,
                    scale=T)
summary(m_cg_fs_pca)
screeplot(m_cg_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Caregiver Midline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, only the first principal component is significant.

# Now, extract the principal components and combine with the rest of the FS data.
m_cg_fs_pc<-m_cg_fs_pca$x[,1]
m_cg_fs_pc<-cbind(m_cg_fs,m_cg_fs_pc) 

############################################## Caregiver: Endline #######################################

e_cg_fs_pca<-prcomp(~fs1+fs2+fs3+fs4+fs5+fs6+fs7+fs8,
                    data=e_cg_fs,
                    scale=T)
summary(e_cg_fs_pca)
screeplot(e_cg_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Caregiver Endline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, only the first principal component is significant.

# Now, extract the principal components and combine with the rest of the FS data.
e_cg_fs_pc<-e_cg_fs_pca$x[,1]
e_cg_fs_pc<-cbind(e_cg_fs,e_cg_fs_pc) 

############################################## All: Midline #######################################

m_fs_pca<-prcomp(~fs1_child+fs2_child+fs3_child+fs4_child+fs5_child+fs6_child+fs7_child+fs8_child+fs9_child+fs10_child+
                 fs1_cg+fs2_cg+fs3_cg+fs4_cg+fs5_cg+fs6_cg+fs7_cg+fs8_cg,
                    data=m_fs,
                    scale=T)
summary(m_fs_pca)
screeplot(m_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Midline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, the first principal components are significant.

# Now, extract the principal components and combine with the rest of the FS data.
m_fs_pc<-m_fs_pca$x[,1:3]
m_fs_pc<-cbind(m_fs %>% na.omit(),m_fs_pc) 

# Calculate correlation
m_fs_cor<-cor(m_fs_pc[,3:20],m_fs_pc[,21:23])
stargazer(m_fs_cor,
          header=FALSE, 
          type='latex',
          title = "Table 2.6: Midline Food Insecurity PCA Correlation Matrix",
          column.labels=c("FS1: Child","FS2: Child","FS3: Child", "FS4: Child","FS5: Child","FS6: Child","FS7: Child","FS8: Child","FS9: Child","FS10: Child",
                             "FS1: Caregiver","FS2: Caregiver","FS3: Caregiver","FS4: Caregiver","FS5: Caregiver","FS6: Caregiver","FS7: Caregiver","FS8: Caregiver"))

############################################## All: Endline #######################################

e_fs_pca<-prcomp(~fs1_child+fs2_child+fs3_child+fs4_child+fs5_child+fs6_child+fs7_child+fs8_child+fs9_child+fs10_child+
                   fs1_cg+fs2_cg+fs3_cg+fs4_cg+fs5_cg+fs6_cg+fs7_cg+fs8_cg,
                 data=e_fs,
                 scale=T)
summary(e_fs_pca)
screeplot(e_fs_pca, type="l", main="Screeplot for Food Insecurity Factors: Endline")

# Based on "The Elbow Rule" of PCA, the number of principal components that should be dplyr::selected should be the PCs before a steep drop off.
# In this case, the first principal components are significant.

# Now, extract the principal components and combine with the rest of the FS data.
e_fs_pc<-e_fs_pca$x[,1:3]
e_fs_pc<-cbind(e_fs %>% na.omit(),e_fs_pc) 

# Calculate correlation
e_fs_cor<-cor(e_fs_pc[,3:20],e_fs_pc[,21:23])
stargazer(e_fs_cor,
          header=FALSE, 
          type='latex',
          title = "Table 2.7: Endline Food Insecurity PCA Correlation Matrix",
          column.labels=c("FS1: Child","FS2: Child","FS3: Child", "FS4: Child","FS5: Child","FS6: Child","FS7: Child","FS8: Child","FS9: Child","FS10: Child",
                          "FS1: Caregiver","FS2: Caregiver","FS3: Caregiver","FS4: Caregiver","FS5: Caregiver","FS6: Caregiver","FS7: Caregiver","FS8: Caregiver"))


############################################################################################################
###################################### Dummy Variable ######################################################
############################################################################################################

###################################### Child ######################################################

### The FIES reduces the dimensionality of the FS questions by creating a dummy variable that =1 if the sum of FS questions >7 (Frongillo Page 2139)

# Midline
m_ch_fs_pc$m_ch_fies=rowSums(m_ch_fs_pc[,3:12])
m_ch_fs_pc<-m_ch_fs_pc %>%
  mutate(m_ch_fs_dummy=if_else(m_ch_fies>=7,1,0)) %>% 
  dplyr::select(careid,childid,m_ch_fs_pc,m_ch_fies,m_ch_fs_dummy)

# Endline
e_ch_fs_pc$e_ch_fies=rowSums(e_ch_fs_pc[,3:12])
e_ch_fs_pc<-e_ch_fs_pc %>%
  mutate(e_ch_fs_dummy=if_else(e_ch_fies>=7,1,0)) %>% 
  dplyr::select(careid,childid,e_ch_fs_pc,e_ch_fies,e_ch_fs_dummy)

###################################### Caregiver ######################################################

### The FIES reduces the dimensionality of the FS questions by creating a dummy variable that =1 if any of FSQ5-Q8 =1

m_cg_fs_pc$m_cg_fies=rowSums(m_cg_fs_pc[,6:9])
m_cg_fs_pc<-m_cg_fs_pc %>% 
  mutate(m_cg_fs_dummy=ifelse(m_cg_fies>=1,1,0)) %>% 
  dplyr::select(careid,m_cg_fs_pc,m_cg_fies,m_cg_fs_dummy)

e_cg_fs_pc$e_cg_fies=rowSums(e_cg_fs_pc[,6:9])
e_cg_fs_pc<-e_cg_fs_pc %>% 
  mutate(e_cg_fs_dummy=ifelse(e_cg_fies>=1,1,0)) %>% 
  dplyr::select(careid,e_cg_fs_pc,e_cg_fies,e_cg_fs_dummy)

m_fs_pc$m_fs_fies=rowSums(m_fs_pc[,3:20])
m_fs_pc<-m_fs_pc %>% 
  dplyr::select(careid,childid,PC1,PC2,PC3,m_fs_fies) %>% 
  rename(m_fs_pc1=PC1,
         m_fs_pc2=PC2,
         m_fs_pc3=PC3)

e_fs_pc$e_fs_fies=rowSums(e_fs_pc[,3:20])
e_fs_pc<-e_fs_pc %>% 
  dplyr::select(careid,childid,PC1,PC2,PC3,e_fs_fies) %>% 
  rename(e_fs_pc1=PC1,
         e_fs_pc2=PC2,
         e_fs_pc3=PC3)

###################################### Putting all treatment data together #############################

treatment <- m_ch_fs_pc %>% 
  inner_join(m_cg_fs_pc,by=c("careid")) %>% 
  inner_join(e_ch_fs_pc,by=c("careid","childid")) %>% 
  inner_join(e_cg_fs_pc,by=c("careid")) %>% 
  inner_join(m_fs_pc,by=c("careid","childid")) %>%
  inner_join(e_fs_pc,by=c("careid","childid"))

###################################################################################################
#################### Caregiver-Reported Parental Engagement data cleaning #########################
###################################################################################################

# # Load relevant data.
# e_cg <- read_dta("04 Endline Survey/02_PNP_Endline_CaregiverSurvey.dta")
# dplyr::select the careid, childid, and PE variables
cg_pe <- e_cg %>% 
  dplyr::select(careid,childid,starts_with("pe")) %>% 
  # Dedplyr::select the variables regarding which caregiver engaged the child and also PE7
  dplyr::select(matches("^[^_]+$"),-pe7,-ends_with("b"),pe10b) %>% 
  # Change the ordinal form of PE8 and PE9
  mutate_at(vars(pe8,pe9),funs(new=case_when(. == 4 ~ 3,
                                       . == 3 ~ 2,
                                       . == 2 ~ 1,
                                       . == 1 ~ 0,
                                       TRUE ~ NA_real_))) %>% 
  # Drop the old pe8 and pe9; replace with the new ones and then reorder
  dplyr::select(-pe8,-pe9) %>% 
  rename("pe8"="pe8_new",
         "pe9"="pe9_new") %>% 
  dplyr::select(careid,childid,pe1a:pe6a,pe8,pe9,pe10a,pe10b,pe10c,pe10d,pe10e) %>% 
  # Ensure that all columns are numeric
  mutate_if(is.double,as.numeric) %>% 
  mutate_if(is.character,as.numeric) %>%
  # Turn into data frame
  as.data.frame()
# The correlation between parental engagement variables are approximately 0.5 or lower, suggesting a moderate linear relationship.
# Multicollinearity is not a significant issue and the parental engagement variables can be kept in their current form.
# However, I will conduct PCA and linear combinations of PE variables still.

###################################### PCA #############################

# Replace the NAs for children from their siblings and then omit the NAs that don't have any data
cg_pe <- cg_pe %>%
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
  clean_names()

# Create a correlation matrix for caregiver engagement PC and original data
stargazer(cor(cg_pe[,3:15],cg_pe[,16:19]))

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

saveRDS(treatment, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/treatment.rds")
saveRDS(cg_pe, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/cg_pe.rds")


