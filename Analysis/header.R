# Set WD
setwd("/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data")

# Load packages
library(fixest)
library(stargazer)
library(AER)
library(dataCompareR)
library(broom)
library(xtable)
library(lfe)
library(purrr)
library(modelsummary)
library(writexl)
library(ltm)
library(glue)
library(kableExtra)
library(readr)
library(tidyverse)
library(dplyr)
library(officer)

# Define functions
reg_func <- function(category, model){
  e_category_str<-paste0("e_",category,"_per")
  m_category_str<-paste0("m_",category,"_per")
  
  for_reg<-full_data_w %>% 
    rename(lagged_outcome=m_category_str)
  
  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))  
  reg <- feols(fm,
            data=for_reg,
            cluster=~careid)
  return(reg)
}

# Define function for standard errors
cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)
  
  out<-list(se=reg_robust[,2],
       p=reg_robust[,4])
  
  return(out)
}

# Define function for creating tidy results
tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>% 
    mutate(category=category)
  return(out)
}

# Model Summary formatting
f <- function(x) formatC(x, digits = 3, big.mark = ",", format = "f")

format_nobs <- function(x) {
  if (is.numeric(x)) {
    return(format(x, big.mark = ",", scientific = F, digits=3))
  }

  return(x)
}

format_stat <- function(x) {
  if (is.numeric(x)) {
    return(format(x, big.mark = ",", scientific = F, digits=3))
  }

  return(x)
}


gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = format_nobs),
  list("raw" = "adj.r.squared", "clean" = "Adj. R2", "fmt" = format_stat),
  list("raw" = "rmse", "clean" = "RMSE", "fmt" = format_stat))
