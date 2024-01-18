###################################### Food Insecurity and Child Academic Outcomes ############################################

# Author: Allan Lee
# Date: September 16th, 2022
# Purpose: prepare instrumental variable: Drought Index and Agricultural Output

#########################################################################################################

# Set working directory
setwd("/Users/AllanLee/Desktop/Penn/ECON4900/Data")

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
library(sp)
library(sf)
library(readxl)
library(lubridate)
library(weathermetrics)
library(geosphere)
library(raster)
library(data.table)

########################################## Drought and Agricultural Data ###################################################

# Load the shape file of Ghana with administrative subnational boundaries
shapefile <- st_read("gha_admbnda_gss_20210308_SHP/gha_admbnda_adm2_gss_20210308.shp")

# Extract GPS coordinates
districts_df <- data.frame(District = shapefile$ADM2_EN)
districts_df$geometry <- shapefile$geometry
centroids <- st_centroid(districts_df$geometry)
districts_df$Latitude <- st_coordinates(centroids)[, "Y"]
districts_df$Longitude <- st_coordinates(centroids)[, "X"]

# Load drought indices
ssmi <- raster("drought_agriculture_raster/geonode_gha_drought_ssmi_index_present.tif")
spei <- raster("drought_agriculture_raster/geonode_gha_drought_spei_evapotraspiration_index_present.tif")
ssfi <- raster("drought_agriculture_raster/geonode_gha_ssfi_standardized_streamflow_index_present.tif")

# Combine agriculture data with agriculture data
ssmi_extracted_values <- extract(ssmi, shapefile)
ssmi_data_table <- data.table(shapefile, SoilMoisture = unlist(ssmi_extracted_values)) %>% 
  rename_all(., .funs = tolower) %>% 
  rename(ssmi=soilmoisture) %>% 
  dplyr::select(adm2_en,adm2_pcode,adm1_en,adm1_pcode,ssmi) %>% 
  group_by(adm2_pcode,adm2_en,adm1_en,adm1_pcode) %>% 
  summarise(ssmi=mean(ssmi))

spei_extracted_values <- extract(spei, shapefile)
spei_data_table <- data.table(shapefile, SoilMoisture = unlist(spei_extracted_values)) %>% 
  rename_all(., .funs = tolower) %>% 
  rename(spei=soilmoisture) %>% 
  dplyr::select(adm2_en,adm2_pcode,adm1_en,adm1_pcode,spei) %>% 
  group_by(adm2_pcode,adm2_en,adm1_en,adm1_pcode) %>% 
  summarise(spei=mean(spei))

ssfi_extracted_values <- extract(ssfi, shapefile)
ssfi_data_table <- data.table(shapefile, SoilMoisture = unlist(ssfi_extracted_values)) %>% 
  rename_all(., .funs = tolower) %>% 
  rename(ssfi=soilmoisture) %>% 
  dplyr::select(adm2_en,adm2_pcode,adm1_en,adm1_pcode,ssfi) %>% 
  group_by(adm2_pcode,adm2_en,adm1_en,adm1_pcode) %>% 
  summarise(ssfi=mean(ssfi))

# Merge all drought index data
drought_data <- ssmi_data_table %>% 
  inner_join(spei_data_table,by=c("adm2_en","adm2_pcode","adm1_en","adm1_pcode")) %>% 
  inner_join(ssfi_data_table,by=c("adm2_en","adm2_pcode","adm1_en","adm1_pcode")) %>% 
  inner_join(districts_df,by=c("adm2_en"="District")) %>% 
  rename_all(., .funs = tolower) %>% 
  rename(ssmi_index=ssmi,
         spei_index=spei,
         ssfi_index=ssfi)

###################################### Load data/data cleaning #############################

# Load PNP and FAO data. 
m_child <- read_dta("Midline/data/03_PNP_Midline_ChildSurvey.dta")

# Make careid and childid variables numeric
m_child$careid <- as.numeric(m_child$careid)
m_child$childid <- as.numeric(m_child$childid)

###################################### Calculate the weighted average drought probability for each household #############################

# Calculate the distances between each household and weather station
distances <- distm(m_child[, c("gps_vlongitude", "gps_vlatitude")], drought_data[, c("longitude", "latitude")])

# Add col/row names for distances
rownames(distances) <- m_child$childid
colnames(distances) <- drought_data$adm2_pcode

# Compute the weights for each weather station based on the distances
weights <- t(apply(distances, 1, function(x) 1/x))

# Calculate the row sums of the weights matrix
row_sums <- rowSums(weights, na.rm = TRUE)

# Assign the row sums to the "rowSums" attribute of the weights matrix
attr(weights, "rowSums") <- row_sums

# Calculate the weights of each distance
weights <- weights / rowSums(weights, na.rm = TRUE)

# Combine the household and drought weight data
combined_data <- expand.grid(childid = m_child$childid, adm2_pcode = drought_data$adm2_pcode)
combined_data$distance <- distances[as.matrix(combined_data[, c("childid", "adm2_pcode")])]
combined_data$weight <- weights[as.matrix(combined_data[, c("childid", "adm2_pcode")])]

# Merge the drought index data with combined_data
combined_data<-combined_data %>% 
  inner_join(drought_data,by=c("adm2_pcode")) %>% 
  dplyr::select(childid,adm2_pcode,weight,ssmi_index,spei_index,ssfi_index) %>% 
  # Calculate the weighted indice data for each individual district
  mutate(ssmi_w=ssmi_index*weight,
         spei_w=spei_index*weight,
         ssfi_w=ssfi_index*weight) %>% 
  # Combine the indice data together
  group_by(childid) %>% 
  summarise(ssmi_w=sum(ssmi_w,na.rm=T),
            spei_w=sum(spei_w,na.rm=T),
            ssfi_w=sum(ssfi_w,na.rm=T))
  
# Export data
write.csv(combined_data, "/Users/AllanLee/Desktop/Penn/ECON4900/Data/drought_index_iv.csv")

###################################### LATER/MAYBE: Agriculture data #############################
cassava <- raster("drought_agriculture_raster/geonode_gha_cassava_agriculture.tif")
cocoa <- raster("drought_agriculture_raster/geonode_gha_cocoa_agriculture.tif")
groundnut <- raster("drought_agriculture_raster/geonode_gha_groundnut_agriculture.tif")
maize <- raster("drought_agriculture_raster/geonode_gha_maize_agriculture.tif")
plantain <- raster("drought_agriculture_raster/geonode_gha_plantain_agriculture.tif")
taro <- raster("drought_agriculture_raster/geonode_gha_taro_agriculture.tif")
tomato <- raster("drought_agriculture_raster/geonode_gha_tomato_agriculture.tif")
yam <- raster("drought_agriculture_raster/geonode_gha_yam_agriculture.tif")

cassava_extracted_values <- extract(cassava, shapefile)
cassava_data_table <- data.table(shapefile, SoilMoisture = unlist(cassava_extracted_values))

cocoa_extracted_values <- extract(cocoa, shapefile)
cocoa_data_table <- data.table(shapefile, SoilMoisture = unlist(cocoa_extracted_values))

groundnut_extracted_values <- extract(groundnut, shapefile)
groundnut_data_table <- data.table(shapefile, SoilMoisture = unlist(groundnut_extracted_values))

maize_extracted_values <- extract(maize, shapefile)
maize_data_table <- data.table(shapefile, SoilMoisture = unlist(maize_extracted_values))

plantain_extracted_values <- extract(plantain, shapefile)
plantain_data_table <- data.table(shapefile, SoilMoisture = unlist(plantain_extracted_values))

taro_extracted_values <- extract(taro, shapefile)
taro_data_table <- data.table(shapefile, SoilMoisture = unlist(taro_extracted_values))

tomato_extracted_values <- extract(tomato, shapefile)
tomato_data_table <- data.table(shapefile, SoilMoisture = unlist(tomato_extracted_values))

yam_extracted_values <- extract(yam, shapefile)
yam_data_table <- data.table(shapefile, SoilMoisture = unlist(yam_extracted_values))
