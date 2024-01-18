###################################### Food Insecurity and Child Academic Outcomes ############################################

# Author: Allan Lee
# Date: September 16th, 2022
# Purpose: prepare instrumental variable: weather deviance

#########################################################################################################
# Clear R
rm(list=ls())

# Set working directory
setwd("/Users/AllanLee/Desktop/Penn/Personal Projects/Data")

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
library(janitor)

###################################### Load data/data cleaning #############################

# Load PNP and FAO data. 
m_child <- read_dta("import/03_PNP_Midline_ChildSurvey.dta") %>% 
  mutate(across(contains('id'),~as.double(.)))
fao <- read_csv("import/FAOSTAT_data_en_2-18-2023.csv") %>% 
  clean_names()
crop_calendar<-read_excel("import/ghana_crop_calendar.xlsx") %>% 
  dplyr::select(region:growing_end)
region_crop_production<-read_csv("import/1993-2017 region district PRODUCTION ESTIMATES.csv") %>% 
  clean_names() %>% 
  rename(yield=yield_mt_ha)

# Ensure all data are properly formatted to be turned to numeric. Rename district names to be consistent with PNP
f_region_crop_production <- region_crop_production %>% 
  mutate(yield=gsub(",","",yield)) %>%
  mutate(district=if_else(district=="BOLGATANGA MUNICIPAL", "BOLGA",
                          if_else(district=="BUNKPURUGU/YUNYOO","BUNKPURUGU-YUNYOO",
                                  if_else(district=="SAVELUGU/NANTON","SAVELUGU",
                                          if_else(district=="TAMALE METRO", "TAMALE", district))))) %>% 
  mutate(district=if_else(district=="WA EAST", "WA",
                          if_else(district=="WA WEST", "WA",
                                  if_else(district=="WA MUNICIPAL","WA", district)))) %>% 
  mutate(yield=as.double(yield))

# Select relevant data and calculate average yield when there are duplicate crops/district/year
f_region_crop_production_sum <- f_region_crop_production %>% 
  dplyr::select(-area_ha,-production_mt) %>% 
  group_by(region, district, year, crop) %>% 
  summarize(yield=mean(yield, na.rm=T)) %>% 
  ungroup()

##############################################################################################
############################################# IV: Weather ####################################
##############################################################################################

###################################### Create Optimal Weather Value ####################################

# Identify the top three production year for each crop. Filter the data to relevant crops in each region. 
# Then, join with the crop calendar and combine the relevant year to the sowing/growing months.

fao_top_three <- fao %>% 
  group_by(crop=item) %>% 
  top_n(3, value) %>%
  filter(crop=="Wheat and Products"|crop=="Sorghum and products"|crop=="Cassava and products"|
           crop=="Maize and products"|crop=="Rice and products"|crop=="Yams"|crop=="Millet and products") %>% 
  dplyr::select(element, crop, year, unit, value) %>% 
  right_join(crop_calendar,by=c("crop")) %>% 
  mutate(sowing_beg=format(as.Date(sowing_beg), "%m-%d"),
         growing_end=format(as.Date(growing_end), "%m-%d"),
         sowing_beg=paste(year, sowing_beg, sep = "-"),
         growing_end=paste(year, growing_end, sep = "-")) %>% 
  ungroup() %>% 
  # Add relevant station ID data to fao_top_three
  mutate(id=case_when(region == "NORTHERN" ~ "GHM00065418",
                      region == "UPPER EAST" & district == "NAVRONGO" ~ "GHM00065401",
                      region == "UPPER WEST" & district == "WA" ~ "GHM00065404",
                      region == "SAVANNAH" ~ "GHM00065416",
                      TRUE ~ NA_character_),
         sowing_beg=as.Date(sowing_beg,format = "%Y-%m-%d"),
         growing_end=as.Date(growing_end, format = "%Y-%m-%d")
         )

# Identify all Ghanaian GHCN weather stations
gh_ghcnd_stations<-ghcnd_stations(refresh = FALSE) %>% 
  filter(grepl("GHM", id)) %>% 
  dplyr::select(-element,-first_year,-last_year) %>% 
  distinct()

# Extract Ghanaian GHCN data with mean, min, max temperature and precipitation in the relevant time range and region.

# Create new data frame with unique combination of region ID and crop
id_crop <- as.data.frame(fao_top_three %>% distinct(id, crop, sowing_beg,growing_end))

# Gather precipitation and weather data
weather_data <- pmap(id_crop, function(sowing_beg, growing_end, id, crop) {
  meteo_pull_monitors(
    date_min = sowing_beg,
    date_max = growing_end,
    id) %>%
    mutate(id=id, crop = crop)
})

# Combine the various data frames generated from pmap
weather_data_combined <- bind_rows(weather_data)

# Group and summarize the data to calculate the annual average for each region and crop.
weather_yearly_summary <- weather_data_combined %>%
  mutate(year=year(date)) %>% 
  dplyr::select(-tmax,-tmin) %>% 
  group_by(id, crop, year) %>% 
  summarize(avg_precip = mean(prcp, na.rm = TRUE),
            t_avg=mean(tavg,na.rm=T)) %>% 
  ungroup() 

# Then, calculate the average for each crop across all three years.
weather_optimal_summary <- weather_yearly_summary %>% 
  group_by(id,crop) %>% 
  summarize(optimal_precip=mean(avg_precip,na.rm=T),
            optimal_t=mean(t_avg,na.rm=T)) %>% 
  ungroup()

# Combine this data with FAO and location (lat/lon) data
crop_region_optimal_t_prcp <- fao_top_three %>% 
  dplyr::select(-year,-value,-sowing_beg,-growing_end) %>% 
  distinct() %>% 
  inner_join(weather_optimal_summary, by=c('id','crop')) %>% 
  left_join(gh_ghcnd_stations, by = c("id")) %>% 
  dplyr::select(-(elevation:wmo_id))

###################################### Calculate each station's average weather from 2000-2020 ####################################

# Step 1: create general weather variable

# Get relevant date range
earliest<- "2000-01-01"
latest<-"2020-12-31"

# Extract all relevant weather data from stations in relevant date range. Combine with lat/lon data.
all_ghana_weather <- meteo_pull_monitors(
  gh_ghcnd_stations$id,
  date_min=earliest,
  date_max=latest
) %>% 
  inner_join(gh_ghcnd_stations,by=c("id")) %>% 
  dplyr::select(id,date,prcp,tavg,longitude,latitude,elevation)

############################################# Create household weather variable ####################################

# Calculate the annual average for each weather station

# Create relevant m_child data with location/dates
m_child_date_gps <- m_child %>%
  dplyr::select(careid,childid,region,district,community,starts_with("gps")) %>% 
  group_by(careid) %>%
  mutate_at(vars(region:gps_vaccuracy), funs(new=ifelse(is.na(.)|.=="", first(.)[!is.na(.)], .))) %>% 
  ungroup() %>% 
  dplyr::select(-(region:gps_vaccuracy)) %>% 
  filter(!is.na(gps_vlongitude_new),!is.na(gps_vlatitude_new))

# Rename the variables
names(m_child_date_gps)<-sub('_new', '', names(m_child_date_gps))

# Calculate the distances between each household and weather station
distances <- distm(m_child_date_gps[, c("gps_vlongitude", "gps_vlatitude")], gh_ghcnd_stations[, c("longitude", "latitude")])

# Add col/row names for distances
rownames(distances) <- m_child_date_gps$childid
colnames(distances) <- gh_ghcnd_stations$id

# Compute the weights for each weather station based on the distances
weights <- t(apply(distances, 1, function(x) 1/x))

# Calculate the row sums of the weights matrix
row_sums <- rowSums(weights, na.rm = TRUE)

# Assign the row sums to the "rowSums" attribute of the weights matrix
attr(weights, "rowSums") <- row_sums

# Calculate the weights of each distance
weights <- weights / rowSums(weights, na.rm = TRUE)

# Combine the household and weather station data
combined_data <- expand.grid(childid = m_child_date_gps$childid, id = gh_ghcnd_stations$id)
combined_data$distance <- distances[as.matrix(combined_data[, c("childid", "id")])]
combined_data$weight <- weights[as.matrix(combined_data[, c("childid", "id")])]

# Determine the largest crop produced by each region to select each household's relevant farming dates
district_largest_crop <- f_region_crop_production_sum %>% 
  filter(year>=2000) %>% 
  group_by(region,district,crop) %>% 
  summarize(avg_yield=mean(yield,na.rm=T)) %>% 
  arrange(district, desc(avg_yield)) %>% 
  group_by(region,district) %>% 
  top_n(n = 1, wt = avg_yield) %>%
  ungroup()

# Match the largest crop produced with the PNP data. Then, rename the crop names/region to match it with the crop calendar/optimal condition.
# Change the dates so they match 2020 (the year when households actually planted the crop)

m_child_crop_cal <- m_child_date_gps %>% 
  left_join(district_largest_crop,by=c("district")) %>%
  mutate(crop=case_when(crop=="CASSAVA"~"Cassava and products",
                        crop=="RICE"~"Rice and products",
                        crop=="YAM"~"Yams",
                        TRUE ~ NA_character_)) %>% 
  dplyr::select(-region.y) %>% 
  rename(region=region.x) %>% 
  mutate(region=if_else(region=="NORTH EAST","NORTHERN",region)) %>% 
  left_join(crop_calendar,by=c("region","crop")) %>% 
  left_join(crop_region_optimal_t_prcp,by=c("region","crop")) %>% 
  mutate(year=2020,
         sowing_beg=format(as.Date(sowing_beg), "%m-%d"),
         growing_end=format(as.Date(growing_end), "%m-%d"),
         sowing_beg=paste(year, sowing_beg, sep = "-"),
         growing_end=paste(year, growing_end, sep = "-")) %>%
  filter(!is.na(crop)) %>% 
  dplyr::select(-id)

# Combine this data with the household-weights data
combined_data_crop <- combined_data %>% 
  left_join(m_child_crop_cal,by=c('childid')) %>% 
  dplyr::select(careid,childid, id, weight,sowing_beg,growing_end) %>% 
  filter(!is.na(sowing_beg)) %>% 
  mutate(sowing_beg=as.Date(sowing_beg),
         growing_end=as.Date(growing_end))

# Create a sequence of dates for each household
combined_data_ind_day <- combined_data_crop %>% 
  rowwise() %>% 
  mutate(date = list(seq(sowing_beg, growing_end, by = "day"))) %>% 
  ungroup() %>% 
  dplyr::select(-sowing_beg, -growing_end) %>% 
  unnest(date)

# MEAN WEATHER CONDITIONS: Calculate each weather station's average weather condition (2000-2020)
station_average <- all_ghana_weather %>% 
  group_by(id) %>%
  summarise(prcp_mean=mean(prcp,na.rm=T),
            tavg_mean=mean(tavg,na.rm=T))

# SEASONAL WEATHER CONDITIONS: Calculate each weather station's seasonal weather condition based on each region's
seasonal_weather<-crop_calendar %>% 
  group_by(region) %>% 
  summarise(sowing_beg=mean.Date(as.Date(sowing_beg)),
            growing_end=mean.Date(as.Date(growing_end)),
            sowing_beg=format(as.Date(sowing_beg), "%m-%d"),
            growing_end=format(as.Date(growing_end), "%m-%d"),
            sowing_beg=as.Date(paste('2020', sowing_beg, sep = "-")),
            growing_end=as.Date(paste('2020', growing_end, sep = "-"))) %>% 
  mutate(id=case_when(region == "NORTHERN" ~ "GHM00065418",
                      region == "UPPER EAST" ~ "GHM00065401",
                      region == "UPPER WEST" ~ "GHM00065404",
                      T ~ "GHM00065416")) %>% 
  right_join(all_ghana_weather,by=c('id')) %>% 
  filter(date>=sowing_beg,date<=growing_end) %>% 
  group_by(region) %>% 
  summarise(seasonal_prcp=mean(prcp,na.rm=T),
            seasonal_tavg=mean(tavg,na.rm=T))

# Calculate each household's weighted average deviance from both the optimal weather condition and each station's average weather condition
weather_deviance  <- combined_data_ind_day  %>% 
  # Join the household data with weather data
  inner_join(all_ghana_weather,by=c('id',"date")) %>% 
  # Calculate the daily weighted weather.
  mutate(prcp_weighted=weight*prcp,
         tavg_weighted=weight*tavg) %>% 
  # Group the data by careid, childid, weather station id
  group_by(careid,childid,id,weight) %>% 
  # Calculate each household-station's average weather in the year
  summarise(hh_prcp_weighted=mean(prcp_weighted,na.rm=T),
            hh_tavg_weighted=mean(tavg_weighted,na.rm=T)) %>% 
  # Add the optimal weather value to each household
  left_join(m_child_crop_cal,by=c("childid","careid")) %>% 
  mutate(optimal_precip_weighted=weight*optimal_precip,
         optimal_t_weighted=weight*optimal_t) %>% 
  # Add each weather station's average data
  left_join(station_average,by=c("id")) %>% 
  # Calculate the weighted average weather data
  mutate(prcp_mean_weighted=weight*prcp_mean,
         tavg_mean_weighted=weight*tavg_mean) %>% 
  # Calculate the deviance for each station
  mutate(optimal_p_dev=abs(hh_prcp_weighted-optimal_precip_weighted),
         optimal_t_dev=abs(hh_tavg_weighted-optimal_t_weighted),
         mean_p_dev=abs(hh_prcp_weighted-prcp_mean_weighted),
         mean_t_dev=abs(hh_tavg_weighted-tavg_mean_weighted)) %>% 
  # Group by each household and get the total weighted deviance
  group_by(region,careid,childid) %>% 
  summarise(optimal_p_dev=sum(optimal_p_dev),
            optimal_t_dev=sum(optimal_t_dev),
            mean_p_dev=sum(mean_p_dev),
            mean_t_dev=sum(mean_t_dev),
            hh_prcp_weighted=sum(hh_prcp_weighted),
            hh_tavg_weighted=sum(hh_tavg_weighted)) %>% 
  # Join the seasonal weather
  left_join(seasonal_weather,by=c('region')) %>% 
  mutate(seasonal_p_dev=abs(hh_tavg_weighted-seasonal_prcp),
         seasonal_t_dev=abs(hh_tavg_weighted-seasonal_tavg))

##########################################################################################
################################## Exporting Relevant Data ###############################
##########################################################################################

# Construct extreme weather deviation indicator

iv <- weather_deviance %>% 
  ungroup() %>% 
  mutate(extreme_o_p_dev=case_when(abs(optimal_p_dev-mean(optimal_p_dev,na.rm=T))>sd(optimal_p_dev,na.rm=T)~1,
                                   T~0),
         extreme_o_t_dev=ifelse(abs(optimal_t_dev-mean(optimal_t_dev,na.rm=T))>sd(optimal_t_dev,na.rm=T),1,0),
         extreme_m_p_dev=ifelse(abs(mean_p_dev-mean(mean_p_dev,na.rm=T))>sd(mean_p_dev,na.rm=T),1,0),
         extreme_m_t_dev=ifelse(abs(mean_t_dev-mean(mean_t_dev,na.rm=T))>sd(mean_t_dev,na.rm=T),1,0),
         extreme_s_p_dev=ifelse(abs(seasonal_p_dev-mean(seasonal_p_dev,na.rm=T))>sd(seasonal_p_dev,na.rm=T),1,0),
         extreme_s_t_dev=ifelse(abs(seasonal_t_dev-mean(seasonal_t_dev,na.rm=T))>sd(seasonal_t_dev,na.rm=T),1,0))

# Export data
saveRDS(iv, "/Users/AllanLee/Desktop/Personal Projects/ECON4900/Data/build/iv.rds")
