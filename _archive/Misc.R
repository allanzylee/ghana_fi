
##### OLD: ISD Weather Station Data is not used because of significant missing data #####

# Identify all Ghanaian ISD weather stations
gh_isd_stations<-isd_stations_search(lat = 8.17, lon = -1.3, radius = 400) %>% 
  filter(ctry=="GH") %>% 
  # Create region ID
  mutate(region_id=paste0(usaf,wban)) %>% 
  filter(region_id!=65422099999,region_id!=65464099999)

# list of station IDs
gh_isd_station_id <- gh_isd_stations$region_id

# empty list to store the data from each station
data_list <- list()

# loop through each station and extract the data using the lcd function
for (i in 1:length(gh_isd_station_id)) {
  data <- lcd(gh_isd_station_id[i],year=2021)
  data_list[[i]] <- data
}

# combine all the data into one dataframe
gh_isd_weather <- do.call(rbind, data_list)

test<-lcd(65481099999,year=2021)

# Extract the precipitation data for each date
precipitation_list <- list()
for (i in 1:length(date_list)) {
  date <- date_list[i]
  precipitation <- cpc_prcp(date = date,us = FALSE, drop_undefined = TRUE)
  precipitation_list[[i]] <- precipitation
}

# combine all the precipitation data into one dataframe
final_precipitation_data <- do.call(rbind, precipitation_list)




###################################### Family Size, Child Order,and Sibling Analysis #############################

# Create a column that indicates how many children is in each family
fam_pnp_endline <- pnp_endline_raw %>% 
  mutate(num_kids=rowSums(!is.na(pnp_endline_raw %>% dplyr::select(contains('cr2_'))))) %>% 
# Create an indicator checking if each person has an older sister. 
# Calculate the max age of the female child in each family and compare each child to them.
  mutate(cr9_oldersister_1=ifelse(cr6_1<pmax(cr6_2*(cr3_2-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_2=ifelse(cr6_2<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_3=ifelse(cr6_3<pmax(cr6_1*(cr3_1-1),cr6_2*(cr3_2-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_4=ifelse(cr6_4<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_2*(cr3_2-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_5=ifelse(cr6_5<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_2*(cr3_2-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_6=ifelse(cr6_6<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_2*(cr3_2-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_7=ifelse(cr6_7<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_2*(cr3_2-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_8=ifelse(cr6_8<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_2*(cr3_2-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_9=ifelse(cr6_9<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_2*(cr3_2-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_10=ifelse(cr6_10<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_2*(cr3_2-1),cr6_11*(cr3_11-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_11=ifelse(cr6_11<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_2*(cr3_2-1),
                                         cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr9_oldersister_12=ifelse(cr6_12<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                         cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                         cr6_2*(cr3_2-1),na.rm=T),1,0)) %>% 
# Create older brother variable. Firstly create a new temporary gender variable
  mutate(across(starts_with('cr3_'),~ifelse(.x==2,1,ifelse(.x==1,2,NA)))) %>%
# Create older brother variable
  mutate(cr12_olderbrother_1=ifelse(cr6_1<pmax(cr6_2*(cr3_2-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_2=ifelse(cr6_2<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_3=ifelse(cr6_3<pmax(cr6_1*(cr3_1-1),cr6_2*(cr3_2-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_4=ifelse(cr6_4<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_2*(cr3_2-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_5=ifelse(cr6_5<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_2*(cr3_2-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_6=ifelse(cr6_6<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_2*(cr3_2-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_7=ifelse(cr6_7<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_2*(cr3_2-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_8=ifelse(cr6_8<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_2*(cr3_2-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_9=ifelse(cr6_9<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                             cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_2*(cr3_2-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                             cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_10=ifelse(cr6_10<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                               cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_2*(cr3_2-1),cr6_11*(cr3_11-1),
                                               cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_11=ifelse(cr6_11<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                               cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_2*(cr3_2-1),
                                               cr6_12*(cr3_12-1),na.rm=T),1,0),
         cr12_olderbrother_12=ifelse(cr6_12<pmax(cr6_1*(cr3_1-1),cr6_3*(cr3_3-1),cr6_4*(cr3_4-1),cr6_5*(cr3_5-1),cr6_6*(cr3_6-1),
                                               cr6_7*(cr3_7-1),cr6_8*(cr3_8-1),cr6_9*(cr3_9-1),cr6_10*(cr3_10-1),cr6_11*(cr3_11-1),
                                               cr6_2*(cr3_2-1),na.rm=T),1,0)) %>% 
# Convert NAs to 0s in the older sister columns
  mutate(across(starts_with("cr9_oldersister"), ~ifelse(is.na(.x),0,.x))) %>% 
# Convert NAs to 0s in the older sister columns
  mutate(across(starts_with("cr12_olderbrother"), ~ifelse(is.na(.x),0,.x))) %>% 
# Determine if each child is the oldest child 
  mutate(across(starts_with("cr6_"),~ifelse(.x==pmax(cr6_1,cr6_2,cr6_3,cr6_4,cr6_5,cr6_6,cr6_7,cr6_8,cr6_9,cr6_10,cr6_11,cr6_12,na.rm=T),1,0),.names="cr10_oldest_{col}")) %>% 
# Determine if each child is the youngest child 
  mutate(across(starts_with("cr6_"),~ifelse(.x==pmin(cr6_1,cr6_2,cr6_3,cr6_4,cr6_5,cr6_6,cr6_7,cr6_8,cr6_9,cr6_10,cr6_11,cr6_12,na.rm=T),1,0),.names="cr11_youngest_{col}")) %>% 
# Convert NAs to 0s in the oldest and youngest columns
  mutate(across(starts_with("cr10_oldest"), ~ifelse(is.na(.x),0,.x))) %>% 
  mutate(across(starts_with("cr11_youngest"), ~ifelse(is.na(.x),0,.x))) %>% 
  mutate(c_ind_1=1) %>% 
# Group data together based on the child
  dplyr::select(careid,num_kids,c_ind_1,ends_with("_1"),ends_with("_2"),ends_with("_3"),ends_with("_4"),ends_with("_5"),ends_with("_6"),
         ends_with("_7"),ends_with("_8"),ends_with("_9"),ends_with("_10"),ends_with("_11"),ends_with("_12"))

###################################### Convert analysis from per child to per family #############################

pc_pnp_endline <- fam_pnp_endline %>% 
  # Convert data to data frame
  as.data.frame() %>% 
  # Pivot data to per child basis
  reshape(direction = 'long', 
          idvar = 'careid',
          varying = list(seq(3,157,14),seq(4,158,14),seq(5,159,14),seq(6,160,14),seq(7,161,14),seq(8,162,14),seq(9,163,14),seq(10,164,14),seq(11,165,14),seq(12,166,14),seq(13,167,14),seq(14,168,14),seq(15,169,14),seq(16,170,14)),
          v.names = c("c_ind","cr0", "cr2", "cr3", "cr4","cr5_1","cr5_2","cr6_1","cr7_1","cr8_1","cr9_oldersister","cr12_olderbrother","cr10_oldest","cr11_youngest")) %>% 
  select(-time) %>% 
  # Eliminate empty rows
  filter(cr0!="")

# Turn everything in the data frame to numeric
pc_pnp_endline <- mutate_all(pc_pnp_endline, function(x) as.numeric(as.character(x)))

###################################### Load per child academic data #############################

pc_outcome_raw <- read_dta("RIPPLE/Child Order Paper (1)/02_data/Child Order Dataset_8.12.22.dta")

# Filter for relevant variables
pc_outcome <- pc_outcome_raw %>% 
  # Join with per child individual data 
  left_join(pc_pnp_endline,by=c('childid'='cr0','careid','num_kids')) %>% 
  # Convert per child individual gender data to same format as outcome data
  mutate(cr3=case_when(cr3==2 ~ 0,
                       cr3==1 ~ 1,
                       TRUE ~ NA_real_)) %>% 
  # Combine gender values to reduce NAs
  mutate(male=case_when(is.na(cr3) & !is.na(male) ~ male,
                        !is.na(cr3) & is.na(male) ~ cr3,
                        !is.na(cr3) & !is.na(male) ~ cr3,
                        TRUE ~ NA_real_)) %>% 
  # Select relevant variables
  select(careid,childid,cg_age,male,cg_male,num_kids,ch_order,ch_age,cr6_1,age_group,end_cg_home_eng, Has_older_sister,Has_older_brother, end_SEL_59,end_SEL_1117,end_Lit_59,
         end_Lit_1017,end_ch_num59,end_ch_num1017,end_ch_ef59,end_ch_ef1017,end_ch_eng1017,end_ch_eng59,end_sdq_total_diff_per,starts_with("end_"),mid_ppi_score,end_hh_fies) %>% 
  # Filter out rows where the child has none of the outcome data
  filter((!is.na(end_SEL_1117) | !is.na(end_Lit_1017) | !is.na(end_ch_num1017) | !is.na(end_ch_ef1017 | 
         !is.na(end_SEL_59) | !is.na(end_Lit_59) | !is.na(end_ch_num59) | !is.na(end_ch_ef59)))) %>% 
  # Address the rows where the child is the oldest and have an older sister. Also address missing age.
  mutate(ch_order=as.numeric(ch_order),
         ch_order=case_when(childid==1122110901 ~ 0,
                            childid==2030410201 ~ 0,
                            childid==2040710505 ~ 0,
                            childid==1111461504 ~ 0,
                            TRUE ~ ch_order),
         cr6_1=case_when(childid==1060901701 ~ 6,
                         TRUE ~ cr6_1),
         age_group=case_when(cr6_1>=5 & cr6_1<=9 ~ 0,
                             cr6_1>=10 & cr6_1<=17 ~ 1,
                             TRUE ~ NA_real_))

############################ OLD Calculate descriptive data about family/children and engagement ####################################

# Calculate the mean age of the sample and household size
mean(pc_outcome$cr6_1,na.rm=T)
mean(pc_outcome$num_kids,na.rm=T)

# Calculate the percentage of male vs female
table(pc_outcome$male)/nrow(pc_outcome)

# Calculate the number and percentage of the different possible options in child order
ch_order_num<-table(pc_outcome$ch_order) %>% 
  as.data.frame()

ch_order_perc<-table(pc_outcome$ch_order)/nrow(pc_outcome)

# Calculate the number and percentage of those who do and do not have an older sister
table(pc_outcome$Has_older_sister)
table(pc_outcome$Has_older_sister)/nrow(pc_outcome)

# Calculate the mean of home engagement overall
mean(pc_outcome$end_cg_home_eng,na.rm=T)

# Mean of CG home engagement by child order
ch_order_home_eng <- pc_outcome %>% 
  group_by(ch_order) %>% 
  summarise(avg=mean(end_cg_home_eng,na.rm=T))

# Mean of CG home engagement by older sister
older_sister_home_eng <- pc_outcome %>% 
  group_by(Has_older_sister) %>% 
  summarise(avg=mean(end_cg_home_eng,na.rm=T))

# Mean of CG home engagement by older sister and child order
ch_order_older_sister_home_eng <- pc_outcome %>% 
  group_by(Has_older_sister,ch_order) %>% 
  summarise(avg=mean(end_cg_home_eng,na.rm=T))

# Mean of academic outcome by child order
academic <- pc_outcome %>% 
  group_by() %>% 
  summarise(sel59=mean(end_SEL_59,na.rm=T),
            sel1017=mean(end_SEL_1117,na.rm=T),
            lit59=mean(end_Lit_59,na.rm=T),
            lit1017=mean(end_Lit_1017,na.rm=T),
            num59=mean(end_ch_num59,na.rm=T),
            num1017=mean(end_ch_num1017,na.rm=T),
            ef59=mean(end_ch_ef59,na.rm=T),
            ef1017=mean(end_ch_ef1017,na.rm=T))

# Mean of academic outcome by child order and age group
academic_ch_order <- pc_outcome %>% 
  group_by(ch_order) %>% 
  summarise(sel59=mean(end_SEL_59,na.rm=T),
            sel1017=mean(end_SEL_1117,na.rm=T),
            lit59=mean(end_Lit_59,na.rm=T),
            lit1017=mean(end_Lit_1017,na.rm=T),
            num59=mean(end_ch_num59,na.rm=T),
            num1017=mean(end_ch_num1017,na.rm=T),
            ef59=mean(end_ch_ef59,na.rm=T),
            ef1017=mean(end_ch_ef1017,na.rm=T))

# Mean of academic outcome by age group and older sister
academic_sister <- pc_outcome %>% 
  group_by(Has_older_sister) %>% 
  summarise(sel59=mean(end_SEL_59,na.rm=T),
            sel1017=mean(end_SEL_1117,na.rm=T),
            lit59=mean(end_Lit_59,na.rm=T),
            lit1017=mean(end_Lit_1017,na.rm=T),
            num59=mean(end_ch_num59,na.rm=T),
            num1017=mean(end_ch_num1017,na.rm=T),
            ef59=mean(end_ch_ef59,na.rm=T),
            ef1017=mean(end_ch_ef1017,na.rm=T))

# Mean of academic outcome by age group, child order, and older sister
academic_sister_ch_order <- pc_outcome %>% 
  group_by(Has_older_sister,ch_order) %>% 
  summarise(sel59=mean(end_SEL_59,na.rm=T),
            sel1017=mean(end_SEL_1117,na.rm=T),
            lit59=mean(end_Lit_59,na.rm=T),
            lit1017=mean(end_Lit_1017,na.rm=T),
            num59=mean(end_ch_num59,na.rm=T),
            num1017=mean(end_ch_num1017,na.rm=T),
            ef59=mean(end_ch_ef59,na.rm=T),
            ef1017=mean(end_ch_ef1017,na.rm=T))

# Mean of CH home engagement for both age groups
mean(pc_outcome$end_ch_eng59)
mean(pc_outcome$end_ch_eng1017)

# Mean of CH home engagement by child order for both age groups
ch_order_ch_home_eng_59 <- pc_outcome %>% 
  group_by(ch_order) %>% 
  summarise(avg=mean(end_ch_eng59,na.rm=T))

ch_order_ch_home_eng_1017 <- pc_outcome %>% 
  group_by(ch_order) %>% 
  summarise(avg=mean(end_ch_eng1017,na.rm=T))

# Mean of CH home engagement by older sister for both age groups
older_sister_ch_home_eng_59 <- pc_outcome %>% 
  group_by(Has_older_sister) %>% 
  summarise(avg=mean(end_ch_eng59,na.rm=T))

older_sister_ch_home_eng_1017 <- pc_outcome %>% 
  group_by(Has_older_sister) %>% 
  summarise(avg=mean(end_ch_eng1017,na.rm=T))

# Mean of CH home engagement by older sister and child order for both age groups
ch_order_older_sister_ch_home_eng_59 <- pc_outcome %>% 
  group_by(Has_older_sister,ch_order) %>% 
  summarise(avg=mean(end_ch_eng59,na.rm=T))

ch_order_older_sister_ch_home_eng_1017 <- pc_outcome %>% 
  group_by(Has_older_sister,ch_order) %>% 
  summarise(avg=mean(end_ch_eng1017,na.rm=T))

############################ NEW Calculate descriptive data about family/children and academic outcome based on age group, gender, and older sister ####################################

# Calculate the age of caregivers/percentage of male vs female
mean(pc_outcome$cg_age,na.rm=T)
table(pc_outcome$cg_male)
table(pc_outcome$cg_male)/nrow(pc_outcome)

# Calculate child mean age
mean(pc_outcome$cr6_1,na.rm=T)

# Calculate the percentage of male vs female for children
table(pc_outcome$male)/nrow(pc_outcome)

# Mean household size
mean(pc_outcome$num_kids,na.rm=T)

# Table of child order: number and percentage
table(pc_outcome$ch_order)/nrow(pc_outcome)

# Table of older sister/brother: number and percentage
table(pc_outcome$Has_older_brother)/nrow(pc_outcome)

# Academic table by age group, gender, and older sister

academic_age_group<-pc_outcome %>% 
  group_by(age_group) %>% 
  summarise(end_sdq_total_diff_per = mean(end_sdq_total_diff_per ,na.rm=T),
            end_SEL_per = mean(end_SEL_per,na.rm=T),
            end_per_lit = mean(end_per_lit,na.rm=T),
            end_per_num = mean(end_per_num,na.rm=T),
            end_per_ef = mean(end_per_ef,na.rm=T),
            end_ch_hrs_housework = mean(end_ch_hrs_housework,na.rm=T),
            end_ch_hrs_work = mean(end_ch_hrs_work,na.rm=T),
            end_ch_eng59 = mean(end_ch_eng59 ,na.rm=T),
            end_ch_eng1017 = mean(end_ch_eng1017, na.rm=T),
            end_cg_home_eng_per = mean(end_cg_home_eng_per, na.rm=T),
            end_cg_school_eng_per = mean(end_cg_school_eng_per, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("end"),names_to = "academic",values_to = "value") %>% 
  pivot_wider(names_from = age_group,values_from = value) %>% 
  rename("1017"="1","59"="0")

academic_gender<-pc_outcome %>% 
  group_by(male) %>% 
  summarise(end_sdq_total_diff_per = mean(end_sdq_total_diff_per ,na.rm=T),
            end_SEL_per = mean(end_SEL_per,na.rm=T),
            end_per_lit = mean(end_per_lit,na.rm=T),
            end_per_num = mean(end_per_num,na.rm=T),
            end_per_ef = mean(end_per_ef,na.rm=T),
            end_ch_hrs_housework = mean(end_ch_hrs_housework,na.rm=T),
            end_ch_hrs_work = mean(end_ch_hrs_work,na.rm=T),
            end_ch_eng59 = mean(end_ch_eng59 ,na.rm=T),
            end_ch_eng1017 = mean(end_ch_eng1017, na.rm=T),
            end_cg_home_eng_per = mean(end_cg_home_eng_per, na.rm=T),
            end_cg_school_eng_per = mean(end_cg_school_eng_per, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("end"),names_to = "academic",values_to = "value") %>% 
  pivot_wider(names_from = male,values_from = value) %>% 
  rename("male"="1","female"="0")

academic_sister<-pc_outcome %>% 
  group_by(Has_older_sister) %>% 
  summarise(end_sdq_total_diff_per = mean(end_sdq_total_diff_per ,na.rm=T),
            end_SEL_per = mean(end_SEL_per,na.rm=T),
            end_per_lit = mean(end_per_lit,na.rm=T),
            end_per_num = mean(end_per_num,na.rm=T),
            end_per_ef = mean(end_per_ef,na.rm=T),
            end_ch_hrs_housework = mean(end_ch_hrs_housework,na.rm=T),
            end_ch_hrs_work = mean(end_ch_hrs_work,na.rm=T),
            end_ch_eng59 = mean(end_ch_eng59 ,na.rm=T),
            end_ch_eng1017 = mean(end_ch_eng1017, na.rm=T),
            end_cg_home_eng_per = mean(end_cg_home_eng_per, na.rm=T),
            end_cg_school_eng_per = mean(end_cg_school_eng_per, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("end"),names_to = "academic",values_to = "value") %>% 
  pivot_wider(names_from = Has_older_sister,values_from = value) %>% 
  rename("yes_older_sis"="1","no_older_sis"="0")

# Join all three academic tables

academic<-academic_age_group %>% 
  inner_join(academic_gender,by=c('academic')) %>% 
  inner_join(academic_sister,by=c('academic'))





############################ Propensity Score Matching ########################################

# Load match package
library("MatchIt")
library("cobalt")
library("survey")
library("optmatch")

# Create test data and filter out NAs
df<-pc_outcome %>% 
  filter(!is.na(Has_older_sister),!is.na(ch_age),!is.na(mid_ppi_score),!is.na(end_hh_fies),!is.na(end_cg_home_eng))

# Use nearest neighbor matching to determine the propensities of covariates on the likelihood of having an older sister
psm_o<-matchit(Has_older_sister ~
                 ch_age+
                 mid_ppi_score+
                 end_hh_fies+
                 end_cg_home_eng,
               data = df,
               distance="glm",
               method="optimal",
               ratio=2)

# Summarize the data
summary(psm_o)

# Check the balance of the treatment and new control group
love.plot(bal.tab(psm_o), 
          stat = c("m","v"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

# Convert PSM data to be a version that can be regressed
psm_o_df<-match.data(psm_o)

# Regression
mod <- lm(end_per_lit ~ 
            Has_older_sister+
            ch_age+
            mid_ppi_score+
            end_hh_fies+
            end_cg_home_eng,
          data = psm_o_df,
           weights=weights)

summary(mod)

################################### Plot_summs regression chart ##################################
plot_summs(ols_models_cov,
           model.names = c("Literacy","Numeracy","EF","SEL"),
           coefs = c("Endline Food Insecurity PC1"="e_fs_pc1",
                     "Endline Food Insecurity PC2"="e_fs_pc2",
                     "Midline Education Outcome"="m_edu"))


############################## T-Test ###############################

# Extract the coefficient estimates and variance-covariance matrix
model <-ef_test
b1 <- "m_fs_pc2"
b2<- "e_fs_pc2"

coef_estimates <- coef(model)
vcov_matrix <- vcov(model)

# Calculate the t statistic and p-value for the difference between the two coefficients
t_stat <- (coef_estimates[b1] - coef_estimates[b2]) / sqrt(vcov_matrix[b1, b1] + vcov_matrix[b2, b2] - 2*vcov_matrix[b1, b2])
p_value <- 2 * pt(-abs(t_stat), df = model$df.residual)

# Print the results
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

