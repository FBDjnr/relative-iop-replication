#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Updates ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# v3: 2025-10-24
## Updated Stratum_ID to paste0(State, Sector, Stratum)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("custom_functions.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load NSS64 Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Directory of survey data
data_dir <- file.path("..", "data", "raw", "nss_hce23_24")

## State Codes and Names
state_codes <- readr::read_csv(file.path(data_dir, "State Codes and Names.csv")) %>% 
  dplyr::mutate(Code = str_pad(Code, width = 2, pad = "0"))

# item_codes_for_consumption_and_expenditure
item_codes <- readr::read_csv(file.path(data_dir, "item_codes_for_consumption_and_expenditure.csv")) %>% 
  dplyr::mutate(item_code = str_pad(item_code, width = 3, pad = "0"))

#...............................................................................
## Details of the household members ####

### Demographic Data #### 
nss_demo_df <- haven::read_dta(file.path(data_dir, "LEVEL - 02 (Section 3).dta"))

#...............................................................................
## Household Characteristics ####
nss_hh_chars_df <- haven::read_dta(file.path(data_dir, "LEVEL - 03.dta"))

#...............................................................................
## Consumption of Food Items ####

### Consumption of unprocessed food ####
# cereals, pulses, sugar and salt during the last 30 days
# milk & milk products, vegetables, fruits, egg, fish & meat, edible oil, spices, beverages during the last 7 days
# nss_cons_ufood_df <- haven::read_dta(file.path(data_dir, "LEVEL - 05 ( Sec 5 & 6).dta"))

### Consumption of processed food ####
# nss_cons_pfood_df <- haven::read_dta(file.path(data_dir, "LEVEL - 06 (Section 7).dta"))

#...............................................................................
## Consumables & Services ####

### Consumption of energy (fuel, light) during the last 30 days ####
# nss_cons_energy_df <- haven::read_dta(file.path(data_dir, "LEVEL - 08 (Section 8.1).dta"))

###  Expenditure on education, medical (institutional) goods and services during the last 365 days and expenditure on miscellaneous goods and services including, rents and taxes during the last 30 days ####
# Expenditure on toilet articles and other household consumables during the last 30 days
# Expenditure on education and medical (hospitalisation) during the last 365 days and on medical (non-hospitalisation) during the last 30 days
# Expenditure on conveyance, consumer services (excluding conveyance), entertainment, rent and taxes during the last 30 days
# nss_exp_emm_df <- haven::read_dta(file.path(data_dir, "LEVEL - 09 (Section 9 & 10 & 11).dta")) 

### Consumption of pan, tobacco and intoxicants during the last 7 days ####
# nss_cons_pti_df <- haven::read_dta(file.path(data_dir, "LEVEL - 10 (Section 12).dta")) 

#...............................................................................
## Durable Goods ####

### Expenditure for clothing, footwear and bedding during the last 365 days ####
# nss_exp_cfb_df <- haven::read_dta(file.path(data_dir, "LEVEL - 12 (Section 13).dta"))

### Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use during the last 365 days 
# nss_cons_pc_df <- haven::read_dta(file.path(data_dir, "Level - 13 (Section 14).dta"))

#...............................................................................

### Consumption and Expenses Sub Totals ####
nss_sub_totals_df <- haven::read_dta(file.path(data_dir, "LEVEL - 14 (Section  A1,B1 & C1).dta"))
  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Clean NSS-HCE23-24 Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Demographic Data #### 
# it is enough to set the stratum variable to the ‘ultimate’ stratifying variable 
# and the cluster variable to the ‘primary’ level of clustering

nss_demo_df_clean <- nss_demo_df %>% 
  # Select on household heads: Relation to HH head is self (1)
  dplyr::filter(Relation_to_Head == 1) %>% 
  dplyr::mutate(HH_ID = paste0(FSU_Serial_No, Sample_SU_No, Second_Stage_Stratum_No, Sample_Household_No),
                Stratum_ID = paste0(State, Sector, Stratum)
                ) %>%
  # Select necessary variables
  dplyr::select(HH_ID, FSU_Serial_No, Sector, State, District, 
                Stratum_ID, Stratum, Sub_stratum, Sample_Household_No, 
                Age, Education_Level, Gender, Marital_Status) %>% 
  dplyr::rename_with(~paste0("HHH_", .x, recycle0 = TRUE), 
                     c("Age", "Education_Level", "Gender", "Marital_Status")) 

#...............................................................................
## Household Characteristics ####

nss_hh_chars_df_clean <- nss_hh_chars_df %>% 
  # convert land possessed into from acres to hectares (1 acre = 0.4047 hectares)
  # because NSS 2007-2008 uses hectares
  dplyr::mutate(Total_Area_Land_Owned_Hectares = as.numeric(Total_Area_Land_Owned_Acres) * 0.4047) %>%
  dplyr::mutate(HH_ID = paste0(FSU_Serial_No, Sample_SU_No, Second_Stage_Stratum_No, Sample_Household_No),
                Stratum_ID = paste0(State, Sector, Stratum),
                Land_possessed_code = dplyr::case_when(
                  Total_Area_Land_Owned_Acres < 0.005 ~ "01",
                  Total_Area_Land_Owned_Acres <= 0.01 ~ "02",
                  Total_Area_Land_Owned_Acres <= 0.20 ~ "03",
                  Total_Area_Land_Owned_Acres <= 0.40 ~ "04",
                  Total_Area_Land_Owned_Acres <= 1.00 ~ "05",
                  Total_Area_Land_Owned_Acres <= 2.00 ~ "06", 
                  Total_Area_Land_Owned_Acres <= 3.00 ~ "07",
                  Total_Area_Land_Owned_Acres <= 4.00 ~ "08",
                  Total_Area_Land_Owned_Acres <= 6.00 ~ "10",
                  Total_Area_Land_Owned_Acres <= 8.00 ~ "11",
                  Total_Area_Land_Owned_Acres > 8.00 ~ "12",
                  is.na(Total_Area_Land_Owned_Acres) ~ NA,
                  .default = NA)
                ) %>%
  # Select necessary variables
  dplyr::select(HH_ID, FSU_Serial_No, Sector, State, District,
                Stratum_ID, Stratum, Sub_stratum, Sample_Household_No, 
                HH_Size_FDQ, Household_Type, Religion_of_HH_Head, Social_Group_of_HH_Head,
                Land_possessed_code, Type_of_Dwelling_Unit, Multiplier) 

#...............................................................................
### Consumption and Expenses Sub Totals ####

nss_sub_totals_df_clean <- nss_sub_totals_df %>% 
  dplyr::mutate(HH_ID = paste0(FSU_Serial_No, Sample_SU_No, Second_Stage_Stratum_No, Sample_Household_No),
                Stratum_ID = paste0(State, Sector, Stratum)
                ) %>% 
  # Classify consumption and expenditure according to NSS64
  dplyr::left_join(item_codes, by = join_by("ITEM_CODE" == "item_code")) %>% 
  # Select necessary variables
  dplyr::group_by(HH_ID, FSU_Serial_No, Sector, State, District,
                  Stratum_ID, Stratum, Sub_stratum, Sample_Household_No,
                  item_type) %>% 
  # Total consumption of food
  dplyr::summarise(Total = sum(VALUE_RS, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = item_type, values_from = Total) %>% 
  dplyr::rename(Total_Cons = consumption, Total_Exp = expenditure)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Combine Individual Data Sets ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

nss_aggregate_df <- nss_hh_chars_df_clean %>% 
  dplyr::left_join(nss_demo_df_clean) %>% 
  dplyr::left_join(nss_hh_chars_df_clean) %>% 
  dplyr::left_join(nss_sub_totals_df_clean) %>% 
  dplyr::left_join(state_codes, by = join_by(State == Code)) %>% 
  dplyr::rename(FSUno = FSU_Serial_No,
                HHS_No = Sample_Household_No,
                HH_Size = HH_Size_FDQ,
                hh_type_code = Household_Type,
                Religion = Religion_of_HH_Head,
                Social_Group = Social_Group_of_HH_Head,
                Dwelling_unit_code = Type_of_Dwelling_Unit) %>% 
  dplyr::relocate(Multiplier, .after = dplyr::last_col()) %>% 
  dplyr::relocate(starts_with("HHH"), .after = hh_type_code) %>% 
  dplyr::relocate(State_Name, .after = State)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Final Data ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# CSV
readr::write_csv(nss_aggregate_df, file.path(processed_dir, paste0("nss_hce23_24_aggregate_df", ".csv")))

# Stata File
haven::write_dta(nss_aggregate_df, file.path(processed_dir, paste0("nss_hce23_24_aggregate_df", ".dta")))
