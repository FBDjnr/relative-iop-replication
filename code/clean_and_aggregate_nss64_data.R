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
data_dir <- file.path("..", "data", "raw", "nss64")

# Directory to save the derived (aggregated) data
processed_dir <- file.path("..", "data", "processed")

## State Codes and Names
state_codes <- readr::read_csv(file.path(data_dir, "State Codes and Names.csv")) %>% 
  mutate(Code = str_pad(Code, width = 2, pad = "0"))

## Household Characteristics ####
nss_hh_chars_df <- haven::read_dta(file.path(data_dir, "Household Characteristics.dta"))

## Demographic Data #### 
nss_demo_df <- haven::read_dta(file.path(data_dir, "Demographic and other particulars of household members.dta"))

## Consumption of clothing, bedding and footwear during the last 365 days ####
nss_cons_cbf_df <- haven::read_dta(file.path(data_dir, "Consumption of clothing, bedding and footwear during the last 365 days.dta"))

## Consumption of food, pan, tobacco, intoxicants and fuel during  the last 30 days ####
nss_cons_fptif_df <- haven::read_dta(file.path(data_dir, "Consumption of food, pan, tobacco , intoxicants and fuel during  the last 30 days.dta"))

## Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use during the last 365 ####
nss_exp_pc_df <- haven::read_dta(file.path(data_dir, "Expenditure for purchase and construction.dta"))

## Expenditure on education, medical (institutional) goods and services during the last 365 days and expenditure on miscellaneous goods and services including, rents and taxes during the last 30 days ####
nss_exp_emm_df <- haven::read_dta(file.path(data_dir, "Expenditure on education, medical goods and services.dta"))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Clean NSS64 Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Household Characteristics ####
# it is enough to set the stratum variable to the ‘ultimate’ stratifying variable 
# and the cluster variable to the ‘primary’ level of clustering


nss_hh_chars_df_clean <- nss_hh_chars_df %>% 
  # Select necessary variables
  dplyr::select(HH_ID, FSUno, Sector, State, State_Region, District,
                Stratum, Sub_Stratum, HHS_No, 
                HH_Size, hh_type_code, Religion, Social_Group, 
                Land_possessed_code, Dwelling_unit_code, Type_of_dwelling,
                Type_of_structure, MPCE_Value, 
                nss, nsc, mlt, Multiplier) %>% 
  # Create a unique ID for each stratum
  dplyr::mutate(Stratum_ID = paste0(State, Sector, Stratum)) %>%
  dplyr::relocate(Stratum_ID, .before = Stratum)

#...............................................................................
## Demographic Data #### 

nss_demo_df_clean <- nss_demo_df %>% 
  # Select on household heads: Relation to HH head is self (1)
  dplyr::filter(Relation_tohead == 1) %>% 
  # Select necessary variables
  dplyr::select(HH_ID, FSUno, Sector, State, State_Region, district,
                Stratum, Sub_Stratum, HHS_No, 
                Sex, Age, Marital_Status, Education) %>%
  dplyr::rename(District = district) %>% 
  dplyr::rename_with(~paste0("HHH_", .x, recycle0 = TRUE), 
                     c("Sex", "Age", "Marital_Status", "Education")) 

#...............................................................................
## Consumption of clothing, bedding and footwear during the last 365 days ####

nss_cons_cbf_df_clean <- nss_cons_cbf_df %>% 
  # Filter to obtain item subtotals (to avoid double counting)
  dplyr::filter(readr::parse_number(Item_Code) %% 10 == 9) %>% 
  # Select necessary variables
  dplyr::group_by(HH_ID, FSUno, Sector, State, State_Region, District,
                Stratum, Sub_Stratum, HHS_No) %>% 
  # Total consumption of cbf
  dplyr::summarise(CCBF = sum(Value_in_Rs, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#...............................................................................
## Consumption of food, pan, tobacco, intoxicants and fuel during  the last 30 days ####

nss_cons_fptif_df_clean <- nss_cons_fptif_df %>% 
  # Filter to obtain item subtotals (to avoid double counting)
  dplyr::filter(readr::parse_number(Item_Code) %% 10 == 9) %>% 
  # Select necessary variables
  dplyr::group_by(HH_ID, FSUno, Sector, State, State_Region, District,
                Stratum, Sub_Stratum, HHS_No) %>% 
  # Total consumption of fptif
  dplyr::summarise(CFPTIF = sum(Value_in_Rs, na.rm = TRUE)) %>% 
  dplyr::ungroup()
  
#...............................................................................
## Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use during the last 365 ####

nss_exp_pc_df_clean <- nss_exp_pc_df %>% 
  # Filter to obtain item subtotals (to avoid double counting)
  dplyr::filter(readr::parse_number(Item_Code) %% 10 == 9) %>% 
  # Select necessary variables
  dplyr::group_by(HH_ID, FSUno, Sector, State, State_Region, District,
                  Stratum, Sub_Stratum, HHS_No) %>% 
  # Total expenses on purchases and construction
  dplyr::summarise(EPC = sum(Total_expenditure, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#...............................................................................
## Expenditure on education, medical (institutional) goods and services during the last 365 days and expenditure on miscellaneous goods and services including, rents and taxes during the last 30 days ####

nss_exp_emm_df_clean <- nss_exp_emm_df %>%
  # Filter to obtain item subtotals (to avoid double counting)
  dplyr::filter(readr::parse_number(Item_Code) %% 10 == 9) %>% 
  # Select necessary variables
  dplyr::group_by(HH_ID, FSUno, Sector, State, State_Region, District,
                  Stratum, Sub_Stratum, HHS_No) %>% 
  # Total expenses on education and medical goods, and services
  dplyr::summarise(EEMM = sum(Value, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Combine Individual Data Sets ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

nss_aggregate_df <- nss_hh_chars_df_clean %>% 
  dplyr::left_join(nss_demo_df_clean) %>% 
  dplyr::left_join(nss_cons_cbf_df_clean) %>% 
  dplyr::left_join(nss_cons_fptif_df_clean) %>% 
  dplyr::left_join(nss_exp_pc_df_clean) %>% 
  dplyr::left_join(nss_exp_emm_df_clean) %>% 
  dplyr::left_join(state_codes, by = join_by(State == Code))

nss_aggregate_df <- nss_aggregate_df %>% 
  dplyr::rename(MPCE = MPCE_Value) %>% 
  dplyr::mutate(Total_Cons = CCBF + CFPTIF,
                Total_Exp = EPC + EEMM) %>% 
  dplyr::relocate(State_Name, .after = State) %>% 
  dplyr::relocate(MPCE, CCBF, CFPTIF, Total_Cons, EEMM, EPC, Total_Exp, 
                  .before = nss) %>% 
  dplyr::relocate(starts_with("HHH"), .after = hh_type_code) %>% 
  dplyr::relocate(State_Name, .after = State)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Final Data ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# CSV
readr::write_csv(nss_aggregate_df, file.path(processed_dir, paste0("nss64_aggregate_df", ".csv")))

# Stata File
haven::write_dta(nss_aggregate_df, file.path(processed_dir, paste0("nss64_aggregate_df", ".dta")))
