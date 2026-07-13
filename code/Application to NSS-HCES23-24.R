#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Updates ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# v3: 2025-10-24
## Sorted out the stratum variable
# v2: 2025-07-21
## Recoded the following to make it consistent with NSS64:
## hh_type_code, Social_Group, Land_possessed_code, Dwelling_unit_code

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("custom_functions.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Directory of survey data
data_dir <- file.path("..", "data", "processed")

# Directory to save results
outputs_dir <- file.path("..", "output", "tables")

# Survey name
survey_name <- "nss_hce23_24"

# Read data file
nss_hce23_24_aggregate_df <- readr::read_csv(
  file.path(data_dir, paste0(survey_name, "_aggregate_df", ".csv"))
  )

# Select variables of interest and convert them to appropriate data types
nss_df <- nss_hce23_24_aggregate_df %>% 
  dplyr::mutate(hh_type_code = as.numeric(paste0(Sector, hh_type_code))) %>% 
  dplyr::mutate(Religion = dplyr::recode_values(Religion,
                                             1 ~ "Hinduism",
                                             2 ~ "Islam",
                                             3 ~ "Christianity",
                                             4:9 ~ "Others"),
                HHH_Gender = dplyr::recode_values(HHH_Gender,
                                            1 ~ "Male",
                                            2 ~ "Female",
                                            3 ~ "Transgender"),
                Sector = dplyr::recode_values(Sector,
                                           1 ~ "Rural", 
                                           2 ~ "Urban"),
                hh_type_code = dplyr::recode_values(hh_type_code,
                                           11 ~ "self-employment in agriculture (rural)",
                                           12 ~ "self-employment in non-agriculture (rural)", 
                                           # 13 ~ "regular wage/salary earning in agriculture", 
                                           # 14 ~ "regular wage/salary earning in non-agriculture", 
                                           # 15 ~ "casual labour in agriculture", 
                                           # 16 ~ "casual labour in non-agriculture", 
                                           c(13, 15) ~ "agricultural labour (rural)",
                                           c(14, 16) ~ "non-agricultural labour (rural)",
                                           19 ~ "unemployed (rural)", 
                                           21 ~ "self-employment (urban)",
                                           22 ~ "regular wage/salary earning (urban)",
                                           23 ~ "casual labour (urban)",
                                           29 ~ "unemployed (urban)"),
                Social_Group = dplyr::recode_values(Social_Group,
                                                 1 ~ "scheduled tribe",
                                                 2 ~ "scheduled caste",
                                                 3 ~ "other backward class",
                                                 9 ~ "others",
                                                 ),
                Dwelling_unit_code = dplyr::recode_values(Dwelling_unit_code,
                                                       1 ~ "owned",
                                                       2 ~ "hired",
                                                       3 ~ "others")
  ) %>% 
  dplyr::mutate(across(c("HH_ID", "FSUno", "State", #"State_Region", 
                         "Stratum_ID", "HHS_No"), 
                       as.character),
                across(c("Sector", "hh_type_code", "HHH_Gender", "HHH_Marital_Status",
                         "HHH_Education_Level", "Religion", "Social_Group", 
                         "Land_possessed_code", "Dwelling_unit_code"#, 
                         #"Type_of_dwelling", "Type_of_structure"
                         ), 
                       haven::as_factor),
                across(c("HH_Size", "HHH_Age", "Total_Cons", "Total_Exp", "Multiplier"), 
                       as.numeric)
  ) 


# Save clean data to csv file
readr::write_csv(nss_df,
                 file.path(data_dir, paste0(survey_name, "_aggregate_df_clean", ".csv"))
                 )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Apply Computation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("Application of Gini and IOP.R", echo = getOption("verbose"), print.eval = TRUE)
