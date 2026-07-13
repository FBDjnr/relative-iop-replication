#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Updates ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

# File version
ver <- "v3"

# Directory of survey data
data_dir <- file.path("..", "Data Sets", "nss64_consumer_expenditure")

# Directory to save results
outputs_dir <- file.path("..", "Outputs")

# Survey name
survey_name <- "nss64"

# Read data file
nss64_aggregate_df <- readr::read_csv(
  file.path(data_dir, paste0(survey_name, "_aggregate_df_", ver, ".csv"))
)

# Select variables of interest and convert them to appropriate data types
nss_df <- nss64_aggregate_df %>% 
  dplyr::mutate(Religion = dplyr::case_match(Religion,
                                            1 ~ "Hinduism",
                                            2 ~ "Islam",
                                            3 ~ "Christianity",
                                            4:9 ~ "Others"),
                HHH_Sex = dplyr::case_match(HHH_Sex,
                                           1 ~ "Male",
                                           2 ~ "Female"),
                Sector = dplyr::case_match(Sector,
                                          1 ~ "Rural", 
                                          2 ~ "Urban"),
                hh_type_code = dplyr::case_match(hh_type_code,
                                                 14 ~ "self-employment in agriculture (rural)",
                                                 11 ~ "self-employment in non-agriculture (rural)", 
                                                 12 ~ "agricultural labour (rural)",
                                                 13 ~ "non-agricultural labour (rural)",
                                                 19 ~ "unemployed (rural)", 
                                                 21 ~ "self-employment (urban)",
                                                 22 ~ "regular wage/salary earning (urban)",
                                                 23 ~ "casual labour (urban)",
                                                 29 ~ "unemployed (urban)"),
                Social_Group = dplyr::case_match(Social_Group,
                                                 1 ~ "scheduled tribe",
                                                 2 ~ "scheduled caste",
                                                 3 ~ "other backward class",
                                                 9 ~ "others"),
                Land_possessed_code = ifelse(Land_possessed_code == "XX", NA, Land_possessed_code),
                Dwelling_unit_code = dplyr::case_match(Dwelling_unit_code,
                                                       1 ~ "owned",
                                                       2 ~ "hired",
                                                       # 3 ~ "no dwelling unit",
                                                       c(3, 9) ~ "others")
  ) %>% 
  dplyr::mutate(across(c("HH_ID", "FSUno", "State", "State_Region", "Stratum_ID", 
                         "HHS_No"), 
                       as.character),
                across(c("Sector", "hh_type_code", "HHH_Sex", "HHH_Marital_Status",
                         "HHH_Education", "Religion", "Social_Group", 
                         "Land_possessed_code", "Dwelling_unit_code", 
                         "Type_of_dwelling", "Type_of_structure"), 
                       haven::as_factor),
                across(c("HH_Size", "HHH_Age", "MPCE", "CCBF", "Total_Cons", 
                         "EEMM", "EPC", "Total_Exp", "mlt", "Multiplier"), 
                       as.numeric)
                ) 


# Save clean data to csv file
readr::write_csv(nss_df,
                 file.path(data_dir, paste0(survey_name, "_aggregate_df_clean_", ver, ".csv"))
)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Apply Computation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("Application of Gini and IOP.R", echo = getOption("verbose"), print.eval = TRUE)
