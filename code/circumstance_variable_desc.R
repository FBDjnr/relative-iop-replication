#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("custom_functions.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Directory to save results
outputs_dir <- file.path("..", "output", "tables")

# Survey name
survey_names <- c("nss_hce23_24", "nss64")

# Load data
circum_summary <- sapply(survey_names, 
                         function(x) {
                           df <- readr::read_csv(file.path(outputs_dir, paste0(x, "_circum_summary.csv")))
                           df$survey_name <- x
                           return(df)
                           },
                         simplify = FALSE
                         )  %>% 
  purrr::list_rbind() %>% 
  dplyr::relocate(survey_name)
  
# Convert data into wide format
# Replace circumstance variable codes with labels
circum_summary_wide <- circum_summary %>% 
  dplyr::mutate(category_code = if_else(variable == "Land_possessed_code", as.numeric(category), NA)) %>% 
  dplyr::mutate(category = dplyr::case_match(category,
                                             "01" ~ "less than 0.005",
                                             "02" ~ "0.005 - 0.01",
                                             "03" ~ "0.02 - 0.20",
                                             "04" ~ "0.21 - 0.40",
                                             "05" ~ "0.41 - 1.00",
                                             "06" ~ "1.01 - 2.00", 
                                             "07" ~ "2.01 - 3.00",
                                             "08" ~ "3.01 - 4.00",
                                             "10" ~ "4.01 - 6.00",
                                             "11" ~ "6.01 - 8.00",
                                             "12" ~ "greater than 8.00",
                                             .default = category)
                ) %>% 
  dplyr::mutate(category_code = dplyr::case_match(category,
                                             "Hinduism" ~ 1,
                                             "Islam" ~ 2,
                                             "Christianity" ~ 3,
                                             "Others" ~ 4,
                                             
                                             "Male" ~ 1,
                                             "Female" ~ 2,
                                             
                                             "self-employment in agriculture (rural)" ~ 11,
                                             "self-employment in non-agriculture (rural)" ~ 12, 
                                             "agricultural labour (rural)" ~ 13,
                                             "non-agricultural labour (rural)" ~ 14,
                                             "unemployed (rural)" ~ 19, 
                                             "self-employment (urban)" ~ 21,
                                             "regular wage/salary earning (urban)" ~22,
                                             "casual labour (urban)" ~ 23,
                                             "unemployed (urban)" ~ 29,
                                             
                                             "scheduled tribe" ~ 1,
                                             "scheduled caste" ~ 2,
                                             "other backward class" ~ 3,
                                             "others" ~ 9,
                                             
                                             "owned" ~ 1,
                                             "hired" ~ 2,
                                             "others" ~ 9,
                                             .default = category_code
                                             )
  ) %>% 
  # dplyr::mutate(category_code = ifelse(variable == "Land_possessed_code", as.numeric(category), category_code)) %>% 
  tidyr::pivot_wider(names_from = "survey_name", values_from = c("n", "percentage")) %>% 
  dplyr::arrange(variable, category_code) %>% 
  dplyr::rename(circumstance = variable) %>% 
  dplyr::mutate(circumstance = stringr::str_replace_all(circumstance, "hh_type_code", "Household type code"),
                circumstance = stringr::str_replace_all(circumstance, "_", " "),
                circumstance = ifelse(grepl("^Land.+code", circumstance), paste(circumstance, "(hectares)"), circumstance)
                )

# Find total number of observations
total_obs <- circum_summary_wide %>% 
  dplyr::group_by(circumstance) %>% 
  dplyr::summarise(total_nss23 = sum(n_nss_hce23_24),
                   total_nss64 = sum(n_nss64)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-circumstance) %>% 
  dplyr::distinct()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Results ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Save results to latex file
circum_summary_wide %>% 
  dplyr::select(category, percentage_nss64, percentage_nss_hce23_24) %>% 
  dplyr::mutate(across(starts_with("percent"), \(x) round(x, digits = 2)))  %>% 
  tibble::add_row(category = "Total Number of Observations", 
                  percentage_nss64 = total_obs$total_nss64,
                  percentage_nss_hce23_24 = total_obs$total_nss23) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("Circumstance", "NSS 2007 (%)", "NSS 2023 (%)"),
               # linesep = ifelse(c(.$circumstance[-1], NA) == "", "", "\\addlinespace")
  ) %>% #c("", "", "\\addlinespace")) %>% 
  kableExtra::pack_rows(index = table(fct_inorder(circum_summary_wide$circumstance))) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(3)) %>% 
  # kableExtra::add_header_above(header = c(" " = 2, "Rel IOP" = 1, "Standard Error" = 2, "Structure Effect (%)" = 3),
  #                              bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, "tab_circumstance_variable_desc.tex"),
                         keep_tex = TRUE,
                         self_contained = TRUE)


# Save results to csv file
readr::write_csv(circum_summary_wide,
                 file.path(outputs_dir, "circumstance_variable_desc.csv"))

